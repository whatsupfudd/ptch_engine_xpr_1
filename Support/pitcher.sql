-- -----------------------------------------------------------------------------
-- Required extensions
-- -----------------------------------------------------------------------------

create extension if not exists "uuid-ossp";

-- -----------------------------------------------------------------------------
-- Schema
-- -----------------------------------------------------------------------------

create schema if not exists prod;

-- -----------------------------------------------------------------------------
-- Shared asset table
--
-- This matches the shape used by the AI server for persisted binary assets.
-- Pitcher reuses it for audio files, generated images, segment mp4 files,
-- and final rendered mp4 files.
-- -----------------------------------------------------------------------------

create table if not exists asset (
  uid bigint generated always as identity primary key,
  eid uuid not null default uuid_generate_v4(),
  created_at timestamptz not null default now(),
  status int not null default 1,         -- 1 active, 2 disabled, 3 retired
  name text,
  description text,
  contentType varchar(255),
  size bigint not null,
  version int not null default 1,
  notes text,

  constraint asset_eid_version_unq unique (eid, version)
);

create index if not exists asset_eid_idx on asset (eid);
create index if not exists asset_created_at_idx on asset (created_at desc);

-- -----------------------------------------------------------------------------
-- Narration root
--
-- One row per narration script imported into prod.
-- -----------------------------------------------------------------------------

create table if not exists prod.narration (
  uid bigint generated always as identity primary key,
  eid uuid not null default uuid_generate_v4(),
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  slug text not null,
  title text not null,
  language text not null,
  speaker text,
  source_asset_fk bigint references asset(uid) on delete set null,
  notes text,

  constraint pitcher_narration_slug_unq unique (slug),
  constraint pitcher_narration_eid_unq unique (eid)
);

create index if not exists pitcher_narration_language_idx on prod.narration (language);

create index if not exists pitcher_narration_updated_at_idx on prod.narration (updated_at desc);

-- -----------------------------------------------------------------------------
-- Dialogue blocks
--
-- One row per [dialogue] block.
-- -----------------------------------------------------------------------------

create table if not exists prod.dialogue (
  uid bigint generated always as identity primary key,
  narration_fk bigint not null references prod.narration(uid) on delete cas
  ord int not null,
  emotion text not null,
  notes text,

  constraint pitcher_dialogue_ord_ck check (ord >= 1),
  constraint pitcher_dialogue_narration_ord_unq unique (narration_fk, ord)
);

create index if not exists pitcher_dialogue_narration_idx on prod.dialogue (narration_fk, ord);

-- -----------------------------------------------------------------------------
-- Spoken sentences
--
-- One row per spoken sentence/line inside a dialogue block.
-- -----------------------------------------------------------------------------

create table if not exists prod.dialogue_sentence (
  uid bigint generated always as identity primary key,
  dialogue_fk bigint not null references prod.dialogue(uid) on delete cascade,
  ord int not null,
  body text not null,

  constraint pitcher_dialogue_sentence_ord_ck check (ord >= 1),
  constraint pitcher_dialogue_sentence_body_ck check (length(trim(body)) > 0),
  constraint pitcher_dialogue_sentence_dialogue_ord_unq unique (dialogue_fk, ord)
);

create index if not exists pitcher_dialogue_sentence_dialogue_idx on prod.dialogue_sentence (dialogue_fk, ord);

-- -----------------------------------------------------------------------------
-- Visual prompts
--
-- One row per [visuals: ...] or [visuals(i): ...] entry.
-- sentence_ord is null for a general dialogue-level visual;
-- otherwise it points to the sentence ordinal within the same dialogue.
-- -----------------------------------------------------------------------------

create table if not exists prod.dialogue_visual (
  uid bigint generated always as identity primary key,
  dialogue_fk bigint not null references prod.dialogue(uid) on delete cascade,
  ord int not null,
  sentence_ord int,
  body text not null,

  constraint pitcher_dialogue_visual_ord_ck check (ord >= 1),
  constraint pitcher_dialogue_visual_sentence_ord_ck check (sentence_ord is null or sentence_ord >= 1),\
  constraint pitcher_dialogue_visual_body_ck check (length(trim(body)) > 0),
  constraint pitcher_dialogue_visual_dialogue_ord_unq unique (dialogue_fk, ord)
);

create index if not exists pitcher_dialogue_visual_dialogue_idx on prod.dialogue_visual (dialogue_fk, ord);
create index if not exists pitcher_dialogue_visual_sentence_idx on prod.dialogue_visual (dialogue_fk, sentence_ord);

-- -----------------------------------------------------------------------------
-- Render job
--
-- One durable orchestration record per narration rendering run.
-- state stores serialized stage/task state for resumability.
-- final_asset_fk points to the final mp4 when complete.
-- -----------------------------------------------------------------------------

create table if not exists prod.render_job (
  uid bigint generated always as identity primary key,
  eid uuid not null default uuid_generate_v4(),
  narration_fk bigint not null references prod.narration(uid) on delete cascade,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  status text not null,
  state jsonb not null default '{}'::jsonb,
  final_asset_fk bigint references asset(uid) on delete set null,
  notes text,

  constraint pitcher_render_job_eid_unq unique (eid)
);

create index if not exists pitcher_render_job_narration_idx on prod.render_job (narration_fk, created_at desc);
create index if not exists pitcher_render_job_status_idx on prod.render_job (status);
create index if not exists pitcher_render_job_final_asset_idx on prod.render_job (final_asset_fk);
create index if not exists pitcher_render_job_state_gin_idx on prod.render_job using gin (state);

-- -----------------------------------------------------------------------------
-- Per-stage artifact tracking
--
-- Records reusable outputs for audio, image, segment and final stages.
-- source_sig is the deterministic content signature used to decide reuse.
-- -----------------------------------------------------------------------------

create table if not exists prod.render_artifact (
  uid bigint generated always as identity primary key,
  render_job_fk bigint not null references prod.render_job(uid) on delete cascade,

  kind text not null,                     -- audio, image, segment, final
  dialogue_fk bigint references prod.dialogue(uid) on delete cascade,
  visual_ord int,
  source_sig text not null,
  status text not null,                   -- pending, running, done, failed, skipped
  asset_fk bigint references asset(uid) on delete set null,
  asset_eid uuid,
  request_eid uuid,
  notes text,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),

  constraint pitcher_render_artifact_kind_ck check (kind in ('audio', 'image', 'segment', 'final')),
  constraint pitcher_render_artifact_status_ck check (status in ('pending', 'queued', 'running', 'done', 'failed', 'skipped')),
  constraint pitcher_render_artifact_scope_ck check (
      (kind = 'final'   and dialogue_fk is null and visual_ord is null) or
      (kind = 'segment' and dialogue_fk is not null and visual_ord is null) or
      (kind = 'audio'   and dialogue_fk is not null and visual_ord is null) or
      (kind = 'image'   and dialogue_fk is not null and visual_ord is not null)
    )
);

create index if not exists pitcher_render_artifact_job_idx on prod.render_artifact (render_job_fk, kind, updated_at desc);
create index if not exists pitcher_render_artifact_dialogue_idx on prod.render_artifact (dialogue_fk, kind);
create index if not exists pitcher_render_artifact_source_sig_idx on prod.render_artifact (source_sig);
create index if not exists pitcher_render_artifact_asset_idx on prod.render_artifact (asset_fk);
create index if not exists pitcher_render_artifact_asset_eid_idx on prod.render_artifact (asset_eid);
create index if not exists pitcher_render_artifact_request_eid_idx on prod.render_artifact (request_eid);
create unique index if not exists pitcher_render_artifact_reuse_idx on prod.render_artifact (render_job_fk, kind, dialogue_fk, visual_ord, source_sig);

-- -----------------------------------------------------------------------------
-- Optional trigger helper for updated_at maintenance
-- -----------------------------------------------------------------------------

create or replace function prod.touch_updated_at()
returns trigger
language plpgsql
as $$
begin
  new.updated_at := now();
  return new;
end;
$$;

drop trigger if exists pitcher_narration_touch_updated_at_trg on prod.narration;

create trigger pitcher_narration_touch_updated_at_trg
before update on prod.narration
for each row
execute function prod.touch_updated_at();

drop trigger if exists pitcher_render_job_touch_updated_at_trg on prod.render_job;

create trigger pitcher_render_job_touch_updated_at_trg
before update on prod.render_job
for each row
execute function prod.touch_updated_at();

drop trigger if exists pitcher_render_artifact_touch_updated_at_trg on prod.render_artifact;

create trigger pitcher_render_artifact_touch_updated_at_trg
before update on prod.render_artifact
for each row
execute function prod.touch_updated_at();


create table prod.render_graph (
  uid bigint generated always as identity primary key,
  render_job_fk bigint not null references prod.render_job(uid) on delete cascade,
  schema_ver int not null,
  status text not null default 'open',
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  unique (render_job_fk)
);

create table prod.render_node (
  uid bigint generated always as identity primary key,
  graph_fk bigint not null references prod.render_graph(uid) on delete cascade,
  key text not null,
  stage text not null,
  exec text not null,
  ord int not null,
  dialogue_fk bigint null references prod.dialogue(uid) on delete cascade,
  visual_ord int null,
  artifact_kind text null,
  source_sig text not null,
  requirements jsonb not null,
  payload jsonb not null,
  status text not null default 'pending',
  max_attempts int not null,
  attempt_count int not null default 0,
  lease_owner text null,
  lease_expires_at timestamptz null,
  asset_fk bigint null references asset(uid) on delete set null,
  asset_eid uuid null,
  completed_at timestamptz null,
  error_text text null,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  unique (graph_fk, key)
);

create table prod.render_edge (
  uid bigint generated always as identity primary key,
  graph_fk bigint not null references prod.render_graph(uid) on delete cascade,
  from_node_fk bigint not null references prod.render_node(uid) on delete cascade,
  to_node_fk bigint not null references prod.render_node(uid) on delete cascade,
  unique (graph_fk, from_node_fk, to_node_fk)
);
