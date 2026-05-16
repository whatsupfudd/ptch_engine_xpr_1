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
  eid uuid not null unique,
  title text,
  language text not null,
  notes text,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);


-- -----------------------------------------------------------------------------
-- Dialogue blocks
--
-- One row per [dialogue] block.
-- -----------------------------------------------------------------------------

create table if not exists prod.dialogue (
  uid bigint generated always as identity primary key,
  eid uuid not null unique,
  narration_fk bigint not null references prod.narration(uid) on delete cascade,
  ord int not null,
  emotion text not null,
  fingerprint text not null,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  unique (narration_fk, ord)
);

create index if not exists pitcher_dialogue_narration_fingerprint_idx on prod.dialogue (narration_fk, fingerprint);

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
  unique (dialogue_fk, ord)
);


-- -----------------------------------------------------------------------------
-- Visual prompts
--
-- One row per [visuals: ...] or [visuals(i): ...] entry.
-- sentence_ord is null for a general dialogue-level visual;
-- otherwise it points to the sentence ordinal within the same dialogue.
-- -----------------------------------------------------------------------------

create table if not exists prod.dialogue_visual (
  uid bigint generated always as identity primary key,
  eid uuid not null unique,
  dialogue_fk bigint not null references prod.dialogue(uid) on delete cascade,
  ord int not null,
  sentence_ord int,
  body text not null,
  fingerprint text not null,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),

  constraint prod_render_job_eid_unq unique (eid)
);

create index if not exists pitcher_dialogue_visual_dialogue_fingerprint_idx
  on prod.dialogue_visual (dialogue_fk, fingerprint);
  
-- -----------------------------------------------------------------------------
-- Render job
--
-- One durable orchestration record per narration rendering run.
-- final_asset_fk points to the final mp4 when complete.
-- -----------------------------------------------------------------------------

create table if not exists prod.render_job (
  uid bigint generated always as identity primary key,
  eid uuid not null default uuid_generate_v4(),
  narration_fk bigint not null references prod.narration(uid) on delete cascade,
  status text not null,
  supersedes_job_fk bigint references prod.render_job(uid) on delete set null,
  final_asset_fk bigint references asset(uid) on delete set null,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  completed_at timestamptz
);

create index if not exists pitcher_render_job_narration_idx
  on prod.render_job (narration_fk, created_at desc);


-- -----------------------------------------------------------------------------
-- Per-stage artifact tracking
--
-- Records reusable outputs for audio, image, segment and final stages.
-- -----------------------------------------------------------------------------

create table if not exists prod.render_artifact (
  uid bigint generated always as identity primary key,
  narration_fk bigint not null references prod.narration(uid) on delete cascade,

  derive_key text not null,
  lane text not null check (lane in ('generate', 'fuse', 'finalize')),
  exec text not null,
  artifact_kind text not null,

  asset_fk bigint references asset(uid) on delete set null,
  asset_eid uuid,
  status text not null check (status in ('done', 'failed')),
  request_eid uuid,
  notes text,

  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),

  unique (narration_fk, derive_key)
);

create index if not exists pitcher_render_artifact_narration_idx
  on prod.render_artifact (narration_fk, derive_key);


create table if not exists prod.render_node (
  uid bigint generated always as identity primary key,
  render_job_fk bigint not null references prod.render_job(uid) on delete cascade,

  derive_key text not null,
  lane text not null check (lane in ('generate', 'fuse', 'finalize')),
  exec text not null,
  ord int not null,

  source_kind text check (source_kind in ('narration', 'dialogue', 'visual')),
  source_eid uuid,

  params jsonb not null default '{}'::jsonb,
  artifact_kind text not null,

  status text not null check (status in ('pending', 'ready', 'leased', 'running', 'done', 'failed', 'skipped')),
  max_attempts int not null,
  attempt_count int not null default 0,

  lease_owner text,
  lease_expires_at timestamptz,

  asset_fk bigint references asset(uid) on delete set null,
  asset_eid uuid,
  completed_at timestamptz,
  error_text text,

  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),

  -- unique (render_job_fk, derive_key)
);

create unique index pitcher_render_node_job_derive_key_constraint on prod.render_node (render_job_fk, md5(derive_key));

create index if not exists pitcher_render_node_job_status_idx
  on prod.render_node (render_job_fk, status, ord);

create index if not exists pitcher_render_node_source_eid_idx
  on prod.render_node (source_eid);


-- New:
create table if not exists prod.render_input (
  uid bigint generated always as identity primary key,

  node_fk bigint not null
    references prod.render_node(uid)
    on delete cascade,

  ord int not null,

  input_kind text not null
    check (input_kind in ('source', 'node')),

  ref_kind text not null
    check (ref_kind in ('narration', 'dialogue', 'visual', 'render_node')),

  ref_eid uuid,
  ref_derive_key text,

  role text,

  constraint pitcher_render_input_ref_shape_ck
    check (
      (
        input_kind = 'source'
        and ref_kind in ('narration', 'dialogue', 'visual')
        and ref_eid is not null
        and ref_derive_key is null
      )
      or
      (
        input_kind = 'node'
        and ref_kind = 'render_node'
        and ref_eid is null
        and ref_derive_key is not null
      )
    ),

  constraint pitcher_render_input_node_ord_unq
    unique (node_fk, ord)
);

create index if not exists pitcher_render_input_node_idx
  on prod.render_input (node_fk, ord);

create index if not exists pitcher_render_input_ref_eid_idx
  on prod.render_input (ref_kind, ref_eid);

create index if not exists pitcher_render_input_ref_derive_key_idx
  on prod.render_input (ref_derive_key);


create or replace function prod.sourceinputsstillexist(p_node_fk bigint)
returns boolean
language sql
stable
as $$
  select not exists (
    select 1
    from prod.render_input ri
    where ri.node_fk = p_node_fk
      and ri.input_kind = 'source'
      and (
        (
          ri.ref_kind = 'narration'
          and not exists (
            select 1
            from prod.narration n
            where n.eid = ri.ref_eid
          )
        )
        or
        (
          ri.ref_kind = 'dialogue'
          and not exists (
            select 1
            from prod.dialogue d
            where d.eid = ri.ref_eid
          )
        )
        or
        (
          ri.ref_kind = 'visual'
          and not exists (
            select 1
            from prod.dialogue_visual v
            where v.eid = ri.ref_eid
          )
        )
        or
        (
          ri.ref_kind not in ('narration', 'dialogue', 'visual')
        )
        or
        (
          ri.ref_eid is null
        )
      )
  );
$$;



-- Old:

create table prod.render_graph (
  uid bigint generated always as identity primary key,
  render_job_fk bigint not null references prod.render_job(uid) on delete cascade,
  schema_ver int not null,
  status text not null default 'open',
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  unique (render_job_fk)
);

create table prod.render_edge (
  uid bigint generated always as identity primary key,
  graph_fk bigint not null references prod.render_graph(uid) on delete cascade,
  from_node_fk bigint not null references prod.render_node(uid) on delete cascade,
  to_node_fk bigint not null references prod.render_node(uid) on delete cascade,
  unique (graph_fk, from_node_fk, to_node_fk)
);


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

-- Prefix/postfix for visuals in a narration:
create table if not exists prod.vizcontext (
  uid bigint generated always as identity primary key
  , narration_fk bigint not null references prod.narration(uid) on delete cascade
  , kind text not null check (kind in ('prefix', 'postfix'))
  , seqnum int not null
  , content text not null
);

create index if not exists pitcher_vizcontext_narration_idx on prod.vizcontext (narration_fk);
