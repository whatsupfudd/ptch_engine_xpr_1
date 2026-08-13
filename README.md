# narravid

**Narravid is the experimental media-generation execution engine for Pitcher.**

It turns a structured narration into a durable graph of generation and
rendering work, executes that work through distributed workers, and manages
the reusable intermediate assets required to produce a final presentation
video.

Narravid is part of the [FUDD](https://github.com/whatsupfudd) ecosystem and
provides the execution machinery behind Pitcher's generated audiovisual
content.

> **Status**
>
> Narravid is under active development. The current package is version
> `0.1.0.0` and should be considered an experimental implementation rather
> than a production-ready media service.
>
> The repository currently contains working implementations of the core
> ingestion, graph, asset, AI-generation and video-rendering concepts, along
> with a number of integration points that are still being completed. See
> [Current implementation status](#current-implementation-status).

---

## Contents

- [What narravid does](#what-narravid-does)
- [Narravid in the FUDD ecosystem](#narravid-in-the-fudd-ecosystem)
- [Design goals](#design-goals)
- [The processing model](#the-processing-model)
- [Narration source format](#narration-source-format)
- [Stable source identity](#stable-source-identity)
- [Render graph](#render-graph)
- [Derived identity and artifact reuse](#derived-identity-and-artifact-reuse)
- [Render jobs and workers](#render-jobs-and-workers)
- [Generation and rendering](#generation-and-rendering)
- [Assets and storage](#assets-and-storage)
- [PostgreSQL data model](#postgresql-data-model)
- [Command-line interface](#command-line-interface)
- [Configuration](#configuration)
- [Building narravid](#building-narravid)
- [Development workflow](#development-workflow)
- [Source architecture](#source-architecture)
- [Extending narravid](#extending-narravid)
- [Current implementation status](#current-implementation-status)
- [Development direction](#development-direction)
- [License](#license)

---

# What narravid does

At its simplest, Narravid transforms this:

```text
structured narration
        +
spoken dialogue
        +
visual descriptions
```

into this:

```text
generated speech
        +
generated imagery
        +
timed audiovisual segments
        +
final presentation video
```

The important part of Narravid, however, is not simply invoking text-to-speech,
image-generation and video-rendering tools.

Narravid treats media production as a **persistent dependency graph**.

Each source element receives a stable external identity. Each generated
artifact receives an identity derived deterministically from its source inputs
and the policy that generated it. Render jobs, dependencies, worker leases,
results and failures are persisted in PostgreSQL, while generated binary
assets are kept in S3-compatible object storage.

This allows Narravid to support an iterative production model:

1. write or generate a narration;
2. produce a video;
3. change part of the narration;
4. ingest the new version;
5. construct a new render graph;
6. reuse everything whose effective inputs have not changed;
7. regenerate only the affected parts.

This model is particularly important for Pitcher's intended **iterative
presentation creation** workflow, where narration, visual direction and
presentation structure are progressively refined rather than generated once
and discarded.

---

# Narravid in the FUDD ecosystem

Narravid is the **media-production execution layer** associated with Pitcher.

The broader Pitcher concept is responsible for transforming structured
knowledge and creative direction into presentations and communication
artifacts. Narravid deals with the operational side of that transformation:

- ingesting a narration description;
- establishing stable identities for its components;
- translating the narration into executable render work;
- coordinating AI generation;
- coordinating conventional media-processing tools;
- storing intermediate and final artifacts;
- recovering work after worker interruption;
- reusing previously generated results;
- assembling a final video.

Conceptually:

```text
                 FUDD knowledge and applications
                            │
                            ▼
                    ┌───────────────┐
                    │    Pitcher    │
                    │               │
                    │ communication │
                    │  definition   │
                    └───────┬───────┘
                            │
                     narration model
                            │
                            ▼
                    ┌───────────────┐
                    │   narravid    │
                    │               │
                    │  production   │
                    │    engine     │
                    └───────┬───────┘
                            │
           ┌────────────────┼────────────────┐
           │                │                │
           ▼                ▼                ▼
      AI generation      rendering       persistent
       services           tools            assets
           │                │                │
           └────────────────┼────────────────┘
                            │
                            ▼
                   audiovisual output
```

Narravid can therefore support communication products such as:

- investor presentations;
- project briefings;
- explanatory videos;
- onboarding material;
- training material;
- product walkthroughs;
- educational content;
- generated presentations derived from structured FUDD knowledge.

Narravid is not intended to become the system of record for the business
process that produced the information. Its responsibility begins when
structured content and presentation direction need to become controlled,
reproducible media.

---

# Design goals

The present architecture is built around several principles.

## Incremental production

A small change to one visual should not require regeneration of unrelated
speech, images or video sections.

Narravid uses stable source identities and deterministic derived identities
to determine what can be reused.

## Durable execution

A long media render should not depend on the lifetime of one CLI process.

Narration state, render jobs, nodes, leases and artifacts live in PostgreSQL.
Generated binaries live in object storage.

## Distributed work

Generation and rendering tasks are separated into lanes and acquired by
workers through database leases.

Workers can therefore be specialised and scaled independently.

## Deterministic dependency tracking

A generated artifact is identified by the inputs and rendering policy that
logically determine its contents.

Changing an input should change the derived key. Keeping all relevant inputs
the same should preserve it.

## Explicit source identity

Database row identifiers are storage details, not creative identities.

Narravid therefore distinguishes internal database `uid` values from stable
external `eid` UUIDs.

## Reusable intermediate artifacts

Audio, images and segment videos are first-class artifacts rather than
temporary implementation details.

This is the foundation for inexpensive iterative regeneration.

## Separation of orchestration from execution

The producer decides **what must exist**.

Workers decide **how an executable node is performed**.

The database coordinates the two.

---

# The processing model

The current production path consists of four major phases:

```text
    source
      │
      ▼
┌─────────────┐
│   ingest    │
└──────┬──────┘
       │
       │ narration / dialogue / visual identities
       ▼
┌─────────────┐
│   produce   │
└──────┬──────┘
       │
       │ render job + dependency graph
       ▼
┌───────────────────────────────────────────────────┐
│                    workers                        │
│                                                   │
│   generate           fuse             finalize   │
│      │                 │                  │        │
│   TTS/image      section video       final video │
└──────┬─────────────────┬──────────────────┬───────┘
       │                 │                  │
       └─────────────────┼──────────────────┘
                         ▼
                  artifact storage
```

The phases have deliberately different responsibilities.

### `ingest`

Parses the narration source, validates it and persists its semantic source
structure.

### `produce`

Reads the persisted narration and creates a render job containing the nodes
required to render it.

### `work`

Runs a worker for a particular lane. Workers acquire ready nodes through
leases, execute them, persist generated assets and report success or failure.

### `publish`

Reserved for moving a completed production artifact into its publication
destination. The current command is a placeholder; publishing is not yet
implemented.

---

# Narration source format

Narravid currently uses a compact text format designed around dialogue and
visual direction.

A narration consists of one or more `[dialogue]` blocks.

For example:

```text
[dialogue]
[confident, measured]
Narravid is the production engine behind generated Pitcher media.
It turns structured narration into a durable graph of reusable work.
[visuals(1): A clean architectural diagram showing a narration entering a media production engine.]
[visuals(2): A dependency graph with generated audio, imagery and video connected by arrows.]

[dialogue]
[energetic]
Change one source element and only its dependent artifacts need to change.
The rest of the production can be reused.
[visuals: A presentation timeline where one modified segment is highlighted while the other segments remain unchanged.]
```

## Dialogue

Every block starts with:

```text
[dialogue]
```

A dialogue contains spoken sentences and may contain emotion or delivery
annotations and visual descriptions.

## Emotion annotations

An optional bracketed list immediately following the dialogue marker can
describe the desired delivery:

```text
[calm, deliberate]
```

The current persistence layer stores the emotion list as text associated with
the dialogue.

The annotations are part of the dialogue's semantic identity even though the
current TTS runner does not yet expose the full emotion structure to every
speech backend.

## Sentences

Dialogue body text is divided into sentences by the narration parser.

Current sentence termination includes conventional punctuation such as:

```text
.
!
?
:
...
```

The TTS worker reconstructs the spoken dialogue by joining its sentences.

## Visuals

A general visual for a dialogue can be written as:

```text
[visuals: A wide view of a futuristic operations centre.]
```

A visual can instead be anchored to a particular sentence:

```text
[visuals(2): A dependency graph expanding from the concept introduced in the second sentence.]
```

Sentence references start at `1`.

Narravid validates indexed visual references during ingestion and rejects a
reference outside the dialogue's sentence range.

Multiple sentence-specific visuals allow the video renderer to create a
simple shot sequence within a spoken section.

## Timing

The current source language does **not** require an explicit duration for each
dialogue.

Timing is derived primarily from the duration of generated speech. Visual
changes are then positioned within that duration using their sentence
anchors. Where necessary, sentence timing is estimated from the relative
amount of spoken text.

This is deliberately an implementation detail of the current renderer rather
than a permanent restriction on the narration model.

---

# Stable source identity

Stable identity is one of Narravid's most important architectural properties.

Three identifiers should not be confused:

```text
uid          database row identity
eid          stable external/semantic UUID
derive_key   deterministic identity of a generated result
```

## `uid`

A `uid` is the PostgreSQL primary key of a persisted row.

It is useful for joins and database operations, but it is not a durable
creative identity.

Rows can be replaced during ingestion.

## `eid`

Narrations, dialogues and visuals have UUID-based external identities.

These identities are what render nodes use to refer back to source material.

For dialogues and visuals, ingestion calculates content fingerprints and
compares them with the previous persisted version of the narration.

If the semantic source content remains equivalent, Narravid can preserve the
existing `eid` even when the row itself is replaced.

If the content changes, a fresh `eid` is allocated.

This has an important consequence:

```text
reordering unchanged dialogue
        │
        ├── database uid may change
        │
        └── source eid can remain stable
```

and:

```text
changing dialogue content
        │
        └── source eid changes
```

This prevents database implementation details from unnecessarily invalidating
generated media.

### Duplicate fingerprints

Identity lookup is handled as a multimap rather than assuming that one
fingerprint identifies exactly one row. This allows repeated equivalent
content to be consumed individually during reconstruction.

As with any content-derived matching strategy, repeated identical source
items should nevertheless be treated carefully when their distinction is
semantically important.

---

# Render graph

After ingestion, `produce` loads the persisted narration and transforms it
into a render graph.

The current graph contains four major node types:

```text
dialogue ───────► audio
                    │
                    │
visual ─────────► image
                    │
                    ▼
              section segment
                    │
                    ▼
                final video
```

For a narration containing several dialogues and visuals, the graph resembles:

```text
Dialogue A ──► TTS A ──────────────┐
                                   │
Visual A1 ──► Image A1 ────────────┤
Visual A2 ──► Image A2 ────────────┤
                                   ▼
                              Segment 1 ───┐
                                          │
Dialogue B ──► TTS B ──────────────┐      │
                                   │      │
Visual B1 ──► Image B1 ────────────┤      │
                                   ▼      │
                              Segment 2 ───┤
                                          │
Dialogue C ──► TTS C ──────────────┐      │
                                   ▼      │
                              Segment 3 ───┤
                                          ▼
                                     Final video
```

Dependencies are persisted through `prod.render_input`.

There are two kinds of dependency.

## Source inputs

A source input refers to stable narration content:

```text
narration.eid
dialogue.eid
dialogue_visual.eid
```

Workers resolve current source content through these identities rather than
copying all source text into every render node.

The lease logic also checks that referenced source identities still exist.

## Node inputs

A node input refers to another generated node through its `derive_key`.

For example, a segment depends on:

- one or more audio-node derived keys;
- zero or more image-node derived keys.

The final video depends on the derived keys of its segments.

The dependency structure is therefore independent of transient database row
identifiers.

---

# Render sections

A video segment is not necessarily equivalent to exactly one dialogue.

The current producer groups dialogues into **render sections**.

A visual-bearing dialogue establishes a visual section. Dialogues associated
with it can then contribute speech to that section.

This allows narrations containing dialogue without their own visual prompt to
remain useful without requiring artificial imagery for every individual
dialogue.

The current policy is:

```text
AttachTrailingToPreviousSection
```

If a narration ends with dialogue that has no new visual, that speech is
attached to the previous visual section.

If the narration contains no visual-bearing dialogue at all, the producer can
fall back to an audio-only section.

The graph model also contains support for an alternative policy:

```text
RenderTrailingAsAudioOnlySection
```

---

# Derived identity and artifact reuse

Source identity answers:

> Is this still the same dialogue or visual?

Derived identity answers:

> Is this still the same generation or rendering operation?

Every render node receives a deterministic `derive_key`.

Conceptually:

```text
derive_key =
    operation
  + stable source identities
  + upstream derive keys
  + rendering policy
  + rendering version
```

## Audio

An audio derived key currently incorporates values such as:

```text
tts
dialogue.eid
voice
renderVersionTag
```

If a dialogue is unchanged and the same voice/rendering version is used, its
speech artifact can be reused.

## Image

An image derived key incorporates:

```text
image
visual.eid
imageStyleTag
renderVersionTag
```

Changing unrelated dialogue should therefore not invalidate the image.

## Segment

A segment depends on its source section and upstream audio/image artifacts.

Its derived key incorporates the relevant upstream derived keys together with
segment-policy and timing information.

Changing the order or composition of a section may consequently rebuild the
segment even when its individual generated audio and image assets remain
reusable.

## Final video

The final artifact incorporates:

- narration identity;
- ordered segment identities;
- final rendering policy;
- render version;
- configured gap duration;
- configured fade duration.

Any change that materially changes the composition can therefore invalidate
the final video while retaining reusable lower-level artifacts.

---

# Artifact cache

Completed work is recorded independently of an individual render job in:

```text
prod.render_artifact
```

The effective reusable identity is:

```text
narration + derive_key
```

When a new render job is constructed, the producer can mark matching nodes as
already completed if a reusable successful artifact exists.

This gives Narravid two distinct concepts:

```text
render_job
    one attempt to produce a current narration

render_artifact
    reusable result of a deterministic render operation
```

A new job does not imply that every artifact has to be generated again.

This separation is central to the intended iterative Pitcher workflow.

---

# Render jobs and workers

## Render jobs

Each invocation of `produce` creates a durable `prod.render_job`.

A job owns a set of render nodes and records the state of one production run.

Creating a new render job supersedes previous active jobs for the same
narration while allowing their reusable artifacts to remain available.

The final successful asset is ultimately referenced by:

```text
prod.render_job.final_asset_fk
```

## Node states

Render nodes currently use these states:

```text
pending
ready
leased
running
done
failed
skipped
```

A typical successful transition is:

```text
pending
   │
   │ dependencies satisfied
   ▼
 ready
   │
   │ worker lease
   ▼
leased
   │
   │ execution starts / heartbeat
   ▼
running
   │
   │ result persisted
   ▼
 done
```

## Worker lanes

Work is divided into three lanes:

| Lane | Current purpose |
|---|---|
| `generate` | AI-generated speech and imagery |
| `fuse` | Assemble source media into video segments |
| `finalize` | Assemble segments into the final video |

A worker currently serves one lane.

This makes it possible to run different worker populations for fundamentally
different workloads.

For example, an AI-generation worker may have very different resource and
network requirements from a video-encoding worker.

## Leases

Workers do not simply select an unprocessed row and hope that no other worker
does the same thing.

Ready nodes are acquired using durable database leases.

The lease system provides:

- worker ownership;
- lease expiry;
- attempt counts;
- periodic heartbeat extension;
- retry handling;
- stale-lease recycling;
- concurrent acquisition using PostgreSQL locking.

Node acquisition uses the PostgreSQL `FOR UPDATE SKIP LOCKED` pattern, allowing
several workers to compete for ready work without all selecting the same node.

A long-running node periodically renews its lease.

If a worker disappears, an expired lease can eventually return to executable
state or fail when its maximum attempt count has been consumed.

This provides the basis for process-level fault recovery without requiring an
external message broker.

---

# Generation and rendering

The first-phase runner currently knows the following executable node types:

| `exec` | Lane | Result |
|---|---|---|
| `ai_tts` | `generate` | MP3 speech |
| `ai_image` | `generate` | generated image |
| `ffmpeg_segment` | `fuse` | MP4 section |
| `ffmpeg_concat` | `finalize` | final MP4 |
| `blender` | — | reserved; runner not yet implemented |

---

## Text-to-speech

For an `ai_tts` node the worker:

1. resolves the dialogue through its stable `eid`;
2. loads its current sentences from PostgreSQL;
3. combines them into spoken text;
4. authenticates with the configured AI service;
5. invokes the configured TTS function;
6. waits for the remote generated asset;
7. downloads the generated MP3;
8. uploads it into Narravid's object store;
9. records the local asset;
10. completes the render node.

The spoken source is deliberately resolved at execution time rather than
being copied into the render node as a second source of truth.

---

## Image generation

For an `ai_image` node the worker:

1. resolves the visual through its stable `eid`;
2. retrieves its description;
3. retrieves narration-level visual context;
4. constructs the effective image prompt;
5. invokes the configured image-generation function;
6. downloads the generated image;
7. stores it in Narravid's object store;
8. records and completes the artifact.

Narration-level visual context is represented through `prod.vizcontext`.

Current contexts support:

```text
prefix
postfix
```

This allows common artistic direction to be applied around individual visual
descriptions without repeating the entire style prompt in every visual.

---

## Segment rendering

A segment worker gathers the completed media required by one render section.

It can:

- combine audio from several dialogues;
- determine the duration of generated speech using `ffprobe`;
- load the corresponding sentence bodies;
- resolve sentence-anchored visuals;
- estimate visual-change timing;
- turn still images into timed video material;
- combine imagery and speech into an MP4 section.

If a section has no image input, the current renderer can produce an
audio-backed black video section.

---

## Final rendering

The `ffmpeg_concat` node retrieves the ordered segment videos and combines them
into the final presentation.

Current producer defaults include:

```text
gap between segments       0.5 seconds
fade duration              0.5 seconds
render version             v1
segment policy             v1
final policy               v1
```

These values are presently code-level defaults rather than a complete public
rendering profile.

---

# AI service integration

Narravid's AI support code expects a remote service capable of executing
registered generation functions.

The current client workflow uses endpoints conceptually equivalent to:

```text
POST /login
POST /invoke
GET  /invoke/response?tid=<request-eid>
GET  /asset/<asset-eid>
```

The configured AI service provides two important function identities:

```text
ttsFunctionEid
imageFunctionEid
```

The function EIDs allow Narravid to depend on executable AI capabilities
without embedding the implementation of a particular model directly into the
render graph.

Current AI configuration also includes:

```text
ttsSpeaker
imageModel
```

This boundary is useful inside FUDD because Narravid can remain primarily a
production/orchestration application while generative models are provided by
a separate AI execution environment.

---

# Assets and storage

Narravid separates **asset metadata** from **binary asset storage**.

```text
                  ┌─────────────────────┐
                  │      PostgreSQL     │
                  │                     │
                  │ asset metadata      │
                  │ render state        │
                  │ source identities   │
                  └──────────┬──────────┘
                             │
                             │ asset EID
                             ▼
                  ┌─────────────────────┐
                  │  S3-compatible      │
                  │  object storage     │
                  │                     │
                  │ MP3 / PNG / MP4     │
                  └─────────────────────┘
```

The shared `asset` table records information such as:

- asset `uid`;
- asset `eid`;
- content type;
- size;
- version;
- description and notes.

The actual file is uploaded through the S3-compatible storage layer.

The current pipeline uses this mechanism for:

- generated speech;
- generated images;
- intermediate video segments;
- final videos.

This avoids storing large binaries directly in PostgreSQL while keeping
production state transactionally connected to durable asset identities.

S3-compatible storage also makes MinIO a convenient development deployment.

---

# PostgreSQL data model

The current schema is centred around the `prod` namespace.

## Source model

```text
prod.narration
    │
    └── prod.dialogue
            │
            ├── prod.dialogue_sentence
            │
            └── prod.dialogue_visual
```

### `prod.narration`

Represents one ingested narration.

Important concepts include:

```text
uid
eid
title
language
notes
```

Name/nickname support is also referenced by the current CLI and DB statements;
see [Schema drift](#schema-drift).

### `prod.dialogue`

Stores dialogue ordering, stable identity, emotion metadata and source
fingerprint.

### `prod.dialogue_sentence`

Stores the spoken sentences belonging to a dialogue.

### `prod.dialogue_visual`

Stores visual descriptions and optional sentence anchors.

---

## Production model

```text
prod.render_job
    │
    └── prod.render_node
            │
            └── prod.render_input

prod.render_artifact
asset
```

### `prod.render_job`

Durable state for one production attempt.

### `prod.render_node`

Executable unit of the dependency graph.

It records values including:

```text
derive_key
lane
exec
ord
source_kind
source_eid
params
artifact_kind
status
attempt_count
lease_owner
lease_expires_at
asset_fk
```

### `prod.render_input`

Expresses source and node dependencies.

This table is the current graph-edge representation.

### `prod.render_artifact`

Records reusable completed artifacts independently of individual jobs.

### `asset`

Stores metadata for binary artifacts persisted in object storage.

---

## Legacy graph tables

`Support/pitcher.sql` still contains older:

```text
prod.render_graph
prod.render_edge
```

definitions.

The current implementation instead represents the operational graph through:

```text
render_job
render_node
render_input
derive_key
```

The old tables should therefore be considered legacy schema while the
experimental implementation converges on the simplified graph representation.

---

# Command-line interface

The executable is:

```text
narravid
```

General form:

```text
narravid [GLOBAL OPTIONS] COMMAND
```

Current commands are:

```text
help
version
ingest
produce
work
list
publish
```

---

## `help`

Displays command help.

```bash
narravid help
```

The normal optparse-generated help is also available:

```bash
narravid --help
```

---

## `version`

Displays application version information.

```bash
narravid version
```

---

## `ingest`

Parses and persists narration source.

General form:

```bash
narravid ingest PATH \
    (--eid EID | --name NAME) \
    --title TITLE \
    [--lang LANG] \
    [--speaker SPEAKER] \
    [--validate-only]
```

For example:

```bash
narravid ingest ./demo.narr \
    --name demo \
    --title "Narravid demonstration" \
    --lang en
```

To update a known narration by stable EID:

```bash
narravid ingest ./demo-v2.narr \
    --eid 00000000-0000-0000-0000-000000000000 \
    --title "Narravid demonstration" \
    --lang en
```

Validation can be requested with:

```bash
narravid ingest ./demo.narr \
    --name demo \
    --title "Narravid demonstration" \
    --validate-only
```

Successful ingestion reports the number of parsed:

```text
dialogues
sentences
visuals
```

### Name-based lookup

The CLI contains a convenient `--name` abstraction in addition to UUID-based
`--eid` targeting.

The current branch still has incomplete nickname persistence/schema alignment.
For reliable development work, retain the narration EID printed by ingestion
and use `--eid` once the narration exists until the name path has been brought
back into alignment.

---

## `produce`

Creates a render job and graph for an ingested narration.

```bash
narravid produce --eid <narration-eid>
```

or:

```bash
narravid produce --name <narration-name>
```

The command prints the newly created `render_job` UID.

Creating a production job does not perform the expensive generation itself;
that work belongs to workers.

---

## `work`

Starts a persistent lane worker.

General form:

```bash
narravid work \
    --owner OWNER \
    --lane LANE \
    [--has-gpu] \
    [--vram-mb MB] \
    --lease-seconds SECONDS
```

Example generation worker:

```bash
narravid work \
    --owner local-generate-1 \
    --lane generate \
    --lease-seconds 120
```

Example segment worker:

```bash
narravid work \
    --owner local-fuse-1 \
    --lane fuse \
    --lease-seconds 180
```

Example finalization worker:

```bash
narravid work \
    --owner local-finalize-1 \
    --lane finalize \
    --lease-seconds 180
```

Each worker continuously polls for suitable ready nodes.

`--has-gpu` and `--vram-mb` are already represented in the CLI, but the
current simplified leasing path does not yet use them for capability
selection.

---

## `list`

Provides basic inspection of persisted production state.

Without a sub-filter it lists narrations:

```bash
narravid list
```

A specific narration can be selected by EID or name.

Dialogue information:

```bash
narravid list --eid <narration-eid> dialogues
```

Render nodes:

```bash
narravid list --eid <narration-eid> rnode
```

Nodes can be filtered by lane and status:

```bash
narravid list --eid <narration-eid> \
    rnode --lane generate --status ready
```

A render-job UID can also be used when inspecting render nodes.

The current output is developer-oriented and is intended primarily for
inspection and diagnostics.

---

## `publish`

The command surface already reserves:

```bash
narravid publish --eid <narration-eid>
```

but the current implementation is a stub.

A formal publication/distribution path is still to be connected to completed
render artifacts.

---

# Configuration

A configuration file can be selected explicitly with:

```bash
narravid --config ./narravid.yaml ...
```

Using an explicit path is currently recommended for development.

The configuration model covers three important external systems:

```text
PostgreSQL
S3-compatible object storage
FUDD AI service
```

A representative configuration is:

```yaml
pgDb:
  host: localhost
  port: 5432
  user: narravid
  passwd: change-me
  dbase: narravid

s3store:
  accessKey: narravid-dev
  secretKey: change-me
  host: http://localhost:9000
  region: us-east-1
  bucket: narravid

ai:
  server: http://localhost:8000
  user: narravid
  password: change-me
  ttsFunctionEid: "00000000-0000-0000-0000-000000000000"
  ttsSpeaker: "en-US-Standard-A"
  imageFunctionEid: "00000000-0000-0000-0000-000000000000"
  imageModel: "default"
```

These values are examples only.

Do not use development/default credentials in a deployed environment and do
not commit production credentials to the repository.

## Default configuration location

The current configuration loader resolves its default beneath:

```text
~/.fudd/narravid/config.yaml
```

Some CLI help text still references an older location. Using `--config`
avoids ambiguity until those paths are consolidated.

---

# Building narravid

Narravid is a Haskell application built with Stack/Cabal metadata.

The repository currently uses:

```text
Stackage lts-22.44
system-ghc: true
```

## Build

```bash
stack build
```

## Tests

```bash
stack test
```

## Run from Stack

```bash
stack exec narravid -- --help
```

For example:

```bash
stack exec narravid -- \
    --config ./narravid.yaml \
    list
```

---

## Current MinIO development dependency

The current `stack.yaml` contains a development-time local dependency:

```yaml
extra-deps:
- ../../../Haskell/Minio/minio-hs
```

A clean checkout is therefore not yet completely self-contained.

Developers must currently either:

1. provide the expected local `minio-hs` checkout; or
2. adjust the Stack dependency to an appropriate package/source revision.

This should eventually be replaced by a repository-independent dependency
definition.

---

# Runtime dependencies

A complete development environment currently requires approximately:

### PostgreSQL

Used for:

- narration persistence;
- source identities;
- render graph state;
- worker leases;
- artifact metadata;
- production state.

The repository contains a schema snapshot in:

```text
Support/pitcher.sql
```

The schema uses the PostgreSQL `uuid-ossp` extension.

### S3-compatible object storage

Used for generated binary assets.

MinIO is suitable for local development.

### FUDD AI generation service

Must expose configured TTS and image-generation functions and support the AI
client invocation lifecycle used by Narravid.

### FFmpeg and FFprobe

Used for media inspection, section rendering and final video assembly.

### Haskell toolchain

Compatible with the Stack project and its configured system GHC.

---

# Development workflow

The intended development cycle is:

## 1. Write a narration

Create a file such as:

```text
demo.narr
```

## 2. Validate it

```bash
narravid --config ./narravid.yaml ingest ./demo.narr \
    --name demo \
    --title "Demo" \
    --validate-only
```

## 3. Ingest it

```bash
narravid --config ./narravid.yaml ingest ./demo.narr \
    --name demo \
    --title "Demo"
```

Record the narration EID emitted by ingestion.

## 4. Inspect the source model

```bash
narravid --config ./narravid.yaml \
    list --eid <narration-eid> dialogues
```

## 5. Create a render job

```bash
narravid --config ./narravid.yaml \
    produce --eid <narration-eid>
```

Record the resulting render-job UID.

## 6. Inspect the graph

```bash
narravid --config ./narravid.yaml \
    list --eid <narration-eid> rnode
```

## 7. Run lane workers

In separate processes:

```bash
narravid --config ./narravid.yaml \
    work --owner generator-1 --lane generate --lease-seconds 120
```

```bash
narravid --config ./narravid.yaml \
    work --owner fuse-1 --lane fuse --lease-seconds 180
```

```bash
narravid --config ./narravid.yaml \
    work --owner finalize-1 --lane finalize --lease-seconds 180
```

## 8. Inspect status

For example:

```bash
narravid --config ./narravid.yaml \
    list --eid <narration-eid> rnode --status done
```

or:

```bash
narravid --config ./narravid.yaml \
    list --eid <narration-eid> rnode --status failed
```

### Important: producer advancement

The current experimental CLI creates the graph and performs its initial
`producerTick`, promoting immediately executable nodes.

`producerTick` is also responsible for:

- promoting downstream nodes whose dependencies have completed;
- recognising reusable artifacts;
- recycling per-job expired leases;
- finalising completed jobs.

The current CLI does not yet run a persistent producer/manager ticker after
`produce` exits, and workers do not currently invoke `producerTick` after
completing a node.

Consequently, fully autonomous progression from `generate` through `fuse` and
`finalize` still requires completion of the manager/ticker integration.

Do **not** use repeated `produce` calls merely as a graph tick: `produce`
creates a new render job and supersedes the previous active job.

This is one of the main remaining orchestration tasks in the current branch.

---

# Source architecture

The source tree is organised by responsibility rather than by one monolithic
rendering module.

A simplified view is:

```text
app/
└── Main.hs

src/
├── AiSup/
│   ├── Client.hs
│   └── Types.hs
│
├── Assets/
│   ├── S3Ops.hs
│   ├── Store.hs
│   └── Types.hs
│
├── Commands/
│   ├── Help.hs
│   ├── Ingest.hs
│   ├── List.hs
│   ├── Produce.hs
│   ├── Publish.hs
│   ├── Version.hs
│   └── Work.hs
│
├── DB/
│   ├── Connect.hs
│   ├── Helpers.hs
│   ├── IngestStmt.hs
│   ├── LeaseStmt.hs
│   ├── ListStmt.hs
│   ├── ProducerOps.hs
│   ├── ProducerStmt.hs
│   └── TaskStmt.hs
│
├── Options/
│   ├── Cli.hs
│   ├── ConfFile.hs
│   └── Runtime.hs
│
├── Pitcher/
│   ├── Ingest.hs
│   ├── NarrationTypes.hs
│   ├── Ingest/
│   │   └── Parser.hs
│   └── Render/
│       ├── GraphTypes.hs
│       ├── Producer.hs
│       ├── TaskRunner.hs
│       ├── WorkerLease.hs
│       ├── WorkerMain.hs
│       └── WorkTypes.hs
│
├── MainLogic.hs
└── Options.hs

Support/
└── pitcher.sql
```

---

## `Pitcher.Ingest`

Owns the semantic ingestion process:

```text
source file
    │
    ▼
Megaparsec parser
    │
    ▼
Narration AST
    │
    ▼
validation
    │
    ▼
identity reconciliation
    │
    ▼
PostgreSQL
```

This is where source fingerprints and preservation of stable dialogue/visual
EIDs are managed.

---

## `Pitcher.Render.Producer`

Owns render planning.

Its responsibilities include:

- loading the persisted narration;
- grouping render sections;
- constructing audio nodes;
- constructing image nodes;
- constructing segment nodes;
- constructing the final node;
- creating deterministic derived keys;
- persisting graph nodes and inputs;
- advancing graph state through `producerTick`.

The producer should remain focused on **planning and orchestration**, not
performing expensive media work itself.

---

## `Pitcher.Render.WorkerLease`

Encapsulates the durable worker contract:

- lease a node;
- heartbeat a lease;
- load node inputs;
- find upstream artifacts;
- complete successfully;
- complete with failure.

This isolates task execution from much of the database concurrency logic.

---

## `Pitcher.Render.WorkerMain`

Provides the persistent worker loop.

It handles:

- polling;
- idle backoff;
- jitter;
- lease acquisition;
- task execution;
- failure delays;
- stale-lease recycling.

The current design is effectively a two-tier producer/worker architecture.

A later dedicated manager tier can take ownership of graph ticking, scheduling
policy and lease recycling without requiring changes to the fundamental node
execution contract.

---

## `Pitcher.Render.TaskRunner`

Turns leased nodes into concrete work.

It dispatches according to `node.exec` and contains the first-phase
implementations of:

```text
AI TTS
AI image generation
FFmpeg segment rendering
FFmpeg final concatenation
```

This module is intentionally downstream of the graph definition: workers
execute the work the producer describes rather than independently deciding
what the production graph should contain.

---

## `DB.*`

Contains PostgreSQL statements and persistence operations implemented with
Hasql.

The division between ingest, producer, lease, task and list statements mirrors
the major runtime responsibilities.

---

## `Assets.*`

Provides S3-compatible object storage and the bridge between stored binaries
and database `asset` records.

---

## `AiSup.*`

Implements the current client contract with the FUDD AI generation service.

---

## `Options.*`

Defines:

- CLI structure;
- YAML configuration;
- effective runtime configuration.

---

# Important development invariants

Several rules are fundamental to Narravid's incremental rendering model.

## 1. Never use a database `uid` as creative identity

`uid` is a persistence implementation detail.

Use stable source `eid` values and deterministic derived keys when deciding
whether media is equivalent.

## 2. Unchanged source should retain its `eid`

Re-ingestion should not invalidate an audio or image artifact merely because a
row was deleted and recreated.

## 3. Changed semantic source should receive a new `eid`

If a dialogue or visual changes in a way that affects its meaning or rendering
input, its identity must reflect that change.

## 4. Derived keys must include every behaviour-affecting input

If changing a parameter can change the generated result, that parameter must
either:

- participate directly in the derived key; or
- be represented by a version/policy identifier that participates in it.

Otherwise Narravid may incorrectly reuse stale output.

## 5. Rendering changes require cache-version discipline

Changes to rendering semantics should normally be accompanied by an
appropriate change to values such as:

```text
renderVersionTag
imageStyleTag
segmentPolicyTag
finalPolicyTag
```

when the change should invalidate prior artifacts.

## 6. Workers must respect lease ownership

A worker should only complete a node while it still owns a valid lease.

The lease contract is what makes concurrent execution safe.

## 7. Generated binaries belong in object storage

PostgreSQL should retain identities, metadata and orchestration state rather
than becoming the primary binary media store.

## 8. The producer should remain deterministic

Given equivalent source identities and equivalent production configuration,
graph construction should produce equivalent derived identities.

This property makes reasoning about reuse possible.

---

# Extending narravid

## Adding a new executable node type

A new rendering capability generally requires work in several places.

### 1. Define the execution identity

Add or extend the relevant `NodeExec` representation in the graph model.

For example:

```text
ai_video
svg_render
threejs_capture
blender
```

### 2. Decide its lane

Determine whether the operation belongs to an existing scheduling lane or
requires a new one.

The lane represents scheduling/resource affinity, while `exec` represents the
actual operation.

### 3. Define its inputs

Specify whether each dependency is:

```text
source input
```

or:

```text
node input
```

Use stable source EIDs and upstream derive keys.

### 4. Define the derived key

Include all values capable of changing the result.

The key should not depend on incidental database row identity.

### 5. Teach the producer to emit the node

Extend graph construction so the node is created at the correct point in the
dependency structure.

### 6. Implement execution

Add the corresponding task-runner branch.

The task should:

1. resolve its inputs;
2. perform the operation;
3. persist its binary output;
4. return an `AssetRef`;
5. complete through the normal lease protocol.

### 7. Add configuration where required

Externalise backend, model or render settings rather than scattering new
constants through task code.

### 8. Test reuse and invalidation

Tests should cover both:

```text
same inputs  -> same derived identity
changed input -> changed derived identity
```

where appropriate.

---

# Extending the narration language

A change to the source language can affect more of the application than the
parser.

Review at least:

```text
Pitcher.NarrationTypes
Pitcher.Ingest.Parser
Pitcher.Ingest validation
source fingerprints
PostgreSQL persistence
render-source loading
producer derived keys
task rendering semantics
tests
```

The crucial question is:

> Does this new source field change generated media?

If yes, it must participate directly or indirectly in source/derived identity
so that cached artifacts remain correct.

---

# Current implementation status

Narravid already implements a substantial portion of the intended execution
architecture, but the current repository should still be treated as an
experimental integration branch.

## Implemented foundations

The current code includes:

- structured narration parsing;
- narration validation;
- PostgreSQL source persistence;
- stable dialogue identities;
- stable visual identities;
- content fingerprint reconciliation;
- deterministic derived keys;
- durable render jobs;
- render nodes and dependencies;
- reusable render artifacts;
- worker lanes;
- PostgreSQL node leases;
- lease heartbeat;
- lease expiry/recycling;
- retry/failure representation;
- S3-compatible binary asset storage;
- AI-service login/invocation;
- text-to-speech execution;
- image-generation execution;
- FFmpeg segment production;
- final FFmpeg concatenation;
- basic graph/source inspection through `list`.

---

## Manager/ticker integration

`producerTick` implements the operations required to advance a graph after
dependencies complete.

The current `produce` CLI command invokes it once after graph creation.

There is not yet a long-running producer/manager process wired into the CLI to
continue ticking a job while workers complete nodes.

Closing this loop is a priority for end-to-end autonomous execution.

---

## Schema drift

The repository's source code references a narration `nickname` for convenient
`--name` lookup.

The current `Support/pitcher.sql` snapshot does not define that column, and
the current narration upsert does not persist the CLI name.

The support schema and ingestion statements therefore need to be reconciled
before the name-based path should be considered reliable.

The EID-based identity model itself remains the canonical production
mechanism.

---

## FFmpeg portability

The current worker command is still configured for the development
environment in which the first-phase renderer was implemented.

In particular, current code includes Homebrew-style paths such as:

```text
/opt/homebrew/bin/ffmpeg
/opt/homebrew/bin/ffprobe
```

and Apple-oriented encoder choices.

The current render profile is also approximately:

```text
1080 × 1920
24 fps
portrait video
```

FFmpeg paths, codecs, dimensions, frame rate and rendering policy should move
into configuration before workers are expected to run portably across Linux,
macOS and heterogeneous rendering nodes.

---

## GPU capability scheduling

The worker CLI already exposes concepts such as:

```text
--has-gpu
--vram-mb
```

The current simplified lease selection is lane-based and does not yet enforce
those capabilities.

These fields provide a natural starting point for heterogeneous worker
scheduling.

---

## Blender execution

`blender` exists as an execution concept but is deliberately rejected by the
first-phase task runner.

It can later become another render backend without changing the fundamental
job/node/lease model.

---

## Publishing

The `publish` command is currently a placeholder.

The final publication model still needs to define how a completed asset moves
from the internal production store to destinations such as Pitcher-facing
delivery, web publication, presentation packages or other FUDD applications.

---

## Configuration consolidation

Several rendering policies remain hard-coded near the CLI command layer.

They should progressively move into explicit versioned render profiles so a
production can record exactly which policy generated its assets.

---

## SQL cleanup

`Support/pitcher.sql` still carries legacy graph tables alongside the current
`render_node` / `render_input` model.

Once the current model is settled, the schema should be migrated into a
single authoritative form rather than retaining both generations indefinitely.

---

## Repository-independent build

The local filesystem `minio-hs` dependency in `stack.yaml` should be replaced
by a reproducible dependency reference so a fresh clone can build without
knowledge of a developer's surrounding directory structure.

---

# Development direction

The existing implementation provides the core mechanics required for a much
larger Pitcher production system.

A practical progression from the current branch is:

## Phase 1 — close the execution loop

Complete the current first-phase engine:

- persistent producer/manager ticker;
- schema/name reconciliation;
- portable FFmpeg configuration;
- reliable final-job completion;
- final-asset retrieval;
- publication command;
- reproducible dependency configuration;
- stronger integration tests.

The objective is a narration that can move autonomously from ingestion to a
completed final artifact.

## Phase 2 — production profiles and heterogeneous workers

Move generation/rendering policy into explicit configuration:

- voice profiles;
- image-generation profiles;
- visual style contexts;
- video dimensions;
- frame rates;
- codecs;
- transitions;
- render-version policies;
- retry profiles;
- worker capabilities;
- GPU/VRAM requirements.

This turns hard-coded first-phase assumptions into declarative production
policy.

## Phase 3 — richer media graph

Extend the graph beyond still-image presentation videos:

- generated video;
- motion graphics;
- SVG/HTML rendering;
- Three.js/WebGL capture;
- Blender scenes;
- compositing;
- subtitles;
- music and sound effects;
- overlays;
- branding;
- multiple output formats.

These should remain ordinary graph nodes using the same source identity,
derived identity, artifact and lease semantics.

## Phase 4 — Pitcher interactive production

Connect Narravid more tightly to the wider Pitcher experience.

The intended workflow becomes:

```text
describe
   │
   ▼
generate
   │
   ▼
review
   │
   ▼
modify
   │
   ▼
incrementally regenerate
   │
   └───────────────► repeat
```

Because the graph already models dependencies and reusable artifacts,
interactive "vibe pitching" does not need to mean regenerating an entire
presentation after every edit.

The execution engine can instead behave increasingly like a durable reactive
media graph: changed source invalidates its affected descendants while
unaffected branches remain reusable.

## Phase 5 — FUDD communication infrastructure

At larger scale, Narravid can become a general audiovisual production service
for FUDD applications.

Approved structured knowledge can feed Pitcher definitions which are rendered
into audience-specific communication artifacts without losing the provenance,
repeatability and incremental-update properties expected from the wider FUDD
architecture.

---

# Why the architecture matters

Media-generation prototypes are easy to build when every execution is:

```text
prompt -> model -> file
```

The difficult problem begins when the generated media becomes a maintained
product.

Then the system needs to answer:

- Which source produced this audio?
- Which version of that source?
- Which model or render policy produced the image?
- What depends on this visual?
- What must be regenerated if one sentence changes?
- Can an existing image safely be reused?
- Which worker currently owns this render?
- What happens when the worker dies?
- Has another production superseded this one?
- Which final video corresponds to the current narration?
- Can the production be resumed rather than restarted?

Narravid's source identities, derived keys, persistent graph, artifact cache
and lease system are designed to provide the foundation for those answers.

That is the distinction between a media-generation script and a media
production engine.

---

# Project maturity

Narravid is currently best understood as a **working architectural
prototype**.

It has moved beyond the original proof of concept into an implementation with
clear persistent source, dependency, artifact and worker semantics, but some
interfaces still reflect active development and the surrounding Pitcher
architecture is continuing to evolve.

For developers working on the project, the most important principle is to
preserve the identity and dependency model while improving the individual
generation, rendering, scheduling and user-facing layers.

The long-term value of Narravid comes from making generated media:

```text
iterative
reusable
traceable
distributed
recoverable
composable
```

rather than merely generated.

---

# License

Narravid is distributed under the BSD 3-Clause license.

See `LICENSE` for the complete terms.