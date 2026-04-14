## Plan: Stem Comment Plant-Level Redesign Runbook

This runbook implements a structural redesign of the `urbancndep.stem_comment` workflow so comments are managed at the plant level (`shrub_id`, `survey_date`) rather than stem level. It also corrects the migration direction and date-anchor issues discovered during prior execution.

## Scope
1. Evolve `stem_comment` in place by adding plant-level key columns:
- `shrub_id`
- `survey_date`
2. Retain legacy columns for one cycle:
- `stem_id`
- `post_measurement`
3. Backfill new keys for existing `stem_comment` rows using:
- `stem_comment.stem_id -> stems.id -> stems.shrub_id`
- `survey_date = stems.post_date`
4. Migrate note content in corrected direction:
- `shrub_measurements.notes` -> `stem_comment.comment`
5. Restrict source notes to:
- `shrub_measurements.survey_date >= DATE '2022-05-13'`
6. Fix text cleaning to avoid tokenization/splitting corruption.

## Out of Scope
1. Dropping legacy `stem_comment` columns in this run.
2. Enforcing uniqueness on `(shrub_id, survey_date)` in this run.
3. Reconstructing already-corrupted historical comments beyond safe normalization.

## Preconditions
1. ETL is paused.
2. Fresh backup/snapshot completed.
3. Run first in restored test DB.

## Phase 0: Baseline and Safety
1. Capture baseline counts:
- total `stem_comment`
- `stem_comment` rows with null `shrub_id` / `survey_date`
- `shrub_measurements` notes count for `survey_date >= 2022-05-13`
2. Capture sample rows for manual QA, including known problematic examples.
3. Create migration log and audit tables.

### SQL Checkpoints
1. Baseline queries succeed.
2. Audit tables exist and are writable.

### Rollback Criteria
1. Missing backup/snapshot.
2. Baseline or artifact creation fails.

### Acceptance Thresholds
1. 100% baseline metrics captured.
2. 0 DDL failures.

## Phase 1: Schema Evolution (In-Place)
1. Add `stem_comment.shrub_id` and `stem_comment.survey_date` if absent.
2. Add index on `(shrub_id, survey_date)`.
3. Add FK from `stem_comment.shrub_id` to `shrubs.id` as nullable.
4. Keep `stem_id` and `post_measurement` columns (deprecated).

### SQL Checkpoints
1. New columns and index visible in catalog.
2. FK validates.

### Rollback Criteria
1. Any DDL failure.

### Acceptance Thresholds
1. 0 schema errors.

## Phase 2: Safe Text Normalization
1. Normalize text with non-splitting logic only:
- trim
- replace CR/LF with single space
- collapse repeated whitespace
- remove control characters
- empty string -> NULL
2. Apply to:
- `stem_comment.comment`
- `shrub_measurements.notes`

### SQL Checkpoints
1. No control characters remain in normalized columns.
2. Idempotence check passes on re-run.

### Rollback Criteria
1. Unexpected mass change (threshold breach) without explanation.

### Acceptance Thresholds
1. 0 control-char artifacts.
2. 0 tokenization artifacts introduced by migration logic.

## Phase 3: Backfill Plant Keys in stem_comment
1. Populate `stem_comment.shrub_id` from `stems.shrub_id` where possible.
2. Populate `stem_comment.survey_date` from `stems.post_date` where possible.
3. Audit unresolved rows (missing stem mapping, missing post_date, etc.).

### SQL Checkpoints
1. Backfilled row counts are logged.
2. Unresolved rows are captured in audit.

### Rollback Criteria
1. Accounting mismatch between eligible rows and updated+audited rows.

### Acceptance Thresholds
1. 100% of unresolved cases audited.

## Phase 4: Corrected Content Merge (Primary Change)
1. Source set is `shrub_measurements` rows where:
- `survey_date >= DATE '2022-05-13'`
- normalized `notes` is not NULL
- `shrub_id` is not NULL
2. Match target rows in `stem_comment` by `(shrub_id, survey_date)`.
3. If target exists:
- append using `existing || '; ' || incoming`
- append to all matching target rows (decision)
4. If no target exists:
- insert one new `stem_comment` row with `shrub_id`, `survey_date`, `comment`
- leave `stem_id` and `post_measurement` NULL
5. Audit source rows that cannot be migrated.

### SQL Checkpoints
1. Accounting identity:
- `eligible_source = appended_source + inserted_source + audited_source`
2. No source rows outside date filter were processed.

### Rollback Criteria
1. Accounting identity failure.
2. Incorrect date-filter inclusion.

### Acceptance Thresholds
1. 100% source accounting closure.
2. 0 migrations for `survey_date < 2022-05-13`.

## Phase 5: Post-Migration Validation
1. Validate comment integrity on sampled rows.
2. Specifically verify punctuation-bearing strings (example pattern like `Normal. Tall.`) remain semantically intact.
3. Verify new columns are populated on migrated/inserted records.
4. Verify legacy writers still function during transition (if still used).

### SQL Checkpoints
1. Spot-check queries return expected transformed rows.
2. Migration report metrics generated.

### Rollback Criteria
1. Critical semantic corruption found.

### Acceptance Thresholds
1. Manual QA pass on representative samples.
2. No blocking regressions in current ETL.

## Transitional Policy (One Cycle)
1. `stem_id` and `post_measurement` remain, but are deprecated.
2. New writes should prefer `(shrub_id, survey_date, comment)`.
3. A future hardening release can:
- enforce `NOT NULL` on `shrub_id`, `survey_date`
- decide on uniqueness policy
- drop legacy columns

## Deliverables
1. Executable SQL migration script.
2. Migration summary report:
- rows backfilled
- rows appended
- rows inserted
- rows audited by reason

## Relevant Files
- `urbancndep_comment_migration.sql`
- `migration_runbook.md`
- `populate_database.qmd`
- `helper_annotate_new_missing.R`
- `kobo_workflow.R`
- `helper_read_data.R`

## Key Decisions Embedded
1. Migration direction is `shrub_measurements.notes` -> `stem_comment.comment`.
2. Date anchor is `stems.post_date`.
3. Source date filter is `survey_date >= 2022-05-13`.
4. Multi-target updates append to all matching `stem_comment` rows.
5. Missing mappings are audited and skipped.
6. Delimiter is `; `.
