-- urbancndep_comment_migration.sql
-- Purpose: Redesign stem_comment for plant-level usage and migrate notes in corrected direction.
-- Run with:
--   psql -h localhost -U srearl -d caplter -v ON_ERROR_STOP=1 -f urbancndep_comment_migration.sql
--
-- Preconditions:
-- 1) ETL writes paused.
-- 2) Fresh backup/snapshot taken.
-- 3) Run first on restored test database.

\set ON_ERROR_STOP on
\set migration_label '2026_04_14_plant_level_comment_redesign_v1'

SET client_min_messages TO NOTICE;
SET search_path TO urbancndep, public;
SET app.migration_label TO :'migration_label';

-- -----------------------------------------------------------------------------
-- Helpers (safe normalization only; no tokenization/splitting)
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION pg_temp.normalize_note(input_text text)
RETURNS text
LANGUAGE sql
IMMUTABLE
AS $$
  SELECT NULLIF(
    btrim(
      regexp_replace(
        regexp_replace(
          regexp_replace(COALESCE(input_text, ''), E'[\r\n]+', ' ', 'g'),
          '[[:cntrl:]]',
          '',
          'g'
        ),
        '[[:space:]]+',
        ' ',
        'g'
      )
    ),
    ''
  );
$$;

-- -----------------------------------------------------------------------------
-- Phase 0: Artifacts and baseline logging
-- -----------------------------------------------------------------------------
BEGIN;

CREATE TABLE IF NOT EXISTS urbancndep.comment_migration_log (
  id bigserial PRIMARY KEY,
  migration_label text NOT NULL,
  phase text NOT NULL,
  metric text NOT NULL,
  metric_value bigint NOT NULL,
  recorded_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS urbancndep.stem_comment_redesign_audit (
  audit_id bigserial PRIMARY KEY,
  migration_label text NOT NULL,
  stage text NOT NULL,
  stem_comment_id integer,
  stem_id integer,
  shrub_id integer,
  survey_date date,
  source_shrub_measurement_id integer,
  source_note text,
  reason text NOT NULL,
  audited_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS urbancndep.stem_plot_notes_reject_audit (
  audit_id bigserial PRIMARY KEY,
  migration_label text NOT NULL,
  stem_plot_note_id integer,
  plot_id integer,
  survey_date date,
  plot_notes text,
  reason text NOT NULL,
  audited_at timestamptz NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX IF NOT EXISTS stem_comment_redesign_audit_uq
ON urbancndep.stem_comment_redesign_audit (
  migration_label,
  stage,
  COALESCE(stem_comment_id, -1),
  COALESCE(source_shrub_measurement_id, -1),
  reason
);

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_0', 'stem_comment_total', COUNT(*)
FROM urbancndep.stem_comment;

DO $$
DECLARE
  has_shrub_id boolean;
  v_metric bigint := 0;
BEGIN
  SELECT EXISTS (
    SELECT 1
    FROM information_schema.columns ic
    WHERE ic.table_schema = 'urbancndep'
      AND ic.table_name = 'stem_comment'
      AND ic.column_name = 'shrub_id'
  ) INTO has_shrub_id;

  IF has_shrub_id THEN
    EXECUTE 'SELECT COUNT(*) FROM urbancndep.stem_comment WHERE shrub_id IS NULL' INTO v_metric;
  END IF;

  INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
  VALUES (current_setting('app.migration_label'), 'phase_0', 'stem_comment_null_shrub_id_before', v_metric);
END;
$$;

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_0', 'shrub_measurements_source_rows_cutoff', COUNT(*)
FROM urbancndep.shrub_measurements
WHERE survey_date >= DATE '2022-05-13'
  AND pg_temp.normalize_note(notes) IS NOT NULL
  AND shrub_id IS NOT NULL;

COMMIT;

-- -----------------------------------------------------------------------------
-- Phase 1: Schema evolution in place
-- -----------------------------------------------------------------------------
BEGIN;

ALTER TABLE urbancndep.stem_comment
  ADD COLUMN IF NOT EXISTS shrub_id integer,
  ADD COLUMN IF NOT EXISTS survey_date date;

ALTER TABLE urbancndep.stems
  ADD COLUMN IF NOT EXISTS pre_note text;

CREATE INDEX IF NOT EXISTS stem_comment_shrub_survey_idx
  ON urbancndep.stem_comment (shrub_id, survey_date);

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM pg_constraint
    WHERE conname = 'stem_comment_shrub_id_fkey'
      AND conrelid = 'urbancndep.stem_comment'::regclass
  ) THEN
    ALTER TABLE urbancndep.stem_comment
      ADD CONSTRAINT stem_comment_shrub_id_fkey
      FOREIGN KEY (shrub_id)
      REFERENCES urbancndep.shrubs(id);
  END IF;
END;
$$;

COMMENT ON TABLE urbancndep.stem_comment IS
  'Plant-level comments collected as part of the stems sampling workflow. Table name is retained for suite continuity, but comment rows now describe shrubs (plants) by shrub_id and survey_date.';

COMMENT ON COLUMN urbancndep.stem_comment.stem_id IS
  'Legacy stem-level key retained for backward compatibility. No longer the primary join key for active comment workflows.';

COMMENT ON COLUMN urbancndep.stem_comment.post_measurement IS
  'Legacy stem-level pre/post marker retained for backward compatibility. Plant-level comments should be interpreted by shrub_id and survey_date.';

COMMENT ON COLUMN urbancndep.stems.pre_note IS
  'Pre/new measurement note for stem-level collection context (for example, missing pre measurement values).';

COMMIT;

-- -----------------------------------------------------------------------------
-- Phase 2: Normalize source/target text safely
-- -----------------------------------------------------------------------------
BEGIN;

UPDATE urbancndep.stem_comment
SET comment = pg_temp.normalize_note(comment)
WHERE comment IS DISTINCT FROM pg_temp.normalize_note(comment);

UPDATE urbancndep.shrub_measurements
SET notes = pg_temp.normalize_note(notes)
WHERE notes IS DISTINCT FROM pg_temp.normalize_note(notes);

UPDATE urbancndep.stems
SET post_note = pg_temp.normalize_note(post_note)
WHERE post_note IS DISTINCT FROM pg_temp.normalize_note(post_note);

UPDATE urbancndep.stems
SET pre_note = pg_temp.normalize_note(pre_note)
WHERE pre_note IS DISTINCT FROM pg_temp.normalize_note(pre_note);

UPDATE urbancndep.stem_plot_notes
SET plot_notes = pg_temp.normalize_note(plot_notes)
WHERE plot_notes IS DISTINCT FROM pg_temp.normalize_note(plot_notes);

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM urbancndep.stem_comment WHERE comment ~ '[[:cntrl:]]') THEN
    RAISE EXCEPTION 'Normalization failure: control chars remain in stem_comment.comment';
  END IF;

  IF EXISTS (SELECT 1 FROM urbancndep.shrub_measurements WHERE notes ~ '[[:cntrl:]]') THEN
    RAISE EXCEPTION 'Normalization failure: control chars remain in shrub_measurements.notes';
  END IF;

  IF EXISTS (SELECT 1 FROM urbancndep.stems WHERE post_note ~ '[[:cntrl:]]') THEN
    RAISE EXCEPTION 'Normalization failure: control chars remain in stems.post_note';
  END IF;

  IF EXISTS (SELECT 1 FROM urbancndep.stems WHERE pre_note ~ '[[:cntrl:]]') THEN
    RAISE EXCEPTION 'Normalization failure: control chars remain in stems.pre_note';
  END IF;

  IF EXISTS (SELECT 1 FROM urbancndep.stem_plot_notes WHERE plot_notes ~ '[[:cntrl:]]') THEN
    RAISE EXCEPTION 'Normalization failure: control chars remain in stem_plot_notes.plot_notes';
  END IF;
END;
$$;

COMMIT;

-- -----------------------------------------------------------------------------
-- Phase 2b: Move pre-measurement missing-value comments to stems.pre_note
-- Rules:
-- 1) Source rows are legacy stem_comment rows with post_measurement = FALSE
-- 2) Migrate only rows where normalized comment = 'missing value'
-- 3) Append to stems.pre_note if needed, then delete migrated stem_comment rows
-- -----------------------------------------------------------------------------
BEGIN;

WITH pre_missing_source AS (
  SELECT
    urbancndep.stem_comment.id,
    urbancndep.stem_comment.stem_id,
    pg_temp.normalize_note(urbancndep.stem_comment.comment) AS normalized_comment
  FROM urbancndep.stem_comment
  WHERE urbancndep.stem_comment.post_measurement = FALSE
    AND urbancndep.stem_comment.stem_id IS NOT NULL
    AND pg_temp.normalize_note(urbancndep.stem_comment.comment) = 'missing value'
),
pre_note_target AS (
  SELECT
    urbancndep.stems.id AS stem_id,
    CASE
      WHEN pg_temp.normalize_note(urbancndep.stems.pre_note) IS NULL THEN 'missing value'
      WHEN POSITION('missing value' IN pg_temp.normalize_note(urbancndep.stems.pre_note)) > 0 THEN pg_temp.normalize_note(urbancndep.stems.pre_note)
      ELSE pg_temp.normalize_note(urbancndep.stems.pre_note) || '; missing value'
    END AS new_pre_note
  FROM urbancndep.stems
  JOIN pre_missing_source
    ON pre_missing_source.stem_id = urbancndep.stems.id
)
UPDATE urbancndep.stems
SET pre_note = pre_note_target.new_pre_note
FROM pre_note_target
WHERE urbancndep.stems.id = pre_note_target.stem_id;

INSERT INTO urbancndep.stem_comment_redesign_audit (
  migration_label,
  stage,
  stem_comment_id,
  stem_id,
  source_note,
  reason
)
SELECT
  :'migration_label',
  'phase_2b_pre_note_migration',
  urbancndep.stem_comment.id,
  urbancndep.stem_comment.stem_id,
  urbancndep.stem_comment.comment,
  'migrated_pre_measurement_missing_value_to_stems_pre_note'
FROM urbancndep.stem_comment
WHERE urbancndep.stem_comment.post_measurement = FALSE
  AND urbancndep.stem_comment.stem_id IS NOT NULL
  AND pg_temp.normalize_note(urbancndep.stem_comment.comment) = 'missing value'
ON CONFLICT DO NOTHING;

WITH deleted_source AS (
  DELETE FROM urbancndep.stem_comment
  WHERE urbancndep.stem_comment.post_measurement = FALSE
    AND urbancndep.stem_comment.stem_id IS NOT NULL
    AND pg_temp.normalize_note(urbancndep.stem_comment.comment) = 'missing value'
  RETURNING urbancndep.stem_comment.id
)
INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_2b', 'pre_missing_value_comment_rows_migrated_to_pre_note', COUNT(*)
FROM deleted_source;

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_2b', 'stems_with_pre_note_missing_value', COUNT(*)
FROM urbancndep.stems
WHERE POSITION('missing value' IN COALESCE(urbancndep.stems.pre_note, '')) > 0;

COMMIT;

-- -----------------------------------------------------------------------------
-- Phase 2c: Convert sentinel stem length values to NULL
-- Rule:
-- 1) Any stem_lengths.length_in_mm = 999 is treated as missing and set to NULL
-- -----------------------------------------------------------------------------
BEGIN;

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_2c', 'stem_lengths_999_before_nullify', COUNT(*)
FROM urbancndep.stem_lengths
WHERE urbancndep.stem_lengths.length_in_mm = 999;

WITH nullified AS (
  UPDATE urbancndep.stem_lengths
  SET length_in_mm = NULL
  WHERE urbancndep.stem_lengths.length_in_mm = 999
  RETURNING urbancndep.stem_lengths.id
)
INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_2c', 'stem_lengths_999_rows_nullified', COUNT(*)
FROM nullified;

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_2c', 'stem_lengths_999_after_nullify', COUNT(*)
FROM urbancndep.stem_lengths
WHERE urbancndep.stem_lengths.length_in_mm = 999;

COMMIT;

-- -----------------------------------------------------------------------------
-- Phase 2d: Ensure pre-note missing-value context for flagged pre-measurement stems
-- Rules:
-- 1) Target stems with at least one stem_lengths row where post_measurement = FALSE
-- 2) If pre-measurement rows have no non-NULL length and/or pre_note lacks
--    'missing value', append/set pre_note to include 'missing value'
-- -----------------------------------------------------------------------------
BEGIN;

WITH pre_measurement_summary AS (
  SELECT
    urbancndep.stems.id AS stem_id,
    COUNT(*) FILTER (
      WHERE urbancndep.stem_lengths.length_in_mm IS NOT NULL
    ) AS pre_non_null_length_count,
    CASE
      WHEN POSITION('missing value' IN LOWER(COALESCE(urbancndep.stems.pre_note, ''))) > 0 THEN 1
      ELSE 0
    END AS has_missing_value_pre_note
  FROM urbancndep.stems
  JOIN urbancndep.stem_lengths
    ON urbancndep.stem_lengths.stem_id = urbancndep.stems.id
  WHERE urbancndep.stem_lengths.post_measurement = FALSE
  GROUP BY urbancndep.stems.id, urbancndep.stems.pre_note
)
INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_2d', 'candidate_stems_without_pre_length', COUNT(*)
FROM pre_measurement_summary
WHERE pre_non_null_length_count = 0;

WITH pre_measurement_summary AS (
  SELECT
    urbancndep.stems.id AS stem_id,
    COUNT(*) FILTER (
      WHERE urbancndep.stem_lengths.length_in_mm IS NOT NULL
    ) AS pre_non_null_length_count,
    CASE
      WHEN POSITION('missing value' IN LOWER(COALESCE(urbancndep.stems.pre_note, ''))) > 0 THEN 1
      ELSE 0
    END AS has_missing_value_pre_note
  FROM urbancndep.stems
  JOIN urbancndep.stem_lengths
    ON urbancndep.stem_lengths.stem_id = urbancndep.stems.id
  WHERE urbancndep.stem_lengths.post_measurement = FALSE
  GROUP BY urbancndep.stems.id, urbancndep.stems.pre_note
)
INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_2d', 'candidate_stems_missing_pre_note_missing_value', COUNT(*)
FROM pre_measurement_summary
WHERE has_missing_value_pre_note = 0;

WITH pre_measurement_summary AS (
  SELECT
    urbancndep.stems.id AS stem_id,
    COUNT(*) FILTER (
      WHERE urbancndep.stem_lengths.length_in_mm IS NOT NULL
    ) AS pre_non_null_length_count,
    CASE
      WHEN POSITION('missing value' IN LOWER(COALESCE(urbancndep.stems.pre_note, ''))) > 0 THEN 1
      ELSE 0
    END AS has_missing_value_pre_note
  FROM urbancndep.stems
  JOIN urbancndep.stem_lengths
    ON urbancndep.stem_lengths.stem_id = urbancndep.stems.id
  WHERE urbancndep.stem_lengths.post_measurement = FALSE
  GROUP BY urbancndep.stems.id, urbancndep.stems.pre_note
), target_stems AS (
  SELECT
    pre_measurement_summary.stem_id,
    CASE
      WHEN pg_temp.normalize_note(urbancndep.stems.pre_note) IS NULL THEN 'missing value'
      WHEN POSITION('missing value' IN pg_temp.normalize_note(urbancndep.stems.pre_note)) > 0 THEN pg_temp.normalize_note(urbancndep.stems.pre_note)
      ELSE pg_temp.normalize_note(urbancndep.stems.pre_note) || '; missing value'
    END AS new_pre_note
  FROM pre_measurement_summary
  JOIN urbancndep.stems
    ON urbancndep.stems.id = pre_measurement_summary.stem_id
  WHERE pre_measurement_summary.pre_non_null_length_count = 0
     OR pre_measurement_summary.has_missing_value_pre_note = 0
), updated_stems AS (
  UPDATE urbancndep.stems
  SET pre_note = target_stems.new_pre_note
  FROM target_stems
  WHERE urbancndep.stems.id = target_stems.stem_id
    AND urbancndep.stems.pre_note IS DISTINCT FROM target_stems.new_pre_note
  RETURNING urbancndep.stems.id
)
INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_2d', 'stems_pre_note_updated_for_pre_measurement_rule', COUNT(*)
FROM updated_stems;

WITH pre_measurement_summary AS (
  SELECT
    urbancndep.stems.id AS stem_id,
    COUNT(*) FILTER (
      WHERE urbancndep.stem_lengths.length_in_mm IS NOT NULL
    ) AS pre_non_null_length_count,
    CASE
      WHEN POSITION('missing value' IN LOWER(COALESCE(urbancndep.stems.pre_note, ''))) > 0 THEN 1
      ELSE 0
    END AS has_missing_value_pre_note
  FROM urbancndep.stems
  JOIN urbancndep.stem_lengths
    ON urbancndep.stem_lengths.stem_id = urbancndep.stems.id
  WHERE urbancndep.stem_lengths.post_measurement = FALSE
  GROUP BY urbancndep.stems.id, urbancndep.stems.pre_note
)
INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_2d', 'candidate_stems_total_for_pre_measurement_rule', COUNT(*)
FROM pre_measurement_summary
WHERE pre_non_null_length_count = 0
   OR has_missing_value_pre_note = 0;

COMMIT;

-- -----------------------------------------------------------------------------
-- Phase 3: Backfill plant-level keys in stem_comment via stems.post_date
-- -----------------------------------------------------------------------------
BEGIN;

UPDATE urbancndep.stem_comment sc
SET
  shrub_id = st.shrub_id,
  survey_date = COALESCE(st.post_date, st.pre_date)
FROM urbancndep.stems st
WHERE sc.stem_id = st.id
  AND (
    sc.shrub_id IS DISTINCT FROM st.shrub_id
    OR sc.survey_date IS DISTINCT FROM COALESCE(st.post_date, st.pre_date)
  );

-- Audit unresolved legacy rows (missing mapping keys)
INSERT INTO urbancndep.stem_comment_redesign_audit (
  migration_label,
  stage,
  stem_comment_id,
  stem_id,
  shrub_id,
  survey_date,
  source_note,
  reason
)
SELECT
  :'migration_label',
  'phase_3_backfill',
  sc.id,
  sc.stem_id,
  sc.shrub_id,
  sc.survey_date,
  sc.comment,
  CASE
    WHEN sc.stem_id IS NULL THEN 'legacy_row_without_stem_id'
    WHEN sc.shrub_id IS NULL THEN 'missing_shrub_id_after_backfill'
    WHEN sc.survey_date IS NULL THEN 'missing_stem_dates_after_backfill'
    ELSE 'other_unresolved'
  END
FROM urbancndep.stem_comment sc
WHERE sc.shrub_id IS NULL OR sc.survey_date IS NULL
ON CONFLICT DO NOTHING;

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_3', 'stem_comment_null_shrub_or_date_after_backfill', COUNT(*)
FROM urbancndep.stem_comment
WHERE shrub_id IS NULL OR survey_date IS NULL;

COMMIT;

-- -----------------------------------------------------------------------------
-- Phase 4: Corrected merge direction (shrub_measurements.notes -> stem_comment.comment)
-- Rules:
-- 1) Source filter: survey_date >= 2022-05-13
-- 2) Match by (shrub_id, survey_date)
-- 3) Append to all matching stem_comment rows with '; '
-- 4) If no target exists, insert one new stem_comment row (stem_id/post_measurement NULL)
-- -----------------------------------------------------------------------------
BEGIN;

-- A. Append to all existing matches
WITH src AS (
  SELECT
    sm.id AS source_shrub_measurement_id,
    sm.shrub_id,
    sm.survey_date,
    pg_temp.normalize_note(sm.notes) AS source_note
  FROM urbancndep.shrub_measurements sm
  WHERE sm.survey_date >= DATE '2022-05-13'
    AND sm.shrub_id IS NOT NULL
    AND pg_temp.normalize_note(sm.notes) IS NOT NULL
),
matched AS (
  SELECT
    sc.id AS stem_comment_id,
    s.source_shrub_measurement_id,
    s.source_note
  FROM src s
  JOIN urbancndep.stem_comment sc
    ON sc.shrub_id = s.shrub_id
   AND sc.survey_date = s.survey_date
)
UPDATE urbancndep.stem_comment sc
SET comment = CASE
  WHEN sc.comment IS NULL THEN matched.source_note
  WHEN POSITION(matched.source_note IN sc.comment) > 0 THEN sc.comment
  ELSE sc.comment || '; ' || matched.source_note
END
FROM matched
WHERE sc.id = matched.stem_comment_id;

-- B. Insert one new row per unmatched shrub/date source
WITH src AS (
  SELECT
    sm.id AS source_shrub_measurement_id,
    sm.shrub_id,
    sm.survey_date,
    pg_temp.normalize_note(sm.notes) AS source_note
  FROM urbancndep.shrub_measurements sm
  WHERE sm.survey_date >= DATE '2022-05-13'
    AND sm.shrub_id IS NOT NULL
    AND pg_temp.normalize_note(sm.notes) IS NOT NULL
),
unmatched AS (
  SELECT s.*
  FROM src s
  LEFT JOIN urbancndep.stem_comment sc
    ON sc.shrub_id = s.shrub_id
   AND sc.survey_date = s.survey_date
  WHERE sc.id IS NULL
),
ins AS (
  INSERT INTO urbancndep.stem_comment (
    shrub_id,
    survey_date,
    comment,
    stem_id,
    post_measurement
  )
  SELECT
    u.shrub_id,
    u.survey_date,
    u.source_note,
    NULL,
    NULL
  FROM unmatched u
  RETURNING id, shrub_id, survey_date, comment
)
SELECT COUNT(*)
FROM ins;

-- C. Audit source rows that were in-range but not migratable (missing key)
INSERT INTO urbancndep.stem_comment_redesign_audit (
  migration_label,
  stage,
  shrub_id,
  survey_date,
  source_shrub_measurement_id,
  source_note,
  reason
)
SELECT
  :'migration_label',
  'phase_4_source_audit',
  sm.shrub_id,
  sm.survey_date,
  sm.id,
  sm.notes,
  CASE
    WHEN sm.shrub_id IS NULL THEN 'source_missing_shrub_id'
    WHEN sm.survey_date IS NULL THEN 'source_missing_survey_date'
    WHEN pg_temp.normalize_note(sm.notes) IS NULL THEN 'source_missing_note'
    ELSE 'source_unclassified'
  END
FROM urbancndep.shrub_measurements sm
WHERE (sm.survey_date >= DATE '2022-05-13' OR sm.survey_date IS NULL)
  AND (sm.shrub_id IS NULL OR sm.survey_date IS NULL OR pg_temp.normalize_note(sm.notes) IS NULL)
ON CONFLICT DO NOTHING;

-- D. Accounting metrics
INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_4', 'source_rows_eligible', COUNT(*)
FROM urbancndep.shrub_measurements sm
WHERE sm.survey_date >= DATE '2022-05-13'
  AND sm.shrub_id IS NOT NULL
  AND pg_temp.normalize_note(sm.notes) IS NOT NULL;

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_4', 'target_rows_with_shrub_survey_after_merge', COUNT(*)
FROM urbancndep.stem_comment sc
WHERE sc.survey_date >= DATE '2022-05-13'
  AND sc.shrub_id IS NOT NULL;

COMMIT;

-- -----------------------------------------------------------------------------
-- Phase 4b: Duplicate-key consolidation before uniqueness hardening
-- Rules:
-- 1) Detect duplicate groups by (shrub_id, survey_date)
-- 2) Keep one canonical row per key and merge comments onto keeper
-- 3) Audit and remove non-keeper rows
-- 4) Fail only if duplicates remain after consolidation
-- -----------------------------------------------------------------------------
BEGIN;

WITH duplicate_groups AS (
  SELECT
    urbancndep.stem_comment.shrub_id,
    urbancndep.stem_comment.survey_date,
    COUNT(*) AS row_count
  FROM urbancndep.stem_comment
  WHERE urbancndep.stem_comment.shrub_id IS NOT NULL
    AND urbancndep.stem_comment.survey_date IS NOT NULL
  GROUP BY urbancndep.stem_comment.shrub_id, urbancndep.stem_comment.survey_date
  HAVING COUNT(*) > 1
),
duplicate_rows AS (
  SELECT
    urbancndep.stem_comment.id,
    urbancndep.stem_comment.stem_id,
    urbancndep.stem_comment.shrub_id,
    urbancndep.stem_comment.survey_date,
    urbancndep.stem_comment.comment,
    duplicate_groups.row_count
  FROM urbancndep.stem_comment
  JOIN duplicate_groups
    ON duplicate_groups.shrub_id = urbancndep.stem_comment.shrub_id
   AND duplicate_groups.survey_date = urbancndep.stem_comment.survey_date
)
INSERT INTO urbancndep.stem_comment_redesign_audit (
  migration_label,
  stage,
  stem_comment_id,
  stem_id,
  shrub_id,
  survey_date,
  source_note,
  reason
)
SELECT
  :'migration_label',
  'phase_4b_duplicate_guardrail',
  duplicate_rows.id,
  duplicate_rows.stem_id,
  duplicate_rows.shrub_id,
  duplicate_rows.survey_date,
  duplicate_rows.comment,
  'duplicate_shrub_id_survey_date_detected'
FROM duplicate_rows
ON CONFLICT DO NOTHING;

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_4b', 'duplicate_groups_by_shrub_survey', COUNT(*)
FROM (
  SELECT
    urbancndep.stem_comment.shrub_id,
    urbancndep.stem_comment.survey_date
  FROM urbancndep.stem_comment
  WHERE urbancndep.stem_comment.shrub_id IS NOT NULL
    AND urbancndep.stem_comment.survey_date IS NOT NULL
  GROUP BY urbancndep.stem_comment.shrub_id, urbancndep.stem_comment.survey_date
  HAVING COUNT(*) > 1
) duplicate_group_count;

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_4b', 'duplicate_rows_by_shrub_survey', COUNT(*)
FROM (
  SELECT
    urbancndep.stem_comment.id
  FROM urbancndep.stem_comment
  JOIN (
    SELECT
      urbancndep.stem_comment.shrub_id,
      urbancndep.stem_comment.survey_date
    FROM urbancndep.stem_comment
    WHERE urbancndep.stem_comment.shrub_id IS NOT NULL
      AND urbancndep.stem_comment.survey_date IS NOT NULL
    GROUP BY urbancndep.stem_comment.shrub_id, urbancndep.stem_comment.survey_date
    HAVING COUNT(*) > 1
  ) duplicate_keys
    ON duplicate_keys.shrub_id = urbancndep.stem_comment.shrub_id
   AND duplicate_keys.survey_date = urbancndep.stem_comment.survey_date
) duplicate_row_count;

WITH duplicate_groups AS (
  SELECT
    urbancndep.stem_comment.shrub_id,
    urbancndep.stem_comment.survey_date
  FROM urbancndep.stem_comment
  WHERE urbancndep.stem_comment.shrub_id IS NOT NULL
    AND urbancndep.stem_comment.survey_date IS NOT NULL
  GROUP BY urbancndep.stem_comment.shrub_id, urbancndep.stem_comment.survey_date
  HAVING COUNT(*) > 1
),
ranked_rows AS (
  SELECT
    urbancndep.stem_comment.id,
    urbancndep.stem_comment.shrub_id,
    urbancndep.stem_comment.survey_date,
    ROW_NUMBER() OVER (
      PARTITION BY urbancndep.stem_comment.shrub_id, urbancndep.stem_comment.survey_date
      ORDER BY urbancndep.stem_comment.id
    ) AS row_rank
  FROM urbancndep.stem_comment
  JOIN duplicate_groups
    ON duplicate_groups.shrub_id = urbancndep.stem_comment.shrub_id
   AND duplicate_groups.survey_date = urbancndep.stem_comment.survey_date
),
keeper_rows AS (
  SELECT
    ranked_rows.id,
    ranked_rows.shrub_id,
    ranked_rows.survey_date
  FROM ranked_rows
  WHERE ranked_rows.row_rank = 1
),
merged_comments AS (
  SELECT
    keeper_rows.id AS keeper_id,
    STRING_AGG(
      DISTINCT NULLIF(BTRIM(urbancndep.stem_comment.comment), ''),
      '; ' ORDER BY NULLIF(BTRIM(urbancndep.stem_comment.comment), '')
    ) AS merged_comment
  FROM keeper_rows
  JOIN urbancndep.stem_comment
    ON urbancndep.stem_comment.shrub_id = keeper_rows.shrub_id
   AND urbancndep.stem_comment.survey_date = keeper_rows.survey_date
  GROUP BY keeper_rows.id
)
UPDATE urbancndep.stem_comment
SET comment = merged_comments.merged_comment
FROM merged_comments
WHERE urbancndep.stem_comment.id = merged_comments.keeper_id;

WITH duplicate_groups AS (
  SELECT
    urbancndep.stem_comment.shrub_id,
    urbancndep.stem_comment.survey_date
  FROM urbancndep.stem_comment
  WHERE urbancndep.stem_comment.shrub_id IS NOT NULL
    AND urbancndep.stem_comment.survey_date IS NOT NULL
  GROUP BY urbancndep.stem_comment.shrub_id, urbancndep.stem_comment.survey_date
  HAVING COUNT(*) > 1
),
ranked_rows AS (
  SELECT
    urbancndep.stem_comment.id,
    urbancndep.stem_comment.stem_id,
    urbancndep.stem_comment.shrub_id,
    urbancndep.stem_comment.survey_date,
    urbancndep.stem_comment.comment,
    ROW_NUMBER() OVER (
      PARTITION BY urbancndep.stem_comment.shrub_id, urbancndep.stem_comment.survey_date
      ORDER BY urbancndep.stem_comment.id
    ) AS row_rank
  FROM urbancndep.stem_comment
  JOIN duplicate_groups
    ON duplicate_groups.shrub_id = urbancndep.stem_comment.shrub_id
   AND duplicate_groups.survey_date = urbancndep.stem_comment.survey_date
)
INSERT INTO urbancndep.stem_comment_redesign_audit (
  migration_label,
  stage,
  stem_comment_id,
  stem_id,
  shrub_id,
  survey_date,
  source_note,
  reason
)
SELECT
  :'migration_label',
  'phase_4b_duplicate_consolidation',
  ranked_rows.id,
  ranked_rows.stem_id,
  ranked_rows.shrub_id,
  ranked_rows.survey_date,
  ranked_rows.comment,
  'duplicate_shrub_id_survey_date_removed_after_merge'
FROM ranked_rows
WHERE ranked_rows.row_rank > 1
ON CONFLICT DO NOTHING;

WITH duplicate_groups AS (
  SELECT
    urbancndep.stem_comment.shrub_id,
    urbancndep.stem_comment.survey_date
  FROM urbancndep.stem_comment
  WHERE urbancndep.stem_comment.shrub_id IS NOT NULL
    AND urbancndep.stem_comment.survey_date IS NOT NULL
  GROUP BY urbancndep.stem_comment.shrub_id, urbancndep.stem_comment.survey_date
  HAVING COUNT(*) > 1
),
ranked_rows AS (
  SELECT
    urbancndep.stem_comment.id,
    ROW_NUMBER() OVER (
      PARTITION BY urbancndep.stem_comment.shrub_id, urbancndep.stem_comment.survey_date
      ORDER BY urbancndep.stem_comment.id
    ) AS row_rank
  FROM urbancndep.stem_comment
  JOIN duplicate_groups
    ON duplicate_groups.shrub_id = urbancndep.stem_comment.shrub_id
   AND duplicate_groups.survey_date = urbancndep.stem_comment.survey_date
),
deleted_rows AS (
  DELETE FROM urbancndep.stem_comment
  USING ranked_rows
  WHERE urbancndep.stem_comment.id = ranked_rows.id
    AND ranked_rows.row_rank > 1
  RETURNING urbancndep.stem_comment.id
)
INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_4b', 'duplicate_rows_deleted_after_merge', COUNT(*)
FROM deleted_rows;

DO $$
DECLARE
  v_duplicate_groups bigint;
BEGIN
  SELECT COUNT(*)
  INTO v_duplicate_groups
  FROM (
    SELECT
      urbancndep.stem_comment.shrub_id,
      urbancndep.stem_comment.survey_date
    FROM urbancndep.stem_comment
    WHERE urbancndep.stem_comment.shrub_id IS NOT NULL
      AND urbancndep.stem_comment.survey_date IS NOT NULL
    GROUP BY urbancndep.stem_comment.shrub_id, urbancndep.stem_comment.survey_date
    HAVING COUNT(*) > 1
  ) duplicate_group_count;

  IF v_duplicate_groups > 0 THEN
    RAISE EXCEPTION
      'Phase 4b failed: % duplicate (shrub_id, survey_date) groups remain after consolidation. Resolve before enforcing uniqueness.',
      v_duplicate_groups;
  END IF;
END;
$$;

COMMIT;

-- -----------------------------------------------------------------------------
-- Phase 5: Standardize stems.post_note and route residuals
-- Rules:
-- 1) dead|dog|ground|dop|dob -> 'dead (on branch or ground)'
-- 2) missing|msising|find|found -> 'not found'
-- 3) all other non-null values move to shrub_measurements.notes
-- 4) routed residuals then nulled in stems.post_note
-- Mapping anchor to shrub_measurements uses stems.post_date
-- -----------------------------------------------------------------------------
BEGIN;

-- Canonicalize categories
UPDATE urbancndep.stems
SET post_note = CASE
  WHEN post_note ~* '(^|[^a-z])(dead|dog|ground|dop|dob)([^a-z]|$)' THEN 'dead (on branch or ground)'
  WHEN post_note ~* '(^|[^a-z])(missing|msising|find|found)([^a-z]|$)' THEN 'not found'
  ELSE post_note
END
WHERE post_note IS NOT NULL;

-- Route non-canonical residual post_note values to shrub_measurements.notes
WITH residual_source AS (
  SELECT
    st.id AS stem_id,
    st.shrub_id,
    st.post_date AS survey_date,
    st.post_note AS original_post_note,
    pg_temp.normalize_note(st.post_note) AS normalized_post_note
  FROM urbancndep.stems st
  WHERE st.post_note IS NOT NULL
    AND st.post_note NOT IN ('dead (on branch or ground)', 'not found')
),
matched AS (
  SELECT
    rs.stem_id,
    sm.id AS shrub_measurement_id,
    rs.normalized_post_note
  FROM residual_source rs
  JOIN urbancndep.shrub_measurements sm
    ON sm.shrub_id = rs.shrub_id
   AND sm.survey_date = rs.survey_date
  WHERE rs.normalized_post_note IS NOT NULL
)
UPDATE urbancndep.shrub_measurements sm
SET notes = CASE
  WHEN sm.notes IS NULL THEN m.normalized_post_note
  WHEN POSITION(m.normalized_post_note IN sm.notes) > 0 THEN sm.notes
  ELSE sm.notes || '; ' || m.normalized_post_note
END
FROM matched m
WHERE sm.id = m.shrub_measurement_id;

-- Audit residual post_note rows that could not be mapped to shrub_measurements
WITH residual_source AS (
  SELECT
    st.id AS stem_id,
    st.shrub_id,
    st.post_date AS survey_date,
    st.post_note AS original_post_note,
    pg_temp.normalize_note(st.post_note) AS normalized_post_note
  FROM urbancndep.stems st
  WHERE st.post_note IS NOT NULL
    AND st.post_note NOT IN ('dead (on branch or ground)', 'not found')
),
target_counts AS (
  SELECT shrub_id, survey_date, COUNT(*) AS target_count
  FROM urbancndep.shrub_measurements
  GROUP BY shrub_id, survey_date
),
classified AS (
  SELECT
    rs.*,
    COALESCE(tc.target_count, 0) AS target_count,
    CASE
      WHEN rs.shrub_id IS NULL THEN 'post_note_missing_shrub_id'
      WHEN rs.survey_date IS NULL THEN 'post_note_missing_post_date'
      WHEN COALESCE(tc.target_count, 0) = 0 THEN 'post_note_no_target_shrub_measurement'
      ELSE 'post_note_other_unresolved'
    END AS reason
  FROM residual_source rs
  LEFT JOIN target_counts tc
    ON tc.shrub_id = rs.shrub_id
   AND tc.survey_date = rs.survey_date
)
INSERT INTO urbancndep.stem_comment_redesign_audit (
  migration_label,
  stage,
  stem_id,
  shrub_id,
  survey_date,
  source_note,
  reason
)
SELECT
  :'migration_label',
  'phase_5_post_note_audit',
  stem_id,
  shrub_id,
  survey_date,
  original_post_note,
  reason
FROM classified
WHERE reason <> 'post_note_other_unresolved' OR target_count = 0
ON CONFLICT DO NOTHING;

-- Null out residuals after routing/audit
UPDATE urbancndep.stems
SET post_note = NULL
WHERE post_note IS NOT NULL
  AND post_note NOT IN ('dead (on branch or ground)', 'not found');

-- Checkpoint: no non-canonical values remain
DO $$
BEGIN
  IF EXISTS (
    SELECT 1
    FROM urbancndep.stems
    WHERE post_note IS NOT NULL
      AND post_note NOT IN ('dead (on branch or ground)', 'not found')
  ) THEN
    RAISE EXCEPTION 'Phase 5 failed: non-canonical stems.post_note values remain';
  END IF;
END;
$$;

COMMIT;

-- -----------------------------------------------------------------------------
-- Phase 6: Harden stem_plot_notes (survey_date and plot_notes NOT NULL)
-- -----------------------------------------------------------------------------
BEGIN;

-- Re-normalize to ensure blanks become NULL prior to hardening
UPDATE urbancndep.stem_plot_notes
SET plot_notes = pg_temp.normalize_note(plot_notes)
WHERE plot_notes IS DISTINCT FROM pg_temp.normalize_note(plot_notes);

INSERT INTO urbancndep.stem_plot_notes_reject_audit (
  migration_label,
  stem_plot_note_id,
  plot_id,
  survey_date,
  plot_notes,
  reason
)
SELECT
  :'migration_label',
  spn.id,
  spn.plot_id,
  spn.survey_date,
  spn.plot_notes,
  CASE
    WHEN spn.survey_date IS NULL THEN 'null_survey_date'
    WHEN spn.plot_notes IS NULL THEN 'null_or_blank_plot_notes'
    ELSE 'unknown_reject_reason'
  END
FROM urbancndep.stem_plot_notes spn
WHERE spn.survey_date IS NULL OR spn.plot_notes IS NULL
ON CONFLICT DO NOTHING;

DELETE FROM urbancndep.stem_plot_notes
WHERE survey_date IS NULL OR plot_notes IS NULL;

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM urbancndep.stem_plot_notes WHERE survey_date IS NULL) THEN
    RAISE EXCEPTION 'Phase 6 failed: NULL survey_date remains in stem_plot_notes';
  END IF;

  IF EXISTS (SELECT 1 FROM urbancndep.stem_plot_notes WHERE plot_notes IS NULL) THEN
    RAISE EXCEPTION 'Phase 6 failed: NULL plot_notes remains in stem_plot_notes';
  END IF;
END;
$$;

ALTER TABLE urbancndep.stem_plot_notes
  ALTER COLUMN survey_date SET NOT NULL,
  ALTER COLUMN plot_notes SET NOT NULL;

COMMIT;

-- -----------------------------------------------------------------------------
-- Phase 6b: Enforce stem_comment key integrity (NOT NULL + UNIQUE)
-- -----------------------------------------------------------------------------
BEGIN;

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_6b', 'stem_comment_null_shrub_or_date_before_hardening', COUNT(*)
FROM urbancndep.stem_comment
WHERE urbancndep.stem_comment.shrub_id IS NULL OR urbancndep.stem_comment.survey_date IS NULL;

DO $$
DECLARE
  v_null_rows bigint;
BEGIN
  SELECT COUNT(*)
  INTO v_null_rows
  FROM urbancndep.stem_comment
  WHERE urbancndep.stem_comment.shrub_id IS NULL OR urbancndep.stem_comment.survey_date IS NULL;

  IF v_null_rows > 0 THEN
    RAISE EXCEPTION
      'Phase 6b failed: % stem_comment rows have NULL shrub_id or survey_date. Resolve before NOT NULL hardening.',
      v_null_rows;
  END IF;
END;
$$;

ALTER TABLE urbancndep.stem_comment
  ALTER COLUMN shrub_id SET NOT NULL,
  ALTER COLUMN survey_date SET NOT NULL;

CREATE UNIQUE INDEX IF NOT EXISTS stem_comment_shrub_survey_uq
  ON urbancndep.stem_comment (shrub_id, survey_date);

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_6b', 'stem_comment_unique_index_present',
  CASE WHEN EXISTS (
    SELECT 1
    FROM pg_indexes
    WHERE schemaname = 'urbancndep'
      AND indexname = 'stem_comment_shrub_survey_uq'
  ) THEN 1 ELSE 0 END;

COMMIT;

-- -----------------------------------------------------------------------------
-- Phase 7: Drop explicitly unused columns and emit cleanup candidates
-- Requested hard drops:
-- 1) stems.sample_period
-- 2) stem_lengths.flag
-- -----------------------------------------------------------------------------
BEGIN;

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_7', 'stems_sample_period_non_null_before_drop', COUNT(*)
FROM urbancndep.stems
WHERE urbancndep.stems.sample_period IS NOT NULL;

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_7', 'stem_lengths_flag_non_null_before_drop', COUNT(*)
FROM urbancndep.stem_lengths
WHERE urbancndep.stem_lengths.flag IS NOT NULL;

ALTER TABLE urbancndep.stems
  DROP COLUMN IF EXISTS sample_period;

ALTER TABLE urbancndep.stem_lengths
  DROP COLUMN IF EXISTS flag;

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_7', 'stems_sample_period_exists_after_drop',
  CASE WHEN EXISTS (
    SELECT 1
    FROM information_schema.columns
    WHERE table_schema = 'urbancndep'
      AND table_name = 'stems'
      AND column_name = 'sample_period'
  ) THEN 1 ELSE 0 END;

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_7', 'stem_lengths_flag_exists_after_drop',
  CASE WHEN EXISTS (
    SELECT 1
    FROM information_schema.columns
    WHERE table_schema = 'urbancndep'
      AND table_name = 'stem_lengths'
      AND column_name = 'flag'
  ) THEN 1 ELSE 0 END;

COMMIT;

-- -----------------------------------------------------------------------------
-- Phase 8: Final validation checkpoints
-- -----------------------------------------------------------------------------
-- Ensure no control characters introduced by merge
DO $$
BEGIN
  IF EXISTS (
    SELECT 1
    FROM urbancndep.stem_comment
    WHERE comment ~ '[[:cntrl:]]'
  ) THEN
    RAISE EXCEPTION 'Validation failure: control chars found in stem_comment.comment after merge';
  END IF;
END;
$$;

-- Checkpoint outputs
SELECT 'post_redesign_null_keys' AS checkpoint,
       SUM(CASE WHEN shrub_id IS NULL THEN 1 ELSE 0 END) AS null_shrub_id,
       SUM(CASE WHEN survey_date IS NULL THEN 1 ELSE 0 END) AS null_survey_date
FROM urbancndep.stem_comment;

SELECT 'post_redesign_recent_rows' AS checkpoint,
       COUNT(*) AS n
FROM urbancndep.stem_comment
WHERE survey_date >= DATE '2022-05-13';

SELECT 'post_note_distribution' AS checkpoint,
       post_note,
       COUNT(*) AS n
FROM urbancndep.stems
GROUP BY post_note
ORDER BY post_note;

SELECT 'stem_plot_notes_null_check' AS checkpoint,
       SUM(CASE WHEN survey_date IS NULL THEN 1 ELSE 0 END) AS null_survey_date,
       SUM(CASE WHEN plot_notes IS NULL THEN 1 ELSE 0 END) AS null_plot_notes
FROM urbancndep.stem_plot_notes;

SELECT 'audit_reasons' AS checkpoint,
       reason,
       COUNT(*) AS n
FROM urbancndep.stem_comment_redesign_audit
WHERE migration_label = :'migration_label'
GROUP BY reason
ORDER BY reason;

SELECT *
FROM urbancndep.comment_migration_log
WHERE migration_label = :'migration_label'
ORDER BY id;
