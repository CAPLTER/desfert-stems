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

  IF EXISTS (SELECT 1 FROM urbancndep.stem_plot_notes WHERE plot_notes ~ '[[:cntrl:]]') THEN
    RAISE EXCEPTION 'Normalization failure: control chars remain in stem_plot_notes.plot_notes';
  END IF;
END;
$$;

COMMIT;

-- -----------------------------------------------------------------------------
-- Phase 3: Backfill plant-level keys in stem_comment via stems.post_date
-- -----------------------------------------------------------------------------
BEGIN;

UPDATE urbancndep.stem_comment sc
SET
  shrub_id = st.shrub_id,
  survey_date = st.post_date
FROM urbancndep.stems st
WHERE sc.stem_id = st.id
  AND (
    sc.shrub_id IS DISTINCT FROM st.shrub_id
    OR sc.survey_date IS DISTINCT FROM st.post_date
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
    WHEN sc.survey_date IS NULL THEN 'missing_post_date_after_backfill'
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
-- Phase 4b: Collapse duplicate plant-level comments where appropriate
-- Rules:
-- 1) Duplicates are rows sharing (shrub_id, survey_date, comment)
-- 2) Collapse only when shrub_id, survey_date, and comment are all non-null
-- 3) Keep the lowest id row and audit/remove the remainder
-- -----------------------------------------------------------------------------
BEGIN;

WITH ranked AS (
  SELECT
    sc.id,
    sc.stem_id,
    sc.shrub_id,
    sc.survey_date,
    sc.comment,
    ROW_NUMBER() OVER (
      PARTITION BY sc.shrub_id, sc.survey_date, sc.comment
      ORDER BY sc.id
    ) AS rn
  FROM urbancndep.stem_comment sc
  WHERE sc.shrub_id IS NOT NULL
    AND sc.survey_date IS NOT NULL
    AND sc.comment IS NOT NULL
),
dupes AS (
  SELECT *
  FROM ranked
  WHERE rn > 1
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
  'phase_4b_deduplicate',
  d.id,
  d.stem_id,
  d.shrub_id,
  d.survey_date,
  d.comment,
  'duplicate_shrub_id_survey_date_comment_removed'
FROM dupes d
ON CONFLICT DO NOTHING;

WITH ranked AS (
  SELECT
    sc.id,
    ROW_NUMBER() OVER (
      PARTITION BY sc.shrub_id, sc.survey_date, sc.comment
      ORDER BY sc.id
    ) AS rn
  FROM urbancndep.stem_comment sc
  WHERE sc.shrub_id IS NOT NULL
    AND sc.survey_date IS NOT NULL
    AND sc.comment IS NOT NULL
),
deleted AS (
  DELETE FROM urbancndep.stem_comment sc
  USING ranked r
  WHERE sc.id = r.id
    AND r.rn > 1
  RETURNING sc.id
)
INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_4b', 'duplicate_rows_removed', COUNT(*)
FROM deleted;

INSERT INTO urbancndep.comment_migration_log (migration_label, phase, metric, metric_value)
SELECT :'migration_label', 'phase_4b', 'duplicate_groups_remaining', COUNT(*)
FROM (
  SELECT
    sc.shrub_id,
    sc.survey_date,
    sc.comment
  FROM urbancndep.stem_comment sc
  WHERE sc.shrub_id IS NOT NULL
    AND sc.survey_date IS NOT NULL
    AND sc.comment IS NOT NULL
  GROUP BY sc.shrub_id, sc.survey_date, sc.comment
  HAVING COUNT(*) > 1
) q;

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
-- Phase 7: Final validation checkpoints
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
