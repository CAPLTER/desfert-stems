-- urbancndep_comment_migration_verify.sql
-- Purpose: Read-only smoke test for migration acceptance checks.
-- Run with:
--   psql -h localhost -U srearl -d caplter -v ON_ERROR_STOP=1 -f urbancndep_comment_migration_verify.sql

\set ON_ERROR_STOP on
SET client_min_messages TO NOTICE;
SET search_path TO urbancndep, public;

DO $$
DECLARE
  v_db text := current_database();
  v_schema_exists boolean;
  v_core_table_count integer := 0;
BEGIN
  SELECT EXISTS (
    SELECT 1
    FROM pg_namespace
    WHERE nspname = 'urbancndep'
  ) INTO v_schema_exists;

  SELECT
    (CASE WHEN to_regclass('urbancndep.stem_comment') IS NOT NULL THEN 1 ELSE 0 END) +
    (CASE WHEN to_regclass('urbancndep.stems') IS NOT NULL THEN 1 ELSE 0 END) +
    (CASE WHEN to_regclass('urbancndep.shrub_measurements') IS NOT NULL THEN 1 ELSE 0 END)
  INTO v_core_table_count;

  IF NOT v_schema_exists OR v_core_table_count = 0 THEN
    RAISE EXCEPTION
      'Verifier preflight failed in database "%": expected schema/table set not found (schema urbancndep, core tables stem_comment/stems/shrub_measurements). Re-run against target DB, e.g. psql -h localhost -U srearl -d caplter -v ON_ERROR_STOP=1 -f urbancndep_comment_migration_verify.sql',
      v_db;
  END IF;
END;
$$;

\echo '=== 1) Schema checks ==='
SELECT
  'stem_comment_has_shrub_id' AS check_name,
  CASE WHEN EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'urbancndep'
      AND table_name = 'stem_comment'
      AND column_name = 'shrub_id'
  ) THEN 'PASS' ELSE 'FAIL' END AS status;

SELECT
  'stem_comment_has_survey_date' AS check_name,
  CASE WHEN EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'urbancndep'
      AND table_name = 'stem_comment'
      AND column_name = 'survey_date'
  ) THEN 'PASS' ELSE 'FAIL' END AS status;

SELECT
  'stems_has_pre_note' AS check_name,
  CASE WHEN EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'urbancndep'
      AND table_name = 'stems'
      AND column_name = 'pre_note'
  ) THEN 'PASS' ELSE 'FAIL' END AS status;

SELECT
  'stems_sample_period_dropped' AS check_name,
  CASE WHEN EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'urbancndep'
      AND table_name = 'stems'
      AND column_name = 'sample_period'
  ) THEN 'FAIL' ELSE 'PASS' END AS status;

SELECT
  'stem_lengths_flag_dropped' AS check_name,
  CASE WHEN EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'urbancndep'
      AND table_name = 'stem_lengths'
      AND column_name = 'flag'
  ) THEN 'FAIL' ELSE 'PASS' END AS status;

SELECT
  'stem_comment_shrub_id_not_null_constraint' AS check_name,
  CASE WHEN EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'urbancndep'
      AND table_name = 'stem_comment'
      AND column_name = 'shrub_id'
      AND is_nullable = 'NO'
  ) THEN 'PASS' ELSE 'FAIL' END AS status;

SELECT
  'stem_comment_survey_date_not_null_constraint' AS check_name,
  CASE WHEN EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'urbancndep'
      AND table_name = 'stem_comment'
      AND column_name = 'survey_date'
      AND is_nullable = 'NO'
  ) THEN 'PASS' ELSE 'FAIL' END AS status;

SELECT
  'stem_comment_shrub_survey_unique_index' AS check_name,
  CASE WHEN EXISTS (
    SELECT 1
    FROM pg_indexes
    WHERE schemaname = 'urbancndep'
      AND indexname = 'stem_comment_shrub_survey_uq'
  ) THEN 'PASS' ELSE 'FAIL' END AS status;

SELECT
  'stem_plot_notes_survey_date_not_null_constraint' AS check_name,
  CASE WHEN EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'urbancndep'
      AND table_name = 'stem_plot_notes'
      AND column_name = 'survey_date'
      AND is_nullable = 'NO'
  ) THEN 'PASS' ELSE 'FAIL' END AS status;

SELECT
  'stem_plot_notes_plot_notes_not_null_constraint' AS check_name,
  CASE WHEN EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'urbancndep'
      AND table_name = 'stem_plot_notes'
      AND column_name = 'plot_notes'
      AND is_nullable = 'NO'
  ) THEN 'PASS' ELSE 'FAIL' END AS status;

\echo '=== 2) Null integrity checks ==='
DO $$
DECLARE
  v_null_survey bigint := NULL;
  v_null_plot_notes bigint := NULL;
  v_status text := 'FAIL';
BEGIN
  IF to_regclass('urbancndep.stem_plot_notes') IS NULL THEN
    RAISE NOTICE 'stem_plot_notes_null_counts: SKIP (table urbancndep.stem_plot_notes does not exist)';
  ELSE
    EXECUTE $sql$
      SELECT
        SUM(CASE WHEN survey_date IS NULL THEN 1 ELSE 0 END),
        SUM(CASE WHEN plot_notes IS NULL THEN 1 ELSE 0 END)
      FROM urbancndep.stem_plot_notes
    $sql$
    INTO v_null_survey, v_null_plot_notes;

    IF v_null_survey = 0 AND v_null_plot_notes = 0 THEN
      v_status := 'PASS';
    END IF;

    RAISE NOTICE 'stem_plot_notes_null_counts: null_survey_date=%, null_plot_notes=%, status=%',
      v_null_survey, v_null_plot_notes, v_status;
  END IF;
END;
$$;

DO $$
DECLARE
  v_rows_999 bigint := NULL;
  v_status text := 'FAIL';
BEGIN
  IF to_regclass('urbancndep.stem_lengths') IS NULL THEN
    RAISE NOTICE 'stem_lengths_sentinel_999_rows: SKIP (table urbancndep.stem_lengths does not exist)';
  ELSE
    EXECUTE $sql$
      SELECT COUNT(*)
      FROM urbancndep.stem_lengths
      WHERE length_in_mm = 999
    $sql$
    INTO v_rows_999;

    IF v_rows_999 = 0 THEN
      v_status := 'PASS';
    END IF;

    RAISE NOTICE 'stem_lengths_sentinel_999_rows: rows=%, status=%',
      v_rows_999, v_status;
  END IF;
END;
$$;

\echo '=== 3) Pre-measurement completeness rule checks ==='
DO $$
DECLARE
  v_violation_groups bigint := NULL;
  v_status text := 'FAIL';
BEGIN
  IF to_regclass('urbancndep.stems') IS NULL
     OR to_regclass('urbancndep.stem_lengths') IS NULL
     OR to_regclass('urbancndep.shrubs') IS NULL
     OR to_regclass('urbancndep.plots') IS NULL THEN
    RAISE NOTICE 'pre_measurement_completeness_rule: SKIP (required table missing)';
  ELSE
    EXECUTE $sql$
      SELECT COUNT(*)
      FROM (
        SELECT
          plots.id,
          shrubs.code,
          stems.pre_date,
          stems.direction
        FROM urbancndep.stems
        JOIN urbancndep.shrubs
          ON urbancndep.shrubs.id = urbancndep.stems.shrub_id
        JOIN urbancndep.plots
          ON urbancndep.plots.id = urbancndep.shrubs.plot_id
        JOIN urbancndep.stem_lengths
          ON urbancndep.stem_lengths.stem_id = urbancndep.stems.id
        WHERE urbancndep.stem_lengths.post_measurement = FALSE
        GROUP BY
          urbancndep.plots.id,
          urbancndep.shrubs.code,
          urbancndep.stems.pre_date,
          urbancndep.stems.direction
        HAVING COUNT(*) FILTER (
          WHERE urbancndep.stem_lengths.length_in_mm IS NOT NULL
        ) = 0
          AND MAX(
            CASE
              WHEN POSITION('missing value' IN LOWER(COALESCE(urbancndep.stems.pre_note, ''))) > 0 THEN 1
              ELSE 0
            END
          ) = 0
      ) rule_violations
    $sql$
    INTO v_violation_groups;

    IF v_violation_groups = 0 THEN
      v_status := 'PASS';
    END IF;

    RAISE NOTICE 'pre_measurement_completeness_rule: violation_groups=%, status=%',
      v_violation_groups, v_status;
  END IF;
END;
$$;

SELECT
  urbancndep.plots.id AS plot_id,
  urbancndep.shrubs.code AS shrub_code,
  urbancndep.stems.pre_date,
  urbancndep.stems.direction,
  COUNT(*) FILTER (
    WHERE urbancndep.stem_lengths.length_in_mm IS NOT NULL
  ) AS non_null_pre_length_count,
  MAX(
    CASE
      WHEN POSITION('missing value' IN LOWER(COALESCE(urbancndep.stems.pre_note, ''))) > 0 THEN 1
      ELSE 0
    END
  ) AS has_missing_value_pre_note
FROM urbancndep.stems
JOIN urbancndep.shrubs
  ON urbancndep.shrubs.id = urbancndep.stems.shrub_id
JOIN urbancndep.plots
  ON urbancndep.plots.id = urbancndep.shrubs.plot_id
JOIN urbancndep.stem_lengths
  ON urbancndep.stem_lengths.stem_id = urbancndep.stems.id
WHERE urbancndep.stem_lengths.post_measurement = FALSE
GROUP BY
  urbancndep.plots.id,
  urbancndep.shrubs.code,
  urbancndep.stems.pre_date,
  urbancndep.stems.direction
HAVING COUNT(*) FILTER (
  WHERE urbancndep.stem_lengths.length_in_mm IS NOT NULL
) = 0
  AND MAX(
    CASE
      WHEN POSITION('missing value' IN LOWER(COALESCE(urbancndep.stems.pre_note, ''))) > 0 THEN 1
      ELSE 0
    END
  ) = 0
ORDER BY
  urbancndep.plots.id,
  urbancndep.shrubs.code,
  urbancndep.stems.pre_date,
  urbancndep.stems.direction;

\echo '=== 7) Superfluous-field candidate inventory ==='
SELECT
  candidate.table_name,
  candidate.column_name,
  candidate.candidate_group,
  candidate.rationale,
  CASE
    WHEN EXISTS (
      SELECT 1
      FROM information_schema.columns ic
      WHERE ic.table_schema = 'urbancndep'
        AND ic.table_name = candidate.table_name
        AND ic.column_name = candidate.column_name
    ) THEN 'present'
    ELSE 'absent'
  END AS current_state,
  candidate.recommended_action
FROM (
  SELECT
    'stem_comment'::text AS table_name,
    'stem_id'::text AS column_name,
    'transitional_legacy'::text AS candidate_group,
    'Legacy key retained for compatibility during plant-level comment transition.'::text AS rationale,
    'defer_drop_until_post_transition_signoff'::text AS recommended_action
  UNION ALL
  SELECT
    'stem_comment',
    'post_measurement',
    'transitional_legacy',
    'Legacy pre/post marker retained for compatibility during plant-level comment transition.',
    'defer_drop_until_post_transition_signoff'
) candidate
ORDER BY candidate.candidate_group, candidate.table_name, candidate.column_name;

SELECT
  cleanup_object.schema_name,
  cleanup_object.object_name,
  cleanup_object.object_type,
  cleanup_object.rationale,
  cleanup_object.recommended_action,
  CASE
    WHEN cleanup_object.object_type = 'table' AND to_regclass(cleanup_object.schema_name || '.' || cleanup_object.object_name) IS NOT NULL THEN 'present'
    WHEN cleanup_object.object_type = 'table' THEN 'absent'
    ELSE 'n/a'
  END AS current_state
FROM (
  SELECT
    'urbancndep'::text AS schema_name,
    'stem_comment_redesign_audit'::text AS object_name,
    'table'::text AS object_type,
    'Migration audit artifact table.'::text AS rationale,
    'drop_after_post_migration_audit_signoff'::text AS recommended_action
  UNION ALL
  SELECT
    'urbancndep',
    'stem_plot_notes_reject_audit',
    'table',
    'Migration reject-audit artifact table.',
    'drop_after_post_migration_audit_signoff'
  UNION ALL
  SELECT
    'urbancndep',
    'comment_migration_log',
    'table',
    'Migration metrics artifact table.',
    'drop_or_archive_after_post_migration_reporting'
) cleanup_object
ORDER BY cleanup_object.object_type, cleanup_object.object_name;

DO $$
DECLARE
  has_shrub_id boolean;
  has_survey_date boolean;
  sql_text text;
  v_null_shrub bigint;
  v_null_survey bigint;
BEGIN
  SELECT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'urbancndep'
      AND table_name = 'stem_comment'
      AND column_name = 'shrub_id'
  ) INTO has_shrub_id;

  SELECT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'urbancndep'
      AND table_name = 'stem_comment'
      AND column_name = 'survey_date'
  ) INTO has_survey_date;

  IF has_shrub_id THEN
    sql_text := 'SELECT COUNT(*) FROM urbancndep.stem_comment WHERE shrub_id IS NULL';
    EXECUTE sql_text INTO v_null_shrub;
  ELSE
    v_null_shrub := -1;
  END IF;

  IF has_survey_date THEN
    sql_text := 'SELECT COUNT(*) FROM urbancndep.stem_comment WHERE survey_date IS NULL';
    EXECUTE sql_text INTO v_null_survey;
  ELSE
    v_null_survey := -1;
  END IF;

  RAISE NOTICE 'stem_comment_null_new_keys: null_shrub_id=%, null_survey_date=% (value -1 means column absent)', v_null_shrub, v_null_survey;
END;
$$;

DO $$
DECLARE
  has_shrub_id boolean;
  has_survey_date boolean;
  v_duplicate_groups bigint := -1;
  v_status text := 'FAIL';
BEGIN
  SELECT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'urbancndep'
      AND table_name = 'stem_comment'
      AND column_name = 'shrub_id'
  ) INTO has_shrub_id;

  SELECT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'urbancndep'
      AND table_name = 'stem_comment'
      AND column_name = 'survey_date'
  ) INTO has_survey_date;

  IF has_shrub_id AND has_survey_date THEN
    EXECUTE $sql$
      SELECT COUNT(*)
      FROM (
        SELECT
          urbancndep.stem_comment.shrub_id,
          urbancndep.stem_comment.survey_date
        FROM urbancndep.stem_comment
        WHERE urbancndep.stem_comment.shrub_id IS NOT NULL
          AND urbancndep.stem_comment.survey_date IS NOT NULL
        GROUP BY urbancndep.stem_comment.shrub_id, urbancndep.stem_comment.survey_date
        HAVING COUNT(*) > 1
      ) duplicate_group_count
    $sql$
    INTO v_duplicate_groups;

    IF v_duplicate_groups = 0 THEN
      v_status := 'PASS';
    END IF;

    RAISE NOTICE 'stem_comment_duplicate_groups_shrub_date: duplicate_groups=%, status=%',
      v_duplicate_groups, v_status;
  ELSE
    RAISE NOTICE 'stem_comment_duplicate_groups_shrub_date: SKIP (required columns absent)';
  END IF;
END;
$$;

\echo '=== 3) post_note standardization checks ==='
DO $$
DECLARE
  v_noncanonical bigint := NULL;
  v_distinct_values bigint := NULL;
  v_status text := 'FAIL';
BEGIN
  IF to_regclass('urbancndep.stems') IS NULL THEN
    RAISE NOTICE 'stems_post_note_noncanonical_count: SKIP (table urbancndep.stems does not exist)';
    RAISE NOTICE 'stems_post_note_distribution: SKIP (table urbancndep.stems does not exist)';
  ELSE
    EXECUTE $sql$
      SELECT COUNT(*)
      FROM urbancndep.stems
      WHERE post_note IS NOT NULL
        AND post_note NOT IN ('dead (on branch or ground)', 'not found')
    $sql$
    INTO v_noncanonical;

    IF v_noncanonical = 0 THEN
      v_status := 'PASS';
    END IF;

    RAISE NOTICE 'stems_post_note_noncanonical_count: noncanonical_count=%, status=%',
      v_noncanonical, v_status;

    SELECT COUNT(*)
    INTO v_distinct_values
    FROM (
      SELECT COALESCE(post_note, '<NULL>') AS post_note_value
      FROM urbancndep.stems
      GROUP BY COALESCE(post_note, '<NULL>')
    ) d;

    RAISE NOTICE 'stems_post_note_distribution: distinct_values=%', v_distinct_values;
  END IF;
END;
$$;

\echo '=== 3b) pre_note migration checks ==='
DO $$
DECLARE
  v_remaining_pre_missing_comments bigint := NULL;
  v_status text := 'FAIL';
BEGIN
  IF to_regclass('urbancndep.stem_comment') IS NULL THEN
    RAISE NOTICE 'pre_missing_value_comment_remaining: SKIP (table urbancndep.stem_comment does not exist)';
  ELSE
    EXECUTE $sql$
      SELECT COUNT(*)
      FROM urbancndep.stem_comment
      WHERE post_measurement = FALSE
        AND BTRIM(LOWER(comment)) = 'missing value'
    $sql$
    INTO v_remaining_pre_missing_comments;

    IF v_remaining_pre_missing_comments = 0 THEN
      v_status := 'PASS';
    END IF;

    RAISE NOTICE 'pre_missing_value_comment_remaining: rows=%, status=%',
      v_remaining_pre_missing_comments, v_status;
  END IF;
END;
$$;

\echo '=== 4) Date-filter guardrail checks (source cutoff 2022-05-13) ==='
DO $$
DECLARE
  v_rows_before_cutoff bigint := NULL;
BEGIN
  IF to_regclass('urbancndep.shrub_measurements') IS NULL THEN
    RAISE NOTICE 'source_rows_before_cutoff_with_notes: SKIP (table urbancndep.shrub_measurements does not exist)';
  ELSE
    EXECUTE $sql$
      SELECT COUNT(*)
      FROM urbancndep.shrub_measurements
      WHERE survey_date < DATE '2022-05-13'
        AND NULLIF(
          btrim(
            regexp_replace(
              regexp_replace(
                regexp_replace(COALESCE(notes, ''), E'[\r\n]+', ' ', 'g'),
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
        ) IS NOT NULL
    $sql$
    INTO v_rows_before_cutoff;

    RAISE NOTICE 'source_rows_before_cutoff_with_notes: rows_before_cutoff=%', v_rows_before_cutoff;
  END IF;
END;
$$;

\echo '=== 5) Text cleanup checks ==='
DO $$
DECLARE
  v_rows_with_ctrl bigint := NULL;
  v_status text := 'FAIL';
BEGIN
  IF to_regclass('urbancndep.stem_comment') IS NULL THEN
    RAISE NOTICE 'stem_comment_control_char_rows: SKIP (table urbancndep.stem_comment does not exist)';
  ELSE
    EXECUTE $sql$
      SELECT COUNT(*)
      FROM urbancndep.stem_comment
      WHERE comment ~ '[[:cntrl:]]'
    $sql$
    INTO v_rows_with_ctrl;

    IF v_rows_with_ctrl = 0 THEN
      v_status := 'PASS';
    END IF;

    RAISE NOTICE 'stem_comment_control_char_rows: rows_with_ctrl_chars=%, status=%',
      v_rows_with_ctrl, v_status;
  END IF;
END;
$$;

DO $$
DECLARE
  v_rows_with_ctrl bigint := NULL;
  v_status text := 'FAIL';
BEGIN
  IF to_regclass('urbancndep.shrub_measurements') IS NULL THEN
    RAISE NOTICE 'shrub_measurements_notes_control_char_rows: SKIP (table urbancndep.shrub_measurements does not exist)';
  ELSE
    EXECUTE $sql$
      SELECT COUNT(*)
      FROM urbancndep.shrub_measurements
      WHERE notes ~ '[[:cntrl:]]'
    $sql$
    INTO v_rows_with_ctrl;

    IF v_rows_with_ctrl = 0 THEN
      v_status := 'PASS';
    END IF;

    RAISE NOTICE 'shrub_measurements_notes_control_char_rows: rows_with_ctrl_chars=%, status=%',
      v_rows_with_ctrl, v_status;
  END IF;
END;
$$;

DO $$
DECLARE
  v_rows_with_ctrl bigint := NULL;
  v_status text := 'FAIL';
BEGIN
  IF to_regclass('urbancndep.stems') IS NULL THEN
    RAISE NOTICE 'stems_post_note_control_char_rows: SKIP (table urbancndep.stems does not exist)';
  ELSE
    EXECUTE $sql$
      SELECT COUNT(*)
      FROM urbancndep.stems
      WHERE post_note ~ '[[:cntrl:]]'
    $sql$
    INTO v_rows_with_ctrl;

    IF v_rows_with_ctrl = 0 THEN
      v_status := 'PASS';
    END IF;

    RAISE NOTICE 'stems_post_note_control_char_rows: rows_with_ctrl_chars=%, status=%',
      v_rows_with_ctrl, v_status;
  END IF;
END;
$$;

DO $$
DECLARE
  v_rows_with_ctrl bigint := NULL;
  v_status text := 'FAIL';
BEGIN
  IF to_regclass('urbancndep.stem_plot_notes') IS NULL THEN
    RAISE NOTICE 'stem_plot_notes_control_char_rows: SKIP (table urbancndep.stem_plot_notes does not exist)';
  ELSE
    EXECUTE $sql$
      SELECT COUNT(*)
      FROM urbancndep.stem_plot_notes
      WHERE plot_notes ~ '[[:cntrl:]]'
    $sql$
    INTO v_rows_with_ctrl;

    IF v_rows_with_ctrl = 0 THEN
      v_status := 'PASS';
    END IF;

    RAISE NOTICE 'stem_plot_notes_control_char_rows: rows_with_ctrl_chars=%, status=%',
      v_rows_with_ctrl, v_status;
  END IF;
END;
$$;

-- Heuristic for prior tokenization corruption signatures.
DO $$
DECLARE
  v_artifact_rows bigint := NULL;
BEGIN
  IF to_regclass('urbancndep.stem_comment') IS NULL THEN
    RAISE NOTICE 'stem_comment_pipe_artifact_rows: SKIP (table urbancndep.stem_comment does not exist)';
  ELSE
    EXECUTE $sql$
      SELECT COUNT(*)
      FROM urbancndep.stem_comment
      WHERE POSITION('|' IN comment) > 0
    $sql$
    INTO v_artifact_rows;

    RAISE NOTICE 'stem_comment_pipe_artifact_rows: possible_artifact_rows=%', v_artifact_rows;
  END IF;
END;
$$;

\echo '=== 6) Audit table snapshots (if present) ==='
SELECT
  'stem_comment_redesign_audit_exists' AS check_name,
  CASE WHEN to_regclass('urbancndep.stem_comment_redesign_audit') IS NOT NULL THEN 'YES' ELSE 'NO' END AS table_exists;

SELECT
  'stem_plot_notes_reject_audit_exists' AS check_name,
  CASE WHEN to_regclass('urbancndep.stem_plot_notes_reject_audit') IS NOT NULL THEN 'YES' ELSE 'NO' END AS table_exists;

SELECT
  'comment_migration_log_exists' AS check_name,
  CASE WHEN to_regclass('urbancndep.comment_migration_log') IS NOT NULL THEN 'YES' ELSE 'NO' END AS table_exists;

DO $$
BEGIN
  IF to_regclass('urbancndep.stem_comment_redesign_audit') IS NOT NULL THEN
    RAISE NOTICE 'stem_comment_redesign_audit rows=%',
      (SELECT COUNT(*) FROM urbancndep.stem_comment_redesign_audit);
  ELSE
    RAISE NOTICE 'stem_comment_redesign_audit not present';
  END IF;

  IF to_regclass('urbancndep.stem_plot_notes_reject_audit') IS NOT NULL THEN
    RAISE NOTICE 'stem_plot_notes_reject_audit rows=%',
      (SELECT COUNT(*) FROM urbancndep.stem_plot_notes_reject_audit);
  ELSE
    RAISE NOTICE 'stem_plot_notes_reject_audit not present';
  END IF;

  IF to_regclass('urbancndep.comment_migration_log') IS NOT NULL THEN
    RAISE NOTICE 'comment_migration_log rows=%',
      (SELECT COUNT(*) FROM urbancndep.comment_migration_log);
  ELSE
    RAISE NOTICE 'comment_migration_log not present';
  END IF;
END;
$$;
