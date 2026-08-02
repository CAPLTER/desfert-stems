-- urbancndep_stems_integrity_audit.sql
--
-- Purpose:
--   Identify historical key, referential-integrity, and logical-uniqueness
--   conditions that could make the DesFert stems workflow ambiguous or prevent
--   future database constraints from being added safely.
--
-- Safety and scope:
--   * Read-only: every database statement is SELECT-only.
--   * Diagnostic: findings are reported but never repaired or suppressed.
--   * Historical: a REVIEW result does not necessarily mean the current upload
--     introduced the problem.
--   * Focused: this is not an exhaustive audit of the urbancndep schema.
--
-- Output contract:
--   * Detailed queries return no rows when no matching issue is present.
--   * Detail rows usually represent physical database records.
--   * Duplicate queries return one row per violating logical-key group;
--     duplicate_rows is the number of physical records in that group.
--   * The missing-shrub sample is capped at 20 rows and is not a total.
--   * The final summary reports PASS for zero issues and REVIEW otherwise.
--     REVIEW is informational and does not make psql exit with an error.
--
-- Example:
--   psql -h localhost -U srearl -d caplter -v ON_ERROR_STOP=1 \
--     -f urbancndep_stems_integrity_audit.sql

\set ON_ERROR_STOP on

\echo ''
\echo '=== shrub_measurements: missing shrub IDs by year ==='
\echo 'Purpose: quantify measurements that cannot yet be linked to one shrub.'
\echo 'Returns: one row per survey year; issue_rows counts physical measurement rows.'
-- Purpose: establish the extent and historical concentration of missing shrub
-- foreign keys before considering NOT NULL or other mapping constraints.
-- Returns: one row per affected survey year. issue_rows is the number of
-- physical shrub_measurements rows with a NULL shrub_id in that year.
SELECT
  EXTRACT(YEAR FROM shrub_measurements.survey_date) AS survey_year,
  COUNT(*) AS issue_rows
FROM urbancndep.shrub_measurements AS shrub_measurements
WHERE shrub_measurements.shrub_id IS NULL
GROUP BY EXTRACT(YEAR FROM shrub_measurements.survey_date)
ORDER BY survey_year;

\echo ''
\echo '=== shrub_measurements: missing shrub ID sample ==='
\echo 'Purpose: provide a small set of missing-shrub records for manual diagnosis.'
\echo 'Returns: at most 20 physical rows; this sample is not the total issue count.'
-- Purpose: provide representative identifiers and legacy plant text for
-- investigating how missing shrub IDs might be resolved.
-- Returns: at most 20 physical shrub_measurements rows, ordered from the
-- earliest survey dates. Use the preceding yearly counts for total volume.
SELECT
  shrub_measurements.id,
  shrub_measurements.plot_id,
  shrub_measurements.survey_date,
  shrub_measurements.plant
FROM urbancndep.shrub_measurements AS shrub_measurements
WHERE shrub_measurements.shrub_id IS NULL
ORDER BY shrub_measurements.survey_date, shrub_measurements.plot_id
LIMIT 20;

\echo ''
\echo '=== shrub_measurements: orphaned shrub IDs ==='
\echo 'Purpose: find non-NULL shrub IDs that reference no current shrub record.'
\echo 'Returns: one row per orphaned physical measurement; no rows means none found.'
-- Purpose: detect broken references that a validated foreign key would reject.
-- Returns: one row per physical shrub_measurements record whose non-NULL
-- shrub_id has no matching shrubs.id.
SELECT
  shrub_measurements.id,
  shrub_measurements.plot_id,
  shrub_measurements.shrub_id,
  shrub_measurements.survey_date
FROM urbancndep.shrub_measurements AS shrub_measurements
LEFT JOIN urbancndep.shrubs AS shrubs
  ON shrubs.id = shrub_measurements.shrub_id
WHERE shrub_measurements.shrub_id IS NOT NULL
  AND shrubs.id IS NULL
ORDER BY shrub_measurements.survey_date, shrub_measurements.plot_id;

\echo ''
\echo '=== shrub_measurements: shrub/plot disagreements ==='
\echo 'Purpose: find measurements whose direct plot ID conflicts with their shrub mapping.'
\echo 'Returns: one physical measurement per disagreement, with both plot IDs shown.'
-- Purpose: detect internally inconsistent mappings where shrub_id resolves to a
-- shrub on a different plot than shrub_measurements.plot_id.
-- Returns: one row per physical shrub_measurements record in disagreement;
-- measurement_plot_id and shrub_plot_id expose the conflicting values.
SELECT
  shrub_measurements.id,
  shrub_measurements.plot_id AS measurement_plot_id,
  shrubs.plot_id AS shrub_plot_id,
  shrub_measurements.shrub_id,
  shrub_measurements.survey_date
FROM urbancndep.shrub_measurements AS shrub_measurements
JOIN urbancndep.shrubs AS shrubs
  ON shrubs.id = shrub_measurements.shrub_id
WHERE shrub_measurements.plot_id <> shrubs.plot_id
ORDER BY shrub_measurements.survey_date, shrub_measurements.id;

\echo ''
\echo '=== shrub_measurements: duplicate shrub/date groups ==='
\echo 'Purpose: find multiple dimension records for one shrub measurement event.'
\echo 'Returns: one logical shrub/date group; duplicate_rows is its physical row count.'
-- Purpose: identify logical-key collisions that could make one shrub's
-- dimensions ambiguous for a survey event.
-- Returns: one row per duplicated (shrub_id, survey_date) group.
-- duplicate_rows is the number of physical records in that group, not the
-- number beyond the first record.
SELECT
  shrub_measurements.shrub_id,
  shrub_measurements.survey_date,
  COUNT(*) AS duplicate_rows
FROM urbancndep.shrub_measurements AS shrub_measurements
WHERE shrub_measurements.shrub_id IS NOT NULL
GROUP BY shrub_measurements.shrub_id, shrub_measurements.survey_date
HAVING COUNT(*) > 1
ORDER BY duplicate_rows DESC, shrub_measurements.survey_date;

\echo ''
\echo '=== stem_plot_notes: missing or orphaned plot IDs ==='
\echo 'Purpose: find plot notes that cannot be linked to a current plot.'
\echo 'Returns: one physical note row with reason missing_plot_id or orphaned_plot_id.'
-- Purpose: detect plot-event notes that lack a usable plots.id reference.
-- Returns: one row per physical stem_plot_notes record. reason distinguishes a
-- NULL plot_id from a non-NULL plot_id with no matching plots row.
SELECT
  stem_plot_notes.id,
  stem_plot_notes.plot_id,
  stem_plot_notes.survey_date,
  CASE
    WHEN stem_plot_notes.plot_id IS NULL THEN 'missing_plot_id'
    ELSE 'orphaned_plot_id'
  END AS reason
FROM urbancndep.stem_plot_notes AS stem_plot_notes
LEFT JOIN urbancndep.plots AS plots
  ON plots.id = stem_plot_notes.plot_id
WHERE stem_plot_notes.plot_id IS NULL
   OR plots.id IS NULL
ORDER BY stem_plot_notes.survey_date, stem_plot_notes.id;

\echo ''
\echo '=== stem_plot_notes: duplicate plot/date groups ==='
\echo 'Purpose: find multiple note records for one plot visit.'
\echo 'Returns: one logical plot/date group; duplicate_rows is its physical row count.'
-- Purpose: identify logical-key collisions that can cause more than one
-- plot-level comment to map to the same survey event.
-- Returns: one row per duplicated (plot_id, survey_date) group.
-- duplicate_rows is the number of physical records in that group, not the
-- number beyond the first record.
SELECT
  stem_plot_notes.plot_id,
  stem_plot_notes.survey_date,
  COUNT(*) AS duplicate_rows
FROM urbancndep.stem_plot_notes AS stem_plot_notes
GROUP BY stem_plot_notes.plot_id, stem_plot_notes.survey_date
HAVING COUNT(*) > 1
ORDER BY duplicate_rows DESC, stem_plot_notes.survey_date;

\echo ''
\echo '=== stems workflow mapping audit summary ==='
\echo 'Purpose: provide a compact status view of the principal upload-mapping checks.'
\echo 'Returns: one row per named check; PASS means zero and REVIEW means nonzero.'
\echo 'Note: duplicate issue_rows count violating groups; other counts are physical rows.'
-- Purpose: provide an operator-friendly roll-up of the principal conditions
-- checked above. This condensed result does not replace the detailed outputs.
-- Returns: one row per check_name. issue_rows counts physical records for
-- missing-ID and plot-disagreement checks, but counts violating logical-key
-- groups for duplicate checks. status is PASS when issue_rows is zero and
-- REVIEW otherwise; REVIEW does not raise a SQL error.
WITH audit_counts AS (
  SELECT
    'shrub_measurements_missing_shrub_id'::text AS check_name,
    COUNT(*) AS issue_rows
  FROM urbancndep.shrub_measurements AS shrub_measurements
  WHERE shrub_measurements.shrub_id IS NULL

  UNION ALL

  SELECT
    'shrub_measurements_plot_disagreement',
    COUNT(*)
  FROM urbancndep.shrub_measurements AS shrub_measurements
  JOIN urbancndep.shrubs AS shrubs
    ON shrubs.id = shrub_measurements.shrub_id
  WHERE shrub_measurements.plot_id <> shrubs.plot_id

  UNION ALL

  SELECT
    'stem_plot_notes_missing_plot_id',
    COUNT(*)
  FROM urbancndep.stem_plot_notes AS stem_plot_notes
  WHERE stem_plot_notes.plot_id IS NULL

  UNION ALL

  SELECT
    'shrub_measurements_duplicate_shrub_date',
    COUNT(*)
  FROM (
    SELECT
      shrub_measurements.shrub_id,
      shrub_measurements.survey_date
    FROM urbancndep.shrub_measurements AS shrub_measurements
    WHERE shrub_measurements.shrub_id IS NOT NULL
    GROUP BY shrub_measurements.shrub_id, shrub_measurements.survey_date
    HAVING COUNT(*) > 1
  ) AS duplicate_shrub_measurements

  UNION ALL

  SELECT
    'stem_plot_notes_duplicate_plot_date',
    COUNT(*)
  FROM (
    SELECT
      stem_plot_notes.plot_id,
      stem_plot_notes.survey_date
    FROM urbancndep.stem_plot_notes AS stem_plot_notes
    GROUP BY stem_plot_notes.plot_id, stem_plot_notes.survey_date
    HAVING COUNT(*) > 1
  ) AS duplicate_plot_notes
)
SELECT
  audit_counts.check_name,
  audit_counts.issue_rows,
  CASE WHEN audit_counts.issue_rows = 0 THEN 'PASS' ELSE 'REVIEW' END AS status
FROM audit_counts
ORDER BY audit_counts.check_name;
