\copy (
WITH stem_rows AS (
  SELECT
    stems.id AS stem_id,
    stems.shrub_id,
    stems.direction,
    stems.pre_date,
    stems.post_date,
    stems.pre_note,
    stems.post_note,
    stem_lengths.post_measurement,
    stem_lengths.length_in_mm,
    CASE
      WHEN stem_lengths.post_measurement IS TRUE THEN stems.post_date
      WHEN stem_lengths.post_measurement IS FALSE THEN stems.pre_date
      ELSE NULL
    END AS measurement_date
  FROM urbancndep.stems
  LEFT JOIN urbancndep.stem_lengths
    ON stem_lengths.stem_id = stems.id
),
aggregated_plot_notes AS (
  SELECT
    stem_plot_notes.plot_id,
    stem_plot_notes.survey_date,
    STRING_AGG(DISTINCT stem_plot_notes.plot_notes, '; ') AS plot_comment
  FROM urbancndep.stem_plot_notes
  GROUP BY
    stem_plot_notes.plot_id,
    stem_plot_notes.survey_date
)
SELECT
  sites.code AS site_code,
  plots.id AS plot_id,
  treatments.code AS treatment_code,
  shrub_species.scientific_name,
  shrubs.code AS shrub_code,
  stem_rows.direction,
  stem_rows.pre_date,
  stem_rows.post_date,
  stem_rows.pre_note,
  stem_rows.post_note,
  stem_rows.post_measurement,
  stem_rows.measurement_date,
  stem_rows.length_in_mm AS stem_length,
  stem_comment.comment AS stem_comment,
  aggregated_plot_notes.plot_comment
FROM stem_rows
JOIN urbancndep.shrubs
  ON shrubs.id = stem_rows.shrub_id
JOIN urbancndep.plots
  ON shrubs.plot_id = plots.id
JOIN urbancndep.sites
  ON plots.site_id = sites.id
JOIN urbancndep.treatments
  ON plots.treatment_id = treatments.id
JOIN urbancndep.shrub_species
  ON shrubs.shrub_species_id = shrub_species.id
LEFT JOIN urbancndep.stem_comment
  ON stem_comment.shrub_id = stem_rows.shrub_id
 AND stem_comment.survey_date = stem_rows.measurement_date
LEFT JOIN aggregated_plot_notes
  ON aggregated_plot_notes.plot_id = plots.id
 AND aggregated_plot_notes.survey_date = stem_rows.measurement_date
WHERE
  NOT (
    EXTRACT(YEAR FROM stem_rows.pre_date) = 2016
    AND EXTRACT(MONTH FROM stem_rows.pre_date) = 10
    AND stem_rows.post_date IS NULL
  )
  AND NOT (plots.id = 13 AND stem_rows.pre_date = DATE '2010-05-10')
  AND NOT (plots.id = 12 AND stem_rows.pre_date = DATE '2010-05-11')
ORDER BY
  stem_rows.pre_date,
  plots.id,
  stem_rows.post_measurement,
  shrubs.code,
  stem_rows.direction
  ) TO '/tmp/stems_out_new_999.csv' WITH CSV HEADER;
