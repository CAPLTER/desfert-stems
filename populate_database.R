# README -----------------------------------------------------------------------

# Workflow to upload DesFert stem-length data to the urbancndep database.


source("helper_load_table.R")
source("helper_identify_new_missing.R")
source("helper_annotate_new_missing.R")

# temporary schema -------------------------------------------------------------

DBI::dbExecute(pg, "DROP SCHEMA IF EXISTS stems_temp CASCADE ;")
DBI::dbExecute(pg, "CREATE SCHEMA IF NOT EXISTS stems_temp ;")


# add raw data to temp postgres tables -----------------------------------------

data_tables <- c("new", "old", "plots_plants", "plot_notes", "shrub_dimensions")

purrr::walk(.x = data_tables, ~ helper_load_table(table_name = .x))


# data manipulation (SQL) -----------------------------------------------------

# run SQL run sequentially!

# alter stems_temp.plots_plants as appropriate
# - change survey_date to a date (may not be needed)
# - add shrub ID from urbancndep
# - prep for and add shrub_ids to the plots_plants tablet-derived data
# - prep for and add stem_id generated from insert of new stems to plots_plants (a later step)

DBI::dbExecute(
  conn      = pg,
  statement = "
  ALTER TABLE stems_temp.plots_plants
    ALTER COLUMN survey_date TYPE DATE USING survey_date::date,
    ADD COLUMN shrub_id INTEGER,
    ADD COLUMN stem_id_new INTEGER,
    ADD COLUMN stem_id_old INTEGER
  ;
  "
)


# add stem_id generated from update of old stems to plots_plants (a later step)

DBI::dbExecute(
  conn      = pg,
  statement = "
  UPDATE stems_temp.plots_plants
  SET shrub_id = urbancndep.shrubs.id
  FROM urbancndep.shrubs
  WHERE
    urbancndep.shrubs.plot_id = plots_plants.plot_id AND
    urbancndep.shrubs.code = plots_plants.plant_id
  ;
  "
)


# update existing pg.stems with this survey's post date & notes

# Be certain to set month & year to the correct pre date, which should be the
# most recent set (may include multiple months).

# Get the month and date of the most recent previous stems survey to pass to
# the update query. Note that this query is specific to when the last set of
# stems were measured in a single month (e.g., 5 or 10). If stem surveys
# spanned multiple months, which is rare but has happened, you either need to
# update the query or pass those values manually.

recent_pre_date <- DBI::dbGetQuery(
  conn      = pg,
  statement = "
  SELECT
    DISTINCT EXTRACT(MONTH FROM pre_date) AS recentmos,
    EXTRACT(YEAR FROM pre_date) AS recentyear
  FROM
    urbancndep.stems
  WHERE
    EXTRACT(YEAR FROM pre_date) = (SELECT MAX(EXTRACT(YEAR FROM pre_date)) FROM urbancndep.stems)
  ;
  "
)


pre_date_month <- max(recent_pre_date$recentmos)
pre_date_year  <- max(recent_pre_date$recentyear)

# to address manually, change the last two lines of update_post_query to:
# EXTRACT(MONTH from urbancndep.stems.pre_date) = __SET_PREVIOUS_PERIOD_MONTH__ AND
# EXTRACT(YEAR from urbancndep.stems.pre_date) = __SET_PREVIOUS_PERIOD_YEAR;") # set appropriatly !

update_post_query <- glue::glue_sql("
  UPDATE urbancndep.stems
  SET
    post_date = plots_plants.survey_date,
    post_note = plots_plants.post_note
  FROM stems_temp.plots_plants
  WHERE
    plots_plants.shrub_id = urbancndep.stems.shrub_id AND
    plots_plants.direction = urbancndep.stems.direction AND
    EXTRACT(MONTH from urbancndep.stems.pre_date) IN ({ pre_date_month }) AND
    EXTRACT(YEAR from urbancndep.stems.pre_date) = { pre_date_year }
  ;
  ",
  .con = DBI::ANSI()
)

DBI::dbExecute(
  conn      = pg,
  statement = update_post_query
)


# insert pre data into pg.stems

DBI::dbExecute(
  conn      = pg,
  statement = "
  INSERT INTO urbancndep.stems
  (
    shrub_id,
    direction,
    pre_date
  )
  (
    SELECT
      shrub_id,
      direction,
      survey_date
    FROM stems_temp.plots_plants
  )
  ;
  "
)


# add stem_id generated from insert of new stems to plots_plants

DBI::dbExecute(
  conn      = pg,
  statement = "
  UPDATE stems_temp.plots_plants
  SET stem_id_new = urbancndep.stems.id
  FROM urbancndep.stems
  WHERE
    urbancndep.stems.shrub_id = plots_plants.shrub_id AND
    urbancndep.stems.direction = plots_plants.direction AND
    urbancndep.stems.pre_date = plots_plants.survey_date
  ;
  "
) # note pre_date used here!


# add stem_id generated from update of old stems to plots_plants

DBI::dbExecute(
  conn      = pg,
  statement = "
  UPDATE stems_temp.plots_plants
  SET stem_id_old = urbancndep.stems.id
  FROM urbancndep.stems
  WHERE
    urbancndep.stems.shrub_id = plots_plants.shrub_id AND
    urbancndep.stems.direction = plots_plants.direction AND
    urbancndep.stems.post_date = plots_plants.survey_date
  ;
  "
) # note post_date used here!


# prep for and add stem_id generated from update of stems to old;

DBI::dbExecute(
  conn      = pg,
  statement = "
  ALTER TABLE stems_temp.old
    ADD COLUMN stem_id INTEGER,
    ADD COLUMN postmeas boolean
  ;
  "
)

DBI::dbExecute(
  conn      = pg,
  statement = "
  UPDATE stems_temp.old
  SET stem_id = plots_plants.stem_id_old
  FROM stems_temp.plots_plants
  WHERE
    plots_plants.plant_id = old.plant_id AND
    plots_plants.plot_id = old.plot_id AND
    SUBSTRING(plots_plants.direction, 1, 1) = old.old_direction
  ;
  "
)


# add boolean TRUE to denote post measurement

DBI::dbExecute(
  conn      = pg,
  statement = "
  UPDATE stems_temp.old
  SET postmeas=TRUE
  ;
  "
)


# insert old lengths into pg.stem_lengths

DBI::dbExecute(
  conn      = pg,
  statement = "
  INSERT INTO urbancndep.stem_lengths
  (
    stem_id,
    length_in_mm,
    post_measurement
  )
  (
    SELECT
      stem_id,
      old_length::DOUBLE PRECISION,
      postmeas
    FROM stems_temp.old
    WHERE old_length IS NOT NULL
  )
  ;
  "
)


# prep for and add stem_id generated from insert of stems to new;
# prep boolean FALSE to denote post measurement (later step)

DBI::dbExecute(
  conn      = pg,
  statement = "
  ALTER TABLE stems_temp.new
    ADD COLUMN stem_id integer,
    ADD COLUMN postmeas boolean
  ;
  "
)

DBI::dbExecute(
  conn      = pg,
  statement = "
  UPDATE stems_temp.new
  SET stem_id = plots_plants.stem_id_new
  FROM stems_temp.plots_plants
  WHERE
    plots_plants.plant_id = new.plant_id AND
    plots_plants.plot_id = new.plot_id AND
    SUBSTRING(plots_plants.direction, 1, 1) = new.new_direction
  ;
  "
)

DBI::dbExecute(
  conn      = pg,
  statement = "
  UPDATE stems_temp.new
  SET postmeas = FALSE
  ;
  "
)


# insert new lengths into pg.stem_lengths

DBI::dbExecute(
  conn      = pg,
  statement = "
  INSERT INTO urbancndep.stem_lengths
  (
    stem_id,
    length_in_mm,
    post_measurement
  )
  (
    SELECT
      stem_id,
      new_length::DOUBLE PRECISION,
      postmeas
    FROM stems_temp.new
    WHERE new_length is not null
    )
  ;
  "
)


# insert any notes about the plants into the stems_comment table by setting
# post_measuremnt to TRUE, the query assumes the comment was made upon old
# collection (post visit) this is reasonable and it is hard to imagine a comment
# corresponding to the new collection, though dead or new plant might be an
# example. Just be cognizant of this and edit as required.

DBI::dbExecute(
  conn      = pg,
  statement = "
  INSERT INTO urbancndep.stem_comment
  (
    stem_id,
    comment,
    post_measurement
  )
  (
    SELECT
      stem_id_old,  -- forces association with old lenghts (post visit)
      note_about_plant,
      'TRUE'        -- forces association with old lengths (post visit)
    FROM stems_temp.plots_plants
    WHERE note_about_plant IS NOT NULL
    )
  ;
  "
)


# add plot notes (new as of 2018-10-22)

DBI::dbExecute(
  conn      = pg,
  statement = "
  INSERT INTO urbancndep.stem_plot_notes
  (
    plot_id,
    survey_date,
    plot_notes
  )
  (
    SELECT
      plot_id,
      survey_date::date,
      plot_notes
    FROM stems_temp.plot_notes
    )
  ;
  "
)
          

# address missing NULL values ---------------------------------------------

# WHILE THE WORKFLOW TO ADDRESS MISSING NEW VALUES WAS IMPROVED WITH THE FALL
# 2022 WORKFLOW, THERE WERE NOT ANY NULL VALUES ASSOCIATED WITH THAT COLLECTION
# SO ADDRESS A WORKFLOW FOR NULL NEW VALUES IN A FUTURE COLLECTION.
# Still the case through the 2023 data.

# identify the details of any NULL value new stems using new_null(). The trick
# here is that this should only be an issue if there is a new stem with a single
# null value and no other values. If that is the case, you may have to treat
# that stem like a missing new value. However, if there are two new values for a
# single direction, then one can be NULL but the other one will be entered into
# the database and thus avoid having a missing direction. So, for example, in
# May 2017, there was a NULL value at WTM-71-L1-E but there was another
# measurement for E so we could simple ignore that NULL value

new_null <- function(workflow = "XML") {

  newLengths <- new

  dirSym <- c("N", "S", "W", "E")
  dirSym <- data.frame(dirSym, stringsAsFactors = F)

  nullvalues <- newLengths |>
    dplyr::filter(is.na(new_length))

  aframe <- data.frame(stringsAsFactors = F)

  for (i in 1:nrow(nullvalues)) {

    plot  <- nullvalues[i, ]$plot_id
    plant <- nullvalues[i, ]$plant_id

    aframe <- dplyr::bind_rows(
      aframe,
      newLengths |>
        dplyr::filter(plot_id == plot, plant_id == plant) |>
        dplyr::right_join(dirSym, by = c("new_direction" = "dirSym")) |>
        dplyr::mutate(plot_id = replace(plot_id, is.na(new_length), plot)) |>
        dplyr::mutate(plant_id = replace(plant_id, is.na(new_length), plant))
    )

  }

  aframe <- aframe |>
    dplyr::arrange(plot_id, plant_id, new_direction)

  return(aframe)

}

new_null()


# address missing NEW values ----------------------------------------------

# identify and document missing new stems data

# Identify the details of any missing new stems using
# \code{identify_new_missing}. We need to document cases where there are not
# any new-stem measurements for a particular plot*plant*direction. We can then
# pass these details to \code{annotate_new_missing} to: (1) document the NULL
# value for that direction, and (2) add a comment.

new_stems_missing <- identify_new_missing()

purrr::pwalk(new_stems_missing, ~ annotate_new_missing(plot_id = ..4, plant_id = ..1, direction = ..2))


# post-processing error checking -----------------------------------------------

years <- c(
  lubridate::year(Sys.Date()) - 1,
  lubridate::year(Sys.Date())
)

query_recent <- glue::glue_sql("
  SELECT
    s.code AS site_code,
    p.id AS plot_id,
    t.code AS treatment_code,
    sp.scientific_name,
    sh.code AS shrub_code,
    sh.note AS shrub_note,
    st.id,
    st.direction,
    st.pre_date,
    st.post_date,
    st.post_note,
    sl.post_measurement,
    sl.length_in_mm as stem_length,
    sc.comment
  FROM urbancndep.stems st
  JOIN urbancndep.shrubs sh ON sh.id = st.shrub_id
  JOIN urbancndep.plots p ON sh.plot_id = p.id
  JOIN urbancndep.sites s ON p.site_id = s.id
  JOIN urbancndep.treatments t ON p.treatment_id = t.id
  JOIN urbancndep.shrub_species sp ON sh.shrub_species_id = sp.id
  LEFT JOIN urbancndep.stem_lengths sl ON st.id = sl.stem_id
  LEFT JOIN urbancndep.stem_comment sc ON (sc.stem_id = st.id AND sl.post_measurement = sc.post_measurement)
  WHERE
    EXTRACT (YEAR FROM st.pre_date) IN ({ years* })
  ORDER BY
    sl.post_measurement,
    p.id,
    st.pre_date,
    sh.code,
    st.direction
  ;
  ",
  .con = DBI::ANSI()
)

recent_data <- DBI::dbGetQuery(
  conn      = pg,
  statement = query_recent
)


# shrub dimensions -------------------------------------------------------------

DBI::dbExecute(
  conn      = pg,
  statement = "
  ALTER TABLE stems_temp.shrub_dimensions
  ADD COLUMN shrub_id INTEGER
  ;
  "
)

DBI::dbExecute(
  conn      = pg,
  statement = "
  UPDATE stems_temp.shrub_dimensions
  SET shrub_id = urbancndep.shrubs.id
  FROM urbancndep.shrubs
  WHERE
    urbancndep.shrubs.plot_id = shrub_dimensions.plot_id AND
    urbancndep.shrubs.code = shrub_dimensions.plant_id
  ;
  "
)


DBI::dbExecute(
  conn      = pg,
  statement = "
  INSERT INTO urbancndep.shrub_measurements
  (
    plot_id,
    survey_date,
    plant,
    canopy_extent_n_s,
    canopy_extent_e_w,
    height,
    notes,
    shrub_id
  )
  (
    SELECT
      plot_id,
      survey_date::DATE,
      'Larrea tridentata',
      n_s,
      e_w,
      height_of_plant,
      note_about_plant,
      shrub_id
    FROM stems_temp.shrub_dimensions
  )
  ;
  "
)

# check shrub dimensions data

query_recent_measurents <- glue::glue_sql("
  SELECT
    sites.code AS site_code,
    plots.id AS plot_id,
    shrub_measurements.survey_date,
    shrub_measurements.canopy_extent_n_s AS n_s,
    shrub_measurements.canopy_extent_e_w AS e_w,
    shrub_measurements.height,
    shrubs.code AS shrub_code,
    shrub_measurements.notes
  FROM urbancndep.shrub_measurements
  LEFT JOIN urbancndep.shrubs ON (shrubs.id = shrub_measurements.shrub_id)
  LEFT JOIN urbancndep.plots ON (shrubs.plot_id = plots.id)
  LEFT JOIN urbancndep.sites ON (plots.site_id = sites.id)
  ORDER BY
    plots.id,
    shrubs.code
  ;
  ",
  .con = DBI::ANSI()
)

recent_measurements <- DBI::dbGetQuery(
  conn      = pg,
  statement = query_recent_measurents
)

# CLEAN UP ---------------------------------------------------------------------

DBI::dbExecute(
  conn      = pg,
  statement = "DROP SCHEMA IF EXISTS stems_temp CASCADE;"
)
