#' @title helper: document missing new stem data
#'
#' @description Failing to collect and/or misidentifying a plant or stem
#' direction such that a plot*plant*direction ends up not having a new (i.e.,
#' pre-date) measurement is an all too common occurence. These data are
#' generally not recoverable but it is important to note in the database that
#' the data were not collected. We can use \code{identify_new_missing} to
#' identify occurence where new stem measurements are not available;
#' \code{annotate_new_missing} can then be used to add the appropriate
#' documentation to the database, including a `missing value` note in
#' \code{urbancndep.stems.pre_note}.
#'
#' @export
#'
annotate_new_missing <- function(
  pre_year  = max(lubridate::year(plots_plants[["survey_date"]])),
  pre_month = max(lubridate::month(plots_plants[["survey_date"]])),
  plot_id,
  plant_id,
  direction
  ) {

  this_direction <- paste0(direction, "%")

  # Where missing, add NULL value to stem length - use details from
  # new_missing(). But note that new_missing() will not catch all, see error
  # checking for newLenghts above to populate year, month, plot, plant, and
  # direction. Do this for each missing stem.

  lengths_query <- glue::glue_sql("
    INSERT INTO urbancndep.stem_lengths
    (
      stem_id,
      length_in_mm,
      post_measurement
    )
    (
      SELECT
        id,
        NULL,
        FALSE
      FROM urbancndep.stems
      WHERE
        EXTRACT (YEAR FROM pre_date) = { pre_year } AND
        EXTRACT (MONTH FROM pre_date) = { pre_month } AND
        shrub_id IN (
          SELECT id
          FROM urbancndep.shrubs
          WHERE
            plot_id = { plot_id } AND
            code LIKE { plant_id }
          ) AND
        direction ILIKE { this_direction }
    )
  ;
  ",
  .con = DBI::ANSI()
  )

  # Where missing, add missing-note context directly to stems.pre_note for the
  # pre/new measurement stem.

  pre_note_query <- glue::glue_sql("
    UPDATE urbancndep.stems
    SET pre_note = CASE
      WHEN pre_note IS NULL THEN 'missing value'
      WHEN POSITION('missing value' IN pre_note) > 0 THEN pre_note
      ELSE pre_note || '; missing value'
    END
    WHERE
      EXTRACT (YEAR FROM pre_date) = { pre_year } AND
      EXTRACT (MONTH FROM pre_date) = { pre_month } AND
      shrub_id IN (
        SELECT id
        FROM urbancndep.shrubs
        WHERE
          plot_id = { plot_id } AND
          code LIKE { plant_id }
        ) AND
      direction ILIKE { this_direction }
    ;
    ",
    .con = DBI::ANSI()
  )

  # begin transaction
  DBI::dbBegin(conn = pg)

  lengths_insert <- DBI::dbExecute(
    conn      = pg,
    statement = lengths_query
  )

  pre_note_update <- DBI::dbExecute(
    conn      = pg,
    statement = pre_note_query
  )

  if (lengths_insert == 1 && pre_note_update == 1) {

    DBI::dbCommit(conn = pg)
    message("annotated missing stem: plot ", plot_id,  "; plant ", plant_id, "; direction ", direction)

  } else {

    DBI::dbRollback(conn = pg)
    stop("rolling back transaction")

  }

}
