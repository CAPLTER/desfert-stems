#' @title helper: upload staged KoBo stems data atomically
#'
#' @description Validates staged KoBo tables, maps database-generated keys,
#' writes one collection to the urbancndep schema, and records the completed
#' source export. All production writes occur in one transaction and any failed
#' validation or affected-row check rolls the entire upload back.
#'
#' @param db_connection An open DBI PostgreSQL connection.
#' @param pre_date_month Integer vector identifying the preceding collection.
#' @param pre_date_year Integer year identifying the preceding collection.
#' @param source_file Basename of the KoBo source export.
#' @param source_md5 MD5 checksum identifying the exact KoBo source export.
#'
#' @return A named list of committed row counts, invisibly.
#' @export
#'
upload_stems_data <- function(
  db_connection = pg,
  pre_date_month,
  pre_date_year,
  source_file,
  source_md5
) {

  check_rpostgres_connection(
    connection = db_connection,
    connection_name = "upload transaction connection"
  )

  required_staging_tables <- c(
    "new",
    "old",
    "plots_plants",
    "plot_notes",
    "shrub_dimensions",
    "new_stems_missing"
  )

  missing_staging_tables <- required_staging_tables[
    !purrr::map_lgl(
      required_staging_tables,
      ~ DBI::dbExistsTable(
        db_connection,
        DBI::Id(schema = "stems_temp", table = .x)
      )
    )
  ]

  if (base::length(missing_staging_tables) > 0) {
    base::stop(
      "required staging tables are absent: ",
      base::paste(missing_staging_tables, collapse = ", "),
      call. = FALSE
    )
  }

  upload_log_exists <- DBI::dbGetQuery(
    db_connection,
    "
    SELECT to_regclass('urbancndep.stems_upload_log') IS NOT NULL AS exists
    ;
    "
  )$exists[[1]]

  if (!base::isTRUE(upload_log_exists)) {
    base::stop(
      "urbancndep.stems_upload_log is absent; apply ",
      "urbancndep_stems_workflow_safety_migration.sql before uploading",
      call. = FALSE
    )
  }

  upload_stats <- DBI::dbWithTransaction(
    db_connection,
    {
      prior_upload <- DBI::dbGetQuery(
        db_connection,
        glue::glue_sql(
          "
          SELECT
            stems_upload_log.id,
            stems_upload_log.completed_at
          FROM urbancndep.stems_upload_log AS stems_upload_log
          WHERE stems_upload_log.source_md5 = {source_md5}
          ;
          ",
          .con = db_connection
        )
      )

      if (base::nrow(prior_upload) > 0) {
        base::stop(
          "this KoBo export was already uploaded as upload ID ",
          prior_upload$id[[1]],
          call. = FALSE
        )
      }

      DBI::dbExecute(
        db_connection,
        "
        ALTER TABLE stems_temp.plots_plants
          ALTER COLUMN survey_date TYPE date USING survey_date::date,
          ADD COLUMN shrub_id integer,
          ADD COLUMN stem_id_new integer,
          ADD COLUMN stem_id_old integer
        ;
        "
      )
      DBI::dbExecute(
        db_connection,
        "
        ALTER TABLE stems_temp.old
          ADD COLUMN stem_id integer
        ;
        "
      )
      DBI::dbExecute(
        db_connection,
        "
        ALTER TABLE stems_temp.new
          ADD COLUMN stem_id integer
        ;
        "
      )
      DBI::dbExecute(
        db_connection,
        "
        ALTER TABLE stems_temp.shrub_dimensions
          ADD COLUMN shrub_id integer
        ;
        "
      )

      stop_on_database_issues(
        db_connection = db_connection,
        check_name = "plots_plants shrub mapping",
        statement = "
          SELECT
            plots_plants.id AS submission_id,
            plots_plants.plants_index,
            plots_plants.direction,
            COUNT(shrubs.id) AS match_count
          FROM stems_temp.plots_plants AS plots_plants
          LEFT JOIN urbancndep.shrubs AS shrubs
            ON shrubs.plot_id = plots_plants.plot_id
           AND shrubs.code = plots_plants.plant_id
          GROUP BY
            plots_plants.id,
            plots_plants.plants_index,
            plots_plants.direction
          HAVING COUNT(shrubs.id) <> 1
        "
      )

      expected_plots_plants <- database_scalar_count(
        db_connection,
        "SELECT COUNT(*) FROM stems_temp.plots_plants"
      )

      safe_transaction(
        db_connection = db_connection,
        action = "
          UPDATE stems_temp.plots_plants AS plots_plants
          SET shrub_id = shrubs.id
          FROM urbancndep.shrubs AS shrubs
          WHERE shrubs.plot_id = plots_plants.plot_id
            AND shrubs.code = plots_plants.plant_id
          ;
        ",
        expected_rows = expected_plots_plants,
        action_name = "map plots_plants shrub IDs"
      )

      old_stem_match_query <- glue::glue_sql(
        "
        SELECT
          plots_plants.id AS submission_id,
          plots_plants.plants_index,
          plots_plants.direction,
          COUNT(stems.id) AS match_count
        FROM stems_temp.plots_plants AS plots_plants
        LEFT JOIN urbancndep.stems AS stems
          ON stems.shrub_id = plots_plants.shrub_id
         AND stems.direction = plots_plants.direction
         AND EXTRACT(MONTH FROM stems.pre_date) IN ({pre_date_month*})
         AND EXTRACT(YEAR FROM stems.pre_date) = {pre_date_year}
        GROUP BY
          plots_plants.id,
          plots_plants.plants_index,
          plots_plants.direction
        HAVING COUNT(stems.id) <> 1
        ",
        .con = db_connection
      )

      stop_on_database_issues(
        db_connection = db_connection,
        check_name = "existing stem mapping",
        statement = old_stem_match_query
      )

      update_post_query <- glue::glue_sql(
        "
        UPDATE urbancndep.stems AS stems
        SET
          post_date = plots_plants.survey_date,
          post_note = plots_plants.post_note
        FROM stems_temp.plots_plants AS plots_plants
        WHERE plots_plants.shrub_id = stems.shrub_id
          AND plots_plants.direction = stems.direction
          AND EXTRACT(MONTH FROM stems.pre_date) IN ({pre_date_month*})
          AND EXTRACT(YEAR FROM stems.pre_date) = {pre_date_year}
        ;
        ",
        .con = db_connection
      )

      safe_transaction(
        db_connection = db_connection,
        action = update_post_query,
        expected_rows = expected_plots_plants,
        action_name = "update existing stems with post data"
      )

      stop_on_database_issues(
        db_connection = db_connection,
        check_name = "new stem collision",
        statement = "
          SELECT
            plots_plants.shrub_id,
            plots_plants.direction,
            plots_plants.survey_date,
            COUNT(stems.id) AS existing_rows
          FROM stems_temp.plots_plants AS plots_plants
          JOIN urbancndep.stems AS stems
            ON stems.shrub_id = plots_plants.shrub_id
           AND stems.direction = plots_plants.direction
           AND stems.pre_date = plots_plants.survey_date
          GROUP BY
            plots_plants.shrub_id,
            plots_plants.direction,
            plots_plants.survey_date
        "
      )

      safe_transaction(
        db_connection = db_connection,
        action = "
          INSERT INTO urbancndep.stems
          (
            shrub_id,
            direction,
            pre_date
          )
          SELECT
            plots_plants.shrub_id,
            plots_plants.direction,
            plots_plants.survey_date
          FROM stems_temp.plots_plants AS plots_plants
          ;
        ",
        expected_rows = expected_plots_plants,
        action_name = "insert new stems"
      )

      safe_transaction(
        db_connection = db_connection,
        action = "
          UPDATE stems_temp.plots_plants AS plots_plants
          SET stem_id_new = stems.id
          FROM urbancndep.stems AS stems
          WHERE stems.shrub_id = plots_plants.shrub_id
            AND stems.direction = plots_plants.direction
            AND stems.pre_date = plots_plants.survey_date
          ;
        ",
        expected_rows = expected_plots_plants,
        action_name = "map new stem IDs"
      )

      safe_transaction(
        db_connection = db_connection,
        action = "
          UPDATE stems_temp.plots_plants AS plots_plants
          SET stem_id_old = stems.id
          FROM urbancndep.stems AS stems
          WHERE stems.shrub_id = plots_plants.shrub_id
            AND stems.direction = plots_plants.direction
            AND stems.post_date = plots_plants.survey_date
          ;
        ",
        expected_rows = expected_plots_plants,
        action_name = "map existing stem IDs"
      )

      stop_on_database_issues(
        db_connection = db_connection,
        check_name = "old measurement lineage mapping",
        statement = "
          SELECT
            old.submission_id,
            old.plants_index,
            old.stem_index,
            COUNT(plots_plants.stem_id_old) AS match_count
          FROM stems_temp.old AS old
          LEFT JOIN stems_temp.plots_plants AS plots_plants
            ON plots_plants.id = old.submission_id
           AND plots_plants.plants_index = old.plants_index
           AND SUBSTRING(plots_plants.direction, 1, 1) = old.old_direction
          GROUP BY old.submission_id, old.plants_index, old.stem_index
          HAVING COUNT(plots_plants.stem_id_old) <> 1
        "
      )

      stop_on_database_issues(
        db_connection = db_connection,
        check_name = "new measurement lineage mapping",
        statement = "
          SELECT
            new.submission_id,
            new.plants_index,
            new.stem_index,
            COUNT(plots_plants.stem_id_new) AS match_count
          FROM stems_temp.new AS new
          LEFT JOIN stems_temp.plots_plants AS plots_plants
            ON plots_plants.id = new.mapping_submission_id
           AND plots_plants.plants_index = new.mapping_plants_index
           AND SUBSTRING(plots_plants.direction, 1, 1) = new.new_direction
          GROUP BY new.submission_id, new.plants_index, new.stem_index
          HAVING COUNT(plots_plants.stem_id_new) <> 1
        "
      )

      expected_old_rows <- database_scalar_count(
        db_connection,
        "SELECT COUNT(*) FROM stems_temp.old"
      )
      expected_new_rows <- database_scalar_count(
        db_connection,
        "SELECT COUNT(*) FROM stems_temp.new"
      )

      safe_transaction(
        db_connection = db_connection,
        action = "
          UPDATE stems_temp.old AS old
          SET stem_id = plots_plants.stem_id_old
          FROM stems_temp.plots_plants AS plots_plants
          WHERE plots_plants.id = old.submission_id
            AND plots_plants.plants_index = old.plants_index
            AND SUBSTRING(plots_plants.direction, 1, 1) = old.old_direction
          ;
        ",
        expected_rows = expected_old_rows,
        action_name = "map old measurement stem IDs"
      )

      safe_transaction(
        db_connection = db_connection,
        action = "
          UPDATE stems_temp.new AS new
          SET stem_id = plots_plants.stem_id_new
          FROM stems_temp.plots_plants AS plots_plants
          WHERE plots_plants.id = new.mapping_submission_id
            AND plots_plants.plants_index = new.mapping_plants_index
            AND SUBSTRING(plots_plants.direction, 1, 1) = new.new_direction
          ;
        ",
        expected_rows = expected_new_rows,
        action_name = "map new measurement stem IDs"
      )

      expected_old_lengths <- database_scalar_count(
        db_connection,
        "
        SELECT COUNT(*)
        FROM stems_temp.old AS old
        WHERE old.old_length IS NOT NULL
        "
      )
      expected_new_lengths <- database_scalar_count(
        db_connection,
        "
        SELECT COUNT(*)
        FROM stems_temp.new AS new
        WHERE new.new_length IS NOT NULL
        "
      )

      safe_transaction(
        db_connection = db_connection,
        action = "
          INSERT INTO urbancndep.stem_lengths
          (
            stem_id,
            length_in_mm,
            post_measurement
          )
          SELECT
            old.stem_id,
            old.old_length::double precision,
            TRUE
          FROM stems_temp.old AS old
          WHERE old.old_length IS NOT NULL
          ;
        ",
        expected_rows = expected_old_lengths,
        action_name = "insert old stem lengths"
      )

      safe_transaction(
        db_connection = db_connection,
        action = "
          INSERT INTO urbancndep.stem_lengths
          (
            stem_id,
            length_in_mm,
            post_measurement
          )
          SELECT
            new.stem_id,
            new.new_length::double precision,
            FALSE
          FROM stems_temp.new AS new
          WHERE new.new_length IS NOT NULL
          ;
        ",
        expected_rows = expected_new_lengths,
        action_name = "insert new stem lengths"
      )

      stop_on_database_issues(
        db_connection = db_connection,
        check_name = "missing new stem mapping",
        statement = "
          SELECT
            new_stems_missing.submission_id,
            new_stems_missing.plants_index,
            new_stems_missing.new_direction,
            COUNT(plots_plants.stem_id_new) AS match_count
          FROM stems_temp.new_stems_missing AS new_stems_missing
          LEFT JOIN stems_temp.plots_plants AS plots_plants
            ON plots_plants.id = new_stems_missing.mapping_submission_id
           AND plots_plants.plants_index = new_stems_missing.mapping_plants_index
           AND SUBSTRING(plots_plants.direction, 1, 1) =
               new_stems_missing.new_direction
          GROUP BY
            new_stems_missing.submission_id,
            new_stems_missing.plants_index,
            new_stems_missing.new_direction
          HAVING COUNT(plots_plants.stem_id_new) <> 1
        "
      )

      expected_missing_lengths <- database_scalar_count(
        db_connection,
        "SELECT COUNT(*) FROM stems_temp.new_stems_missing"
      )

      safe_transaction(
        db_connection = db_connection,
        action = "
          INSERT INTO urbancndep.stem_lengths
          (
            stem_id,
            length_in_mm,
            post_measurement
          )
          SELECT
            plots_plants.stem_id_new,
            NULL,
            FALSE
          FROM stems_temp.new_stems_missing AS new_stems_missing
          JOIN stems_temp.plots_plants AS plots_plants
            ON plots_plants.id = new_stems_missing.mapping_submission_id
           AND plots_plants.plants_index = new_stems_missing.mapping_plants_index
           AND SUBSTRING(plots_plants.direction, 1, 1) =
               new_stems_missing.new_direction
          ;
        ",
        expected_rows = expected_missing_lengths,
        action_name = "insert documented missing new lengths"
      )

      safe_transaction(
        db_connection = db_connection,
        action = "
          UPDATE urbancndep.stems AS stems
          SET pre_note = CASE
            WHEN stems.pre_note IS NULL THEN 'missing value'
            WHEN POSITION('missing value' IN stems.pre_note) > 0
              THEN stems.pre_note
            ELSE stems.pre_note || '; missing value'
          END
          FROM stems_temp.new_stems_missing AS new_stems_missing
          JOIN stems_temp.plots_plants AS plots_plants
            ON plots_plants.id = new_stems_missing.mapping_submission_id
           AND plots_plants.plants_index = new_stems_missing.mapping_plants_index
           AND SUBSTRING(plots_plants.direction, 1, 1) =
               new_stems_missing.new_direction
          WHERE stems.id = plots_plants.stem_id_new
          ;
        ",
        expected_rows = expected_missing_lengths,
        action_name = "annotate documented missing new lengths"
      )

      stop_on_database_issues(
        db_connection = db_connection,
        check_name = "plant comment collision",
        statement = "
          WITH staged_comments AS (
            SELECT DISTINCT
              plots_plants.shrub_id,
              plots_plants.survey_date
            FROM stems_temp.plots_plants AS plots_plants
            WHERE NULLIF(BTRIM(plots_plants.note_about_plant), '') IS NOT NULL
          )
          SELECT
            staged_comments.shrub_id,
            staged_comments.survey_date
          FROM staged_comments
          JOIN urbancndep.stem_comment AS stem_comment
            ON stem_comment.shrub_id = staged_comments.shrub_id
           AND stem_comment.survey_date = staged_comments.survey_date
        "
      )

      expected_comments <- database_scalar_count(
        db_connection,
        "
        SELECT COUNT(*)
        FROM (
          SELECT
            plots_plants.shrub_id,
            plots_plants.survey_date
          FROM stems_temp.plots_plants AS plots_plants
          WHERE NULLIF(BTRIM(plots_plants.note_about_plant), '') IS NOT NULL
          GROUP BY plots_plants.shrub_id, plots_plants.survey_date
        ) AS staged_comments
        "
      )

      safe_transaction(
        db_connection = db_connection,
        action = "
          INSERT INTO urbancndep.stem_comment
          (
            shrub_id,
            survey_date,
            comment
          )
          SELECT
            plots_plants.shrub_id,
            plots_plants.survey_date,
            STRING_AGG(
              DISTINCT NULLIF(BTRIM(plots_plants.note_about_plant), ''),
              '; ' ORDER BY NULLIF(BTRIM(plots_plants.note_about_plant), '')
            )
          FROM stems_temp.plots_plants AS plots_plants
          WHERE NULLIF(BTRIM(plots_plants.note_about_plant), '') IS NOT NULL
          GROUP BY plots_plants.shrub_id, plots_plants.survey_date
          ;
        ",
        expected_rows = expected_comments,
        action_name = "insert plant comments"
      )

      stop_on_database_issues(
        db_connection = db_connection,
        check_name = "staged plot note uniqueness",
        statement = "
          SELECT
            plot_notes.plot_id,
            plot_notes.survey_date::date,
            COUNT(*) AS staged_rows
          FROM stems_temp.plot_notes AS plot_notes
          WHERE plot_notes.survey_date IS NOT NULL
            AND NULLIF(BTRIM(plot_notes.plot_notes), '') IS NOT NULL
          GROUP BY plot_notes.plot_id, plot_notes.survey_date::date
          HAVING COUNT(*) > 1
        "
      )

      stop_on_database_issues(
        db_connection = db_connection,
        check_name = "plot note collision",
        statement = "
          SELECT
            plot_notes.plot_id,
            plot_notes.survey_date::date,
            COUNT(*) AS staged_rows
          FROM stems_temp.plot_notes AS plot_notes
          JOIN urbancndep.stem_plot_notes AS stem_plot_notes
            ON stem_plot_notes.plot_id = plot_notes.plot_id
           AND stem_plot_notes.survey_date = plot_notes.survey_date::date
          WHERE NULLIF(BTRIM(plot_notes.plot_notes), '') IS NOT NULL
          GROUP BY plot_notes.plot_id, plot_notes.survey_date::date
        "
      )

      expected_plot_notes <- database_scalar_count(
        db_connection,
        "
        SELECT COUNT(*)
        FROM stems_temp.plot_notes AS plot_notes
        WHERE plot_notes.survey_date IS NOT NULL
          AND NULLIF(BTRIM(plot_notes.plot_notes), '') IS NOT NULL
        "
      )

      safe_transaction(
        db_connection = db_connection,
        action = "
          INSERT INTO urbancndep.stem_plot_notes
          (
            plot_id,
            survey_date,
            plot_notes
          )
          SELECT
            plot_notes.plot_id,
            plot_notes.survey_date::date,
            NULLIF(BTRIM(plot_notes.plot_notes), '')
          FROM stems_temp.plot_notes AS plot_notes
          WHERE plot_notes.survey_date IS NOT NULL
            AND NULLIF(BTRIM(plot_notes.plot_notes), '') IS NOT NULL
          ;
        ",
        expected_rows = expected_plot_notes,
        action_name = "insert plot notes"
      )

      stop_on_database_issues(
        db_connection = db_connection,
        check_name = "shrub dimension mapping",
        statement = "
          SELECT
            shrub_dimensions.submission_id,
            shrub_dimensions.plants_index,
            COUNT(shrubs.id) AS match_count
          FROM stems_temp.shrub_dimensions AS shrub_dimensions
          LEFT JOIN urbancndep.shrubs AS shrubs
            ON shrubs.plot_id = shrub_dimensions.plot_id
           AND shrubs.code = shrub_dimensions.plant_id
          GROUP BY
            shrub_dimensions.submission_id,
            shrub_dimensions.plants_index
          HAVING COUNT(shrubs.id) <> 1
        "
      )

      expected_dimensions <- database_scalar_count(
        db_connection,
        "SELECT COUNT(*) FROM stems_temp.shrub_dimensions"
      )

      safe_transaction(
        db_connection = db_connection,
        action = "
          UPDATE stems_temp.shrub_dimensions AS shrub_dimensions
          SET shrub_id = shrubs.id
          FROM urbancndep.shrubs AS shrubs
          WHERE shrubs.plot_id = shrub_dimensions.plot_id
            AND shrubs.code = shrub_dimensions.plant_id
          ;
        ",
        expected_rows = expected_dimensions,
        action_name = "map shrub dimension IDs"
      )

      safe_transaction(
        db_connection = db_connection,
        action = "
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
          SELECT
            shrub_dimensions.plot_id,
            shrub_dimensions.survey_date::date,
            'Larrea tridentata',
            shrub_dimensions.n_s,
            shrub_dimensions.e_w,
            shrub_dimensions.height_of_plant,
            shrub_dimensions.note_about_plant,
            shrub_dimensions.shrub_id
          FROM stems_temp.shrub_dimensions AS shrub_dimensions
          ;
        ",
        expected_rows = expected_dimensions,
        action_name = "insert shrub measurements"
      )

      stop_on_database_issues(
        db_connection = db_connection,
        check_name = "final staging key completeness",
        statement = "
          SELECT
            'plots_plants'::text AS staging_table,
            COUNT(*) AS issue_rows
          FROM stems_temp.plots_plants AS plots_plants
          WHERE plots_plants.shrub_id IS NULL
             OR plots_plants.stem_id_new IS NULL
             OR plots_plants.stem_id_old IS NULL
          HAVING COUNT(*) > 0

          UNION ALL

          SELECT
            'old',
            COUNT(*)
          FROM stems_temp.old AS old
          WHERE old.stem_id IS NULL
          HAVING COUNT(*) > 0

          UNION ALL

          SELECT
            'new',
            COUNT(*)
          FROM stems_temp.new AS new
          WHERE new.stem_id IS NULL
          HAVING COUNT(*) > 0

          UNION ALL

          SELECT
            'shrub_dimensions',
            COUNT(*)
          FROM stems_temp.shrub_dimensions AS shrub_dimensions
          WHERE shrub_dimensions.shrub_id IS NULL
          HAVING COUNT(*) > 0
        "
      )

      survey_bounds <- DBI::dbGetQuery(
        db_connection,
        "
        SELECT
          MIN(plots_plants.survey_date) AS survey_start,
          MAX(plots_plants.survey_date) AS survey_end
        FROM stems_temp.plots_plants AS plots_plants
        "
      )

      row_counts <- base::list(
        stems = expected_plots_plants,
        old_lengths = expected_old_lengths,
        new_lengths = expected_new_lengths,
        missing_new_lengths = expected_missing_lengths,
        plant_comments = expected_comments,
        plot_notes = expected_plot_notes,
        shrub_measurements = expected_dimensions
      )

      upload_log_query <- glue::glue_sql(
        "
        INSERT INTO urbancndep.stems_upload_log
        (
          source_file,
          source_md5,
          survey_start,
          survey_end,
          row_counts
        )
        VALUES
        (
          {source_file},
          {source_md5},
          {survey_bounds$survey_start[[1]]},
          {survey_bounds$survey_end[[1]]},
          jsonb_build_object(
            'stems', {row_counts$stems},
            'old_lengths', {row_counts$old_lengths},
            'new_lengths', {row_counts$new_lengths},
            'missing_new_lengths', {row_counts$missing_new_lengths},
            'plant_comments', {row_counts$plant_comments},
            'plot_notes', {row_counts$plot_notes},
            'shrub_measurements', {row_counts$shrub_measurements}
          )
        )
        ;
        ",
        .con = db_connection
      )

      safe_transaction(
        db_connection = db_connection,
        action = upload_log_query,
        expected_rows = 1,
        action_name = "record completed stems upload"
      )

      row_counts
    }
  )

  base::message("stems upload committed successfully")
  base::invisible(upload_stats)

}

#' @keywords internal
#'
database_scalar_count <- function(db_connection, statement) {

  result <- DBI::dbGetQuery(db_connection, statement)

  if (base::nrow(result) != 1 || base::ncol(result) != 1) {
    base::stop("count query did not return one scalar value", call. = FALSE)
  }

  base::as.integer(result[[1]][[1]])

}

#' @keywords internal
#'
stop_on_database_issues <- function(
  db_connection,
  check_name,
  statement,
  display_rows = 20
) {

  issues <- DBI::dbGetQuery(db_connection, statement)

  base::cat(
    "\n=== Database preflight: ",
    check_name,
    " ===\n",
    "RESULT: ",
    if (base::nrow(issues) == 0) "PASSED" else "FAILED",
    "\n",
    sep = ""
  )

  if (base::nrow(issues) > 0) {
    base::print(utils::head(issues, display_rows))
  }

  base::cat("=== End database preflight ===\n")

  if (base::nrow(issues) > 0) {
    base::stop(check_name, " failed", call. = FALSE)
  }

  base::invisible(TRUE)

}
