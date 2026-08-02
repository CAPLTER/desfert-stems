#' @title helper: check KoBo stems workflow keys
#'
#' @description Validate assumptions used by the KoBo workflow and render each
#' result as a clearly delineated console block. Intermediate checks can report
#' problems without stopping; a final collection of checks can stop execution.
#'
#' @param plots_plants_data A data frame containing the KoBo records to check.
#' @param keys A character vector of columns that should uniquely identify rows.
#' @param check_name A label identifying the check in diagnostic messages.
#' @param stop_on_error Logical; whether duplicate keys should stop the workflow.
#'
#' @export
#'
check_kobo_key_uniqueness <- function(
  plots_plants_data,
  keys,
  check_name,
  stop_on_error = TRUE
) {

  if (
    !base::is.logical(stop_on_error) ||
      base::length(stop_on_error) != 1 ||
      base::is.na(stop_on_error)
  ) {
    base::stop("stop_on_error must be TRUE or FALSE", call. = FALSE)
  }

  duplicate_keys <- plots_plants_data |>
    dplyr::count(
      dplyr::across(tidyselect::all_of(keys)),
      name = "duplicate_rows"
    ) |>
    dplyr::filter(duplicate_rows > 1)

  if (base::nrow(duplicate_keys) > 0) {

    duplicate_records <- plots_plants_data |>
      dplyr::semi_join(
        duplicate_keys |> dplyr::select(tidyselect::all_of(keys)),
        by = keys
      ) |>
      dplyr::arrange(dplyr::across(tidyselect::all_of(keys)))

  } else {
    duplicate_records <- plots_plants_data[0, , drop = FALSE]
  }

  check_passed <- report_kobo_check(
    check_name    = check_name,
    issue_records = duplicate_records,
    pass_message  = "No duplicate key combinations found.",
    fail_message  = "Duplicate key combinations found.",
    stop_on_error = stop_on_error
  )

  base::invisible(check_passed)

}

#' @rdname check_kobo_key_uniqueness
#'
#' @param required_columns Columns that must not contain missing or blank values.
#'
check_kobo_required_values <- function(
  plots_plants_data,
  required_columns,
  check_name,
  stop_on_error = TRUE
) {

  missing_columns <- base::setdiff(
    required_columns,
    base::names(plots_plants_data)
  )

  if (base::length(missing_columns) > 0) {
    base::stop(
      "required columns are absent: ",
      base::paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  required_value_is_missing <- function(column) {
    if (base::is.character(column)) {
      base::is.na(column) | stringr::str_trim(column) == ""
    } else {
      base::is.na(column)
    }
  }

  missing_records <- plots_plants_data |>
    dplyr::filter(
      dplyr::if_any(
        tidyselect::all_of(required_columns),
        required_value_is_missing
      )
    )

  check_passed <- report_kobo_check(
    check_name    = check_name,
    issue_records = missing_records,
    pass_message  = "All required values are present.",
    fail_message  = "Required values are missing or blank.",
    stop_on_error = stop_on_error
  )

  base::invisible(check_passed)

}

#' @rdname check_kobo_key_uniqueness
#'
#' @param group_keys Columns identifying one plant measurement event.
#' @param direction_column Column containing cardinal directions.
#' @param expected_directions Character vector of required directions.
#'
check_kobo_direction_coverage <- function(
  plots_plants_data,
  group_keys,
  direction_column = "direction",
  expected_directions = c("East", "North", "South", "West"),
  check_name,
  stop_on_error = TRUE
) {

  expected_directions <- base::sort(expected_directions)

  direction_issues <- plots_plants_data |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_keys))) |>
    dplyr::summarise(
      observed_directions = base::list(
        base::sort(base::unique(.data[[direction_column]]))
      ),
      .groups = "drop"
    ) |>
    dplyr::filter(
      !purrr::map_lgl(
        observed_directions,
        ~ base::identical(.x, expected_directions)
      )
    )

  check_passed <- report_kobo_check(
    check_name    = check_name,
    issue_records = direction_issues,
    pass_message  = "Every plant has the expected cardinal directions.",
    fail_message  = "Plant records have missing or unexpected directions.",
    stop_on_error = stop_on_error
  )

  base::invisible(check_passed)

}

#' @rdname check_kobo_key_uniqueness
#'
#' @param expected_rows Expected row count.
#' @param observed_rows Observed row count.
#'
check_kobo_row_count <- function(
  expected_rows,
  observed_rows,
  check_name,
  stop_on_error = TRUE
) {

  if (base::identical(base::as.integer(expected_rows), base::as.integer(observed_rows))) {
    issue_records <- tibble::tibble()
  } else {
    issue_records <- tibble::tibble(
      expected_rows = expected_rows,
      observed_rows = observed_rows
    )
  }

  check_passed <- report_kobo_check(
    check_name    = check_name,
    issue_records = issue_records,
    pass_message  = "The transformation preserved the expected row count.",
    fail_message  = "The transformation changed the expected row count.",
    stop_on_error = stop_on_error
  )

  base::invisible(check_passed)

}

#' @rdname check_kobo_key_uniqueness
#'
#' @param check_results Named logical vector returned by KoBo checks.
#'
stop_on_failed_kobo_checks <- function(check_results) {

  failed_checks <- base::names(check_results)[!check_results]

  if (base::length(failed_checks) > 0) {
    base::stop(
      "final KoBo validation failed: ",
      base::paste(failed_checks, collapse = ", "),
      call. = FALSE
    )
  }

  base::cat("\nFinal KoBo validation passed.\n")
  base::invisible(TRUE)

}

#' @keywords internal
#'
report_kobo_check <- function(
  check_name,
  issue_records,
  pass_message,
  fail_message,
  stop_on_error
) {

  check_passed <- base::nrow(issue_records) == 0

  base::cat(
    "\n=== KoBo workflow check: ",
    check_name,
    " ===\n",
    "RESULT: ",
    if (check_passed) "PASSED" else "FAILED",
    "\n",
    if (check_passed) pass_message else fail_message,
    "\n",
    sep = ""
  )

  if (!check_passed) {
    base::cat("Issue records:\n")
    base::print(issue_records)
    base::cat(
      "WORKFLOW: ",
      if (stop_on_error) "STOPPING" else "CONTINUING",
      "\n",
      sep = ""
    )
  }

  base::cat("=== End KoBo workflow check ===\n")

  if (!check_passed && stop_on_error) {
    base::stop(check_name, " failed", call. = FALSE)
  }

  base::invisible(check_passed)

}
