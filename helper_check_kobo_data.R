#' @title helper: check KoBo stems workflow keys
#'
#' @description Validate key uniqueness assumptions used by downstream upload
#' queries. These checks are intended to fail early in the local KoBo workflow,
#' before duplicate staging rows can create ambiguous PostgreSQL updates.
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

  base::cat(
    "\n=== KoBo key uniqueness check: ",
    check_name,
    " ===\n",
    sep = ""
  )

  if (base::nrow(duplicate_keys) > 0) {

    duplicate_records <- plots_plants_data |>
      dplyr::semi_join(
        duplicate_keys |> dplyr::select(tidyselect::all_of(keys)),
        by = keys
      ) |>
      dplyr::arrange(dplyr::across(tidyselect::all_of(keys)))

    base::cat(
      "RESULT: FAILED\n",
      "Duplicate records:\n",
      sep = ""
    )
    base::print(duplicate_records)

    error_message <- base::paste0(
      check_name,
      " found duplicate rows; resolve duplicated KoBo records before upload"
    )

    base::cat(
      error_message,
      "\nWORKFLOW: ",
      if (stop_on_error) "STOPPING" else "CONTINUING",
      "\n=== End KoBo key uniqueness check ===\n",
      sep = ""
    )

    if (stop_on_error) {
      base::stop(error_message, call. = FALSE)
    }
  } else {
    base::cat(
      "RESULT: PASSED\n",
      "No duplicate key combinations found.\n",
      "=== End KoBo key uniqueness check ===\n",
      sep = ""
    )
  }

  base::invisible(plots_plants_data)

}
