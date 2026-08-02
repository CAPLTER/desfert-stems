#' @title helper: check KoBo stems workflow keys
#'
#' @description Validate key uniqueness assumptions used by downstream upload
#' queries. These checks are intended to fail early in the local KoBo workflow,
#' before duplicate staging rows can create ambiguous PostgreSQL updates.
#'
#' @export
#'
check_kobo_key_uniqueness <- function(
  plots_plants_data,
  keys,
  check_name
) {

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

    base::print(duplicate_records)

    base::stop(
      check_name,
      " found duplicate rows; resolve duplicated KoBo records before upload",
      call. = FALSE
    )
  }

  base::invisible(plots_plants_data)

}
