#' @title helper: identify stems for which new values are missing
#'
#' @description Failing to collect and/or misidentifying a plant or stem
#' direction such that a plot*plant*direction ends up not having a new (i.e.,
#' pre-date) measurement is an all too common occurence. These data are
#' generally not recoverable but it is important to note in the database that
#' the data were not collected. \code{identify_new_missing} is a function to
#' identify occurences where new stem measurements are not available. This
#' information can be used in a follow-up step (\code{annotate_new_missing}) to
#' add the appropriate documentation regarding the missing new stem
#' measurement(s) to the database for that particular collection.
#'
#' @export
#'
identify_new_missing <- function(
  new_lengths_data = new
  ) {

  cardinal_directions <- tibble::tibble(direction = c("N", "S", "W", "E"))

  count_less_than_four <- new_lengths_data |>
  dplyr::group_by(
    plot_id,
    plant_id
    ) |>
  dplyr::summarise(
    count   = dplyr::n_distinct(new_direction),
    .groups = "drop"
    ) |>
  dplyr::filter(count < 4) |>
  dplyr::ungroup()

  missing_length_matrix <- purrr::pmap_dfr(
    count_less_than_four,
    ~ generate_matrix_new_null(..1, ..2, lengths_data = new_lengths_data, NSWE = cardinal_directions)
  )

  missing_length_matrix <- missing_length_matrix |>
  dplyr::filter(is.na(new_length))

  return(missing_length_matrix)

}

#'
#' @note \code{generate_matrix_new_null} is a helper function used by
#' \code{identify_new_missing} to construct a matrix of plant*stem*directions
#' for all four cardinal directions for a given plot*plant where at least one
#' stem is missing new (pre-date) data.
#'
generate_matrix_new_null <- function(
  lengths_data,
  new_null_plot,
  new_null_plant,
  NSWE
  ) {

  lengths_data |>
  dplyr::filter(
    plot_id  == new_null_plot,
    plant_id == new_null_plant
    ) |>
  dplyr::right_join(
    NSWE,
    by = c("new_direction" = "direction")
  ) |>
  dplyr::mutate(
    plot_id  = replace(plot_id, is.na(new_length), new_null_plot),
    plant_id = replace(plant_id, is.na(new_length), new_null_plant)
  )

}
