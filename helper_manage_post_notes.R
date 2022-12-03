#' @title helper: harvest wide-format post notes and format to long
#'
#' @description The coalesce_old_notes function harvests post notes (i.e.,
#' notes about a particular plant, stem, or otherwise) when surveying old
#' stems; the data, coming from wide format in the Kobo export, are formatted
#' to a long, concise format.
#'
#' @note The function is specific to a single, cardinal direction, which is
#' passed as an argument to the cardinal_direction parameter. Best use of this
#' function is to wrap it in a mapping workflow to generate a frame of notes
#' for all directions.
#'
#' @export
#'
coalesce_old_notes <- function(source_data, cardinal_direction) {

  this_direction <- tolower({{ cardinal_direction }})

  notes_tibble <- source_data |>
    dplyr::filter(!is.na(!!rlang::sym(this_direction))) |>
    dplyr::mutate(direction = cardinal_direction) |>
    dplyr::select(
      index,
      direction,
      tidyselect::all_of(this_direction)
    )

    notes_tibble <- notes_tibble |>
    dplyr::rename(post_note = this_direction)

  return(notes_tibble)

}
