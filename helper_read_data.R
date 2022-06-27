#' @title helper: read and format stems data collected with KoBo
#'
#' @description Two related functions read_kobo_stems and format_note_field
#' read stems data from the Excel file downloaded from KoBoConnect. Data are
#' formatted (names run through Janitor) and all notes fields are scrubbed for
#' commas and carriage returns.
#'
#' @export
#'
format_note_field <- function(field) {

  field <- gsub("[\n\r]", " ", field)
  field <- stringr::str_trim(field, side = c("both"))

  return(field)

}

#' @export

read_kobo_stems <- function(path_to_file, worksheet) {

  formatted_data <- readxl::read_excel(
    path  = path_to_file,
    sheet = worksheet
    ) |>
  janitor::clean_names() |>
  dplyr::mutate(
    dplyr::across(tidyselect::contains("note"), ~ format_note_field(.x))
  )

  return(formatted_data)

}
