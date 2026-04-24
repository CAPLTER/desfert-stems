#' Build and format plots-plants records
#'
#' Helper function to join KoBo plot and plant sheets, normalize plant/plot note
#' text, and select fields used by downstream DesFert stems workflows.

build_plots_plants <- function(
  plots_data,
  plants_data
) {
  dplyr::left_join(
    x = plots_data |> dplyr::rename(plots_index = index),
    y = plants_data |> dplyr::rename(plants_index = index),
    by = c("uuid" = "submission_uuid")
  ) |>
    dplyr::mutate(
      note_about_plot = gsub(",", " ", note_about_plot),
      note_about_plot = gsub("[\n\r]", " ", note_about_plot),
      note_about_plot = stringr::str_trim(note_about_plot, side = c("both")),
      note_about_plant = gsub(",", " ", note_about_plant),
      note_about_plant = gsub("[\n\r]", " ", note_about_plant),
      note_about_plant = stringr::str_trim(note_about_plant, side = c("both"))
    ) |>
    dplyr::select(
      survey_date = today,
      plot_id,
      id,
      uuid,
      plant_id,
      note_about_plant,
      plots_index,
      plants_index,
      dplyr::contains(c("width", "height"))
    )
}
