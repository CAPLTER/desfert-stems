# README -----------------------------------------------------------------------

# extract_plots_plants is a helper function to extract plot and plant data in
# the workflow of extracting and converting to tabular form stem-length data
# collected with the ODK app.


# LIBRARIES --------------------------------------------------------------------

library(xml2)
library(tibble)
library(tidyr)
library(purrr)
library(dplyr)


# REQUIREMENTS -----------------------------------------------------------------

source("./helper_extract_old_stems_notes.R")


# PLOTS PLANTS -----------------------------------------------------------------

# construct a matrix of cardinal directions
direction <- data.frame(
  direction = c("North", "South", "West", "East"),
  dir = c("N", "S", "W", "E"),
  stringsAsFactors = F
)


extract_plots_plants <- function(xml_file) {

  xml_file <- xml2::read_xml(xml_file)

  # subroutine to extract old stem notes if they exist
  nodeset_missing <- xml2::xml_find_all(
    x = xml_file,
    xpath = ".//old_stems_missing"
    ) %>%
  map(~ xml2::xml_parents(.))

# pass to extract_old_stems_missing function
old_stems_missing_notes <- map_df(
  .x = nodeset_missing,
  .f = extract_old_stems_missing
)

if (nrow(old_stems_missing_notes) == 0) {

  old_stems_missing_notes <- tibble::tibble(
    plant = NA_character_,
    direction = NA_character_
  )

}

# main
shrubs <- xml2::xml_find_all(xml_file, "shrub")

shrubs_list <- shrubs %>%
  as_list()

plot_plants <- tibble::tibble(nodeset = shrubs_list) %>%
  unnest_wider(nodeset)

# not all plants have notes - adjust unnessting accordingly
if ("plant_note" %in% colnames(plot_plants)) {

  plot_plants <- plot_plants |>
    dplyr::select(plant_id, plant_note) |>
    tidyr::unnest_wider(plant_id) |>
    tidyr::unnest_longer(plant_note) |>
    dplyr::mutate(
      plant_note = gsub("[\n\r]", " ", plant_note),
      plant_note = stringr::str_trim(plant_note, side = c("both"))
    )

} else {

  plot_plants <- plot_plants %>%
    select(plant_id) %>%
    unnest_wider(plant_id)

}

# construct plots_plants
plot_plants <- plot_plants %>%
  merge(direction, all = TRUE) %>%
  rename(plant = contains("1")) %>%
  left_join(
    old_stems_missing_notes,
    by = c("plant", "direction")
    ) %>%
  mutate(
    site = xml2::xml_text(xml2::xml_find_all(xml_file, "site")),
    plot_id = xml2::xml_integer(xml2::xml_find_all(xml_file, "plot_id")),
    survey_date = xml2::xml_text(xml2::xml_find_all(xml_file, "survey_date")),
    survey_start = xml2::xml_text(xml2::xml_find_all(xml_file, "start")),
    survey_end = xml2::xml_text(xml2::xml_find_all(xml_file, "end")),
    instance_id = xml2::xml_text(xml2::xml_find_all(xml_file, "meta"))
    ) %>%
  rename(plant_id = plant)

# return plot_plants
return(plot_plants)

}
