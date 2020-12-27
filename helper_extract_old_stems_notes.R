# README -----------------------------------------------------------------------

# extract_old_stems_missing is a helper function to extract notes about old
# stems (i.e., stem not found, dead on ground, etc.) from ODK XML records.


# LIBRARIES --------------------------------------------------------------------

library(xml2)
library(tibble)
library(tidyr)
library(purrr)
library(dplyr)


# extract_old_stems_missing  ---------------------------------------------------

# function to extract old stems missing notes
extract_old_stems_missing <- function(old_stems_missing_nodeset) {

  plant_id <- old_stems_missing_nodeset %>%
    xml2::xml_find_all(., "plant_id") %>%
    xml2::xml_text(.)

  north_note <- old_stems_missing_nodeset[1] %>%
    xml2::xml_find_first(., ".//north_missing") %>%
    xml2::xml_text(.)

  south_note <- old_stems_missing_nodeset[1] %>%
    xml2::xml_find_first(., ".//south_missing") %>%
    xml2::xml_text(.)

  west_note <- old_stems_missing_nodeset[1] %>%
    xml2::xml_find_first(., ".//west_missing") %>%
    xml2::xml_text(.)

  east_note <- old_stems_missing_nodeset[1] %>%
    xml2::xml_find_first(., ".//east_missing") %>%
    xml2::xml_text(.)

  plant_old_missing <- tibble(
    plant = plant_id,
    direction = c("North", "South", "West", "East"),
    postnote = c(north_note, south_note, west_note, east_note)
    ) %>%
  mutate(
    postnote = case_when(
      postnote == "" ~ NA_character_,
      TRUE ~ postnote
    )
  )

  return(plant_old_missing)

}
