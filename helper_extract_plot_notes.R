# README -----------------------------------------------------------------------

# extract_plot_notes is a helper function to extract plot notes in the workflow
# of extracting and converting to tabular form stem-length data collected with
# the ODK app.


# LIBRARIES --------------------------------------------------------------------

library(xml2)
library(tibble)
library(tidyr)
library(purrr)
library(dplyr)


# PLOT NOTES -------------------------------------------------------------------

extract_plot_notes <- function(xml_file) {

  xml_file <- xml2::read_xml(xml_file)

  notes <- tibble(
    site = xml2::xml_text(xml2::xml_find_all(xml_file, "site")),
    plot_id = xml2::xml_integer(xml2::xml_find_all(xml_file, "plot_id")),
    survey_date = xml2::xml_text(xml2::xml_find_all(xml_file, "survey_date")),
    plot_notes = xml2::xml_text(xml2::xml_find_all(xml_file, "plot_notes"))
    ) %>%
  mutate(
    plot_notes = case_when(
      plot_notes == "" ~ NA_character_,
      TRUE ~ plot_notes
    )
    ) %>%
  filter(!is.na(plot_notes))

return(notes)

}
