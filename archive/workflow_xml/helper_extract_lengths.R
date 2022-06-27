# README -----------------------------------------------------------------------

# extract_old_lengths and extract_new_lengths are helper functions to extract
# stem lengths in the workflow of extracting and converting to tabular form
# stem-length data collected with the ODK app.


# LIBRARIES --------------------------------------------------------------------

library(xml2)
library(tibble)
library(tidyr)
library(purrr)
library(dplyr)


# OLD LENGTHS ------------------------------------------------------------------

extract_old_lengths <- function(xml_file) {

  xml_file <- xml2::read_xml(xml_file)

  shrubs <- xml2::xml_find_all(xml_file, "shrub")
  shrubs_list <- shrubs %>%
    as_list()

  old_lengths <- tibble(nodeset = shrubs_list) %>%
    unnest_wider(nodeset) %>%
    select(plant_id, old) %>%
    unnest_wider(plant_id) %>%
    unnest_longer(old) %>%
    filter(old_id != "allStems") %>%
    select(
      plant_id = `...1`,
      old
      ) %>%
    unnest_wider(old) %>%
    unnest_longer(old_direction) %>%
    unnest_longer(oldLength) %>%
    select(
      plant_id,
      contains("old")
      ) %>%
    filter(!is.na(oldLength)) %>%
    mutate(
      site = xml2::xml_text(xml2::xml_find_all(xml_file, "site")),
      plot_id = xml2::xml_integer(xml2::xml_find_all(xml_file, "plot_id")),
      survey_date = xml2::xml_text(xml2::xml_find_all(xml_file, "survey_date")),
      survey_start = xml2::xml_text(xml2::xml_find_all(xml_file, "start")),
      survey_end = xml2::xml_text(xml2::xml_find_all(xml_file, "end")),
      instance_id = xml2::xml_text(xml2::xml_find_all(xml_file, "meta"))
    )

    return(old_lengths)

}


# NEW LENGHTS ------------------------------------------------------------------

extract_new_lengths <- function(xml_file) {

  xml_file <- xml2::read_xml(xml_file)

  shrubs <- xml2::xml_find_all(xml_file, "shrub")
  shrubs_list <- shrubs %>%
    as_list()

  new_lengths <- tibble(nodeset = shrubs_list) %>%
    unnest_wider(nodeset) %>%
    select(plant_id, new) %>%
    unnest_wider(plant_id) %>%
    unnest_longer(new) %>%
    select(
      plant_id = `...1`,
      new
      ) %>%
    unnest_wider(new) %>%
    unnest_longer(new_direction) %>%
    unnest_longer(newLength) %>%
    mutate(
      site = xml2::xml_text(xml2::xml_find_all(xml_file, "site")),
      plot_id = xml2::xml_integer(xml2::xml_find_all(xml_file, "plot_id")),
      survey_date = xml2::xml_text(xml2::xml_find_all(xml_file, "survey_date")),
      survey_start = xml2::xml_text(xml2::xml_find_all(xml_file, "start")),
      survey_end = xml2::xml_text(xml2::xml_find_all(xml_file, "end")),
      instance_id = xml2::xml_text(xml2::xml_find_all(xml_file, "meta"))
    )

    return(new_lengths)

}
