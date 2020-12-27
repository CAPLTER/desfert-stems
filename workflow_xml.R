# README -----------------------------------------------------------------------

# Workflow to extract stem-length data collected using ODK cndep stems app.
# This is a "manual" workflow to extract data from ODK XML files into tabular
# form designed to mimic the functionality provided by ODK Briefcase.


# LIBRARIES --------------------------------------------------------------------

library(xml2)
library(tibble)
library(tidyr)
library(purrr)
library(dplyr)


# REQUIREMENTS -----------------------------------------------------------------

source("./helper_extract_lengths.R")
source("./helper_extract_plot_notes.R")
source("./helper_extract_plots_plants.R")


# XML TO TABULAR ---------------------------------------------------------------

# identify xml files from field tablets

alpha <- list.files(
  path = "~/Desktop/stems_fall/alpha/instances",
  pattern = "xml",
  full.names = TRUE,
  recursive = TRUE
)

delta <- list.files(
  path = "~/Desktop/stems_fall/delta/instances",
  pattern = "xml",
  full.names = TRUE,
  recursive = TRUE
)

# extract xml data and convert to tabular

old <- bind_rows(
  map_df(alpha, extract_old_lengths),
  map_df(delta, extract_old_lengths)
)

new <- bind_rows(
  map_df(alpha, extract_new_lengths),
  map_df(delta, extract_new_lengths)
)

plotnotes <- bind_rows(
  map_df(alpha, extract_plot_notes),
  map_df(delta, extract_plot_notes)
)

plotsplants <- bind_rows(
  map_df(alpha, extract_plots_plants),
  map_df(delta, extract_plots_plants)
)


# ERROR CHECKING ---------------------------------------------------------------

# should be n sites * 16 per plant ID
plotsplants %>%
  count(plant_id)

# should be n sites * 4 plots * 5 plants, and each count should be 4
plotsplants %>%
  count(plot_id, plant_id) %>%
  ungroup() %>%
  arrange(
    desc(n),
    plot_id,
    plant_id
  )

# should be n sites * 4 plots plots, and count for each should be 20
plotsplants %>%
  group_by(plot_id) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))

# Any entries with a count less than four suggest missing data for a given
# direction. Run the following checks to see if there is a problem and possible
# fix with the raw data but see the section "address missing NEW values" about
# how to address situations where there simply are no new measurements for a
# particular direction. Addressing this includes specifying that values for the
# missing directions are NULL and adding a comment to the stems comment table.

new %>%
  group_by(plot_id, plant_id) %>%
  summarise(count = n_distinct(new_direction)) %>%
  filter(count < 4)

new %>%
  group_by(site, plot_id) %>%
  summarize(count = (n_distinct(plant_id))) %>%
  filter(count < 5)

# looking for new-length NAs
new %>%
  filter(is.na(newLength))

# looking for old-length NAs
# really nothing to be done with these
old %>%
  filter(is.na(oldLength))


# SURVEY METADATA --------------------------------------------------------------

# access additional metadata

tablet_file <- read_xml(".xml")

xml_text(xml_find_all(tablet_file, "start"))
xml_text(xml_find_all(tablet_file, "end"))
xml_text(xml_find_all(tablet_file, "today"))
xml_text(xml_find_all(tablet_file, "site"))
xml_integer(xml_find_all(tablet_file, "plot_id"))
xml_text(xml_find_all(tablet_file, "treatment"))
xml_text(xml_find_all(tablet_file, "survey_date"))
xml_text(xml_find_all(tablet_file, "plot_notes"))


# SCRATCH ----------------------------------------------------------------------

extract_new_lengths(tablet_file)
extract_old_lengths(tablet_file)
extract_plot_notes(tablet_file)
extract_plots_plants(tablet_file)
