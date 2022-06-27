# README -----------------------------------------------------------------------

# Workflow to extract stem-length data collected using ODK cndep stems app.
# This workflow employs the ODK Briefcase tool to extract data from XML files,
# and convert to tabular form.


# libraries ---------------------------------------------------------------

library(tidyverse)
library(stringr)


# data import -------------------------------------------------------------

plots <- read_csv("cndep_stems.csv")
plants <- read_csv("cndep_stems-shrub.csv")
new <- read_csv("cndep_stems-new_stems.csv")
old <- read_csv("cndep_stems-old_stems.csv")


# data manipulation (R) ---------------------------------------------------

# ISO date

plots <- plots %>%
  mutate(survey_date = as.Date(survey_date, format = "%b %e, %Y"))
  # mutate(survey_date = as.POSIXct(survey_date, format = "%b %e, %Y"))

# first change name of 'KEY' column in plants to avoid confusion later
plants <- plants %>%
  rename(superkey = KEY)

# merge plots and plants data from tablets
plotsplants <- inner_join(plots, plants, by = c("KEY" = "PARENT_KEY"))

# add cardinal directions. coming off the tablet, there is a single record for
# each plot*date*plant combination where comments such as whether all stems were
# found are in that row - the purpose of the code below (and following) is to
# generate a data frame where there is a record for each
# plot*date*plant*direction combination
direction <- c("North", "South", "West", "East")
direction <- data.frame(direction, stringsAsFactors = F)
plotsplants <- merge(plotsplants, direction, all = T)

# merge notes about missing stems into a single column
plotsplants <- plotsplants %>%
  mutate(`old-old_stems_missing-north_missing` = replace(`old-old_stems_missing-north_missing`, direction != "North", "")) %>%
  mutate(`old-old_stems_missing-south_missing` = replace(`old-old_stems_missing-south_missing`, direction != "South", "")) %>%
  mutate(`old-old_stems_missing-west_missing` = replace(`old-old_stems_missing-west_missing`, direction != "West", "")) %>%
  mutate(`old-old_stems_missing-east_missing` = replace(`old-old_stems_missing-east_missing`, direction != "East", "")) %>%
  mutate(postnote = paste(`old-old_stems_missing-north_missing`,
                          `old-old_stems_missing-south_missing`,
                          `old-old_stems_missing-west_missing`,
                          `old-old_stems_missing-east_missing`)) %>%
  mutate(postnote = replace(postnote, postnote == "", NA)) %>%
  mutate(postnote = str_trim(postnote, side = "both")) %>%
  mutate(postnote = replace(postnote, postnote == "NA", NA)) %>%
  mutate(dir = str_extract(direction, "^\\w{1}")) %>%
  select("plot_id",
         "plot_notes",
         "survey_date",
         "plant_id",
         "plant_note",
         "KEY",
         "superkey",
         "dir",
         "direction",
         "postnote")

# tibble of distinct plot notes (new as of 2018-10-22)
plotnotes <- plotsplants %>%
  filter(!is.na(plot_notes)) %>%
  distinct(plot_id, plot_notes, survey_date)
  
  
# prelimary error checking ------------------------------------------------

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
    ) %>%
  print(n = 400)

# should be n sites * 4 plots plots, and count for each should be 20
plotsplants %>%
  group_by(plot_id) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(plot_id) %>%
  print(n = 400)

plotsplants %>%
  group_by(plot_id, plant_id, direction) %>%
  summarise(count = n()) %>%
  print(n = 800)

# generate easier-to-view data (to facilitate error checking)

oldLengths <- inner_join(plots, plants, by = c("KEY" = "PARENT_KEY")) %>%
  inner_join(old, by = c("superkey" = "PARENT_KEY")) %>%
  select(site,
         plot_id,
         survey_date,
         plant_id,
         plant_note,
         old_direction,
         oldLength,
         note_north = `old-old_stems_missing-north_missing`,
         note_south = `old-old_stems_missing-south_missing`,
         note_west = `old-old_stems_missing-west_missing`,
         note_east = `old-old_stems_missing-east_missing`) %>%
  arrange(plot_id, plant_id, old_direction)

newLengths <- inner_join(plots, plants, by = c("KEY" = "PARENT_KEY")) %>%
  inner_join(new, by = c("superkey" = "PARENT_KEY")) %>%
  select(site,
         plot_id,
         survey_date,
         plant_id,
         KEY = KEY.y,
         plant_note,
         new_direction,
         newLength) %>%
  arrange(plot_id, plant_id, new_direction)

# Any entries with a count less than four suggest missing data for a given
# direction. Run the following checks to see if there is a problem and possible
# fix with the raw data but see the section "address missing NEW values" about
# how to address situations where there simply are no new measurements for a
# particular direction. Addressing this includes specifying that values for the
# missing directions are NULL and adding a comment to the stems comment table.

newLengths %>%
  group_by(plot_id, plant_id) %>%
  summarise(count = n_distinct(new_direction)) %>%
  filter(count < 4)

newLengths %>%
  group_by(site, plot_id) %>%
  summarize(count = (n_distinct(plant_id))) %>%
  filter(count < 5)

# looking for new-length NAs
newLengths %>%
  filter(is.na(newLength))

# looking for old-length NAs
# really nothing to be done with these
oldLengths %>%
  filter(is.na(oldLength))

# proceed to populate_database.R
