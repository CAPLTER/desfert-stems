---
title: "StemMigration"
author: "SRE"
date: "04/20/2015"
output: html_document
---

library('RPostgreSQL')
library('tidyverse')

# database connections ----
source('~/Documents/localSettings/pg_prod.R')
source('~/Documents/localSettings/pg_local.R')

pg <- pg_prod
pg <- pg_local

dbGetInfo(pg)

# bring in processed files ----
# SET WORKING DIRECTORY!!!

files <- list.files(path=".", pattern="*.csv", full.names=T, recursive=FALSE) # identify files
BatchLoad <- function(file) {data <- read.csv(file, header=T, stringsAsFactors=F) } # function to get files
data <- lapply(files, BatchLoad) # import files from target directory

# get filenames of list items from directory (note full.names=F); change to lower case on import
fileNames <- tolower(list.files(path=".", pattern="*.csv", full.names=F, recursive=FALSE))

# assign simpler, more meaningful file names
fileNames[fileNames=="cndep_stems.csv"] <- 'plots'
fileNames[fileNames=="cndep_stems_new_new_stems.csv"] <- 'new'
fileNames[fileNames=="cndep_stems_old_old_stems.csv"] <- 'old'
fileNames[fileNames=="cndep_stems_shrub.csv"] <- 'plants'

# assign filenames to list
names(data) <- fileNames

# begin data manipulation ----
# ISO date
data[['plots']]$survey_date <- as.POSIXct(data[['plots']]$survey_date, format = "%b %e, %Y")

# merge plots and plants data from tablets
names(data[['plants']])[11] <- 'superkey' # first change name of 'KEY' column in plants to avoid confusion later
data[['plotsplants']] <- merge(data[['plots']], data[['plants']], by.x = 'KEY', by.y = 'PARENT_KEY', all = T, sort = F)

# merge plots & plants data from tablets with shrubs data from PG
# data[['plotsplantsShrubs']] <- merge(data[['plotsplants']], shrubs, by.x = c('plot_id', 'plant_id'), by.y = c('plot_id', 'code'), all.x = T, sort = F)

# add cardinal directions
direction <- c('North', 'South', 'West', 'East')
direction <- data.frame(direction)
data[['plotsplants']] <- merge(data[['plotsplants']], direction, all=T)

# merge notes about missing stems into a single column
data[['plotsplants']]$old.old_stems_missing.north_missing[data[['plotsplants']]$direction != 'North'] <- ''
data[['plotsplants']]$old.old_stems_missing.south_missing[data[['plotsplants']]$direction != 'South'] <- ''
data[['plotsplants']]$old.old_stems_missing.east_missing[data[['plotsplants']]$direction != 'East'] <- ''
data[['plotsplants']]$old.old_stems_missing.west_missing[data[['plotsplants']]$direction != 'West'] <- ''
data[['plotsplants']]$postnote <- paste(data[['plotsplants']]$old.old_stems_missing.north_missing,
                                        data[['plotsplants']]$old.old_stems_missing.south_missing,
                                        data[['plotsplants']]$old.old_stems_missing.east_missing,
                                        data[['plotsplants']]$old.old_stems_missing.west_missing)
data[['plotsplants']]$postnote <- gsub(" ", "", data[['plotsplants']]$postnote)
data[['plotsplants']]$postnote[data[['plotsplants']]$postnote == ""] <- NA

# make a N, S, W, E column for matching 'old'
data[['plotsplants']]$dir <- substring(data[['plotsplants']]$direction, 1, 1)

# subset relevant columns
data[['plotsplants']] <- data[['plotsplants']][c('plot_id',
                                                 'plot_notes',
                                                 'survey_date',
                                                 'plant_id',
                                                 'plant_note',
                                                 'KEY',
                                                 'superkey',
                                                 'dir',
                                                 'direction',
                                                 'postnote')]

# do some preliminary error checking ----

data[['plotsplants']] %>%
  count(plant_id)

# should be 240 obs per plant ID


data[['plotsplants']] %>%
  count(plot_id, plant_id) %>%
  ungroup() %>%
  arrange(n, plot_id, plant_id) %>%
  print(n = 400) # 400 is an arbitrary number greater than the expected return

 # should be 300 rows, and each count should be 4


data[['plotsplants']] %>%
  group_by(plot_id) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(plot_id) %>%
  print(n = 400) # 400 is an arbitrary number greater than the expected return

 # should be 60 plots
 # count for each should be 20

data[['plotsplants']] %>%
  group_by(plot_id, plant_id, direction) %>%
  summarise(count = n()) %>%
  print(n = 1200) # 400 is an arbitrary number greater than the expected return

# generate easier-to-view data
plotsShrubs <- inner_join(data[['plots']], data[['plants']], by = c('KEY' = 'PARENT_KEY'))
oldLengths <- inner_join(plotsShrubs, data[['old']], by = c('superkey' = 'PARENT_KEY')) %>%
  select(site,
         plot_id,
         survey_date,
         plant_id,
         plant_note,
         old_direction,
         oldLength,
         note_north = old.old_stems_missing.north_missing,
         note_south = old.old_stems_missing.south_missing,
         note_west = old.old_stems_missing.west_missing,
         note_east = old.old_stems_missing.east_missing) %>%
  arrange(plot_id, plant_id, old_direction)
newLengths <- inner_join(plotsShrubs, data[['new']], by = c('superkey' = 'PARENT_KEY')) %>%
  select(site, plot_id, survey_date, plant_id, plant_note, new_direction, newLength) %>%
  arrange(plot_id, plant_id, new_direction)

newLengths %>%
  group_by(plot_id, plant_id) %>%
  summarise(count = n_distinct(new_direction)) %>%
  filter(count < 4)
# Any entries with a count less than four suggest missing data for a given
# direction. This was a huge headache in fall 2010 as it was not caught until
# after adding the data to the database. To fix, add a comment to the
# stem_comment table. Then review the data carefully as you will want to run the
# insert into stem_lengths of new data without the WHERE IS NOT NULL
# clause...removing that, though, could add unwanted NULLs so, again, review the
# data carefully.

newLengths %>%
  filter(is.na(newLength))
# same as above but here looking for NAs

# add data to database ----

# create a new schema called stemp_temp in pg then loop through list items to push them to postgresql
for (i in 1:length(data)) {
if (dbExistsTable(con, c('stems_temp', names(data)[[i]]))) dbRemoveTable(con, c('stems_temp', names(data)[[i]])) # make sure tbl does not exist
dbWriteTable(con, c('stems_temp', names(data)[[i]]), value = data[[i]], row.names = F) # write temp table
}

# SQL run sequentially! ----

# prep for and add shrub_ids to the plotsplants tablet-derived data
dbGetQuery(con,'
ALTER TABLE stems_temp.plotsplants
ADD COLUMN shrub_id integer;')

dbGetQuery(con,'
UPDATE stems_temp.plotsplants
SET shrub_id = urbancndep.shrubs.id
FROM urbancndep.shrubs
WHERE
  urbancndep.shrubs.plot_id = plotsplants.plot_id AND
  urbancndep.shrubs.code = plotsplants.plant_id;')
# should execute on 1200 rows

# update pg.stems with post date & post notes be sure to set month & year to the
# correct pre data, which should be the most recent set (may include multiple
# months)
dbGetQuery(con,"
UPDATE urbancndep.stems
SET
  post_date = plotsplants.survey_date,
  post_note = plotsplants.postnote
FROM stems_temp.plotsplants
WHERE
  plotsplants.shrub_id = urbancndep.stems.shrub_id AND
  plotsplants.direction = urbancndep.stems.direction AND
  EXTRACT(MONTH from urbancndep.stems.pre_date) = __SET_PREVIOUS_PERIOD_MONTH__ AND
  EXTRACT(YEAR from urbancndep.stems.pre_date) = __SET_PREVIOUS_PERIOD_YEAR;") # set appropriatly !

  # EXTRACT(MONTH from urbancndep.stems.pre_date) = __SET_PREVIOUS_PERIOD_MONTH__ AND
  # EXTRACT(YEAR from urbancndep.stems.pre_date) = __SET_PREVIOUS_PERIOD_YEAR;") # set appropriatly !

# insert pre data into pg.stems
dbGetQuery(con,"
INSERT INTO urbancndep.stems
(
  shrub_id,
  direction,
  pre_date
)
(
 SELECT
  shrub_id,
  direction,
  survey_date
 FROM stems_temp.plotsplants
);")

# prep for and add stem_id generated from insert of new stems to plotsplants
dbGetQuery(con,"
ALTER TABLE stems_temp.plotsplants
ADD COLUMN stem_id_new integer;")

dbGetQuery(con,"
UPDATE stems_temp.plotsplants
SET stem_id_new = urbancndep.stems.id
FROM urbancndep.stems
WHERE
  urbancndep.stems.shrub_id = plotsplants.shrub_id AND
  urbancndep.stems.direction = plotsplants.direction AND
  urbancndep.stems.pre_date = plotsplants.survey_date;") # note pre_date used here!

# prep for and add stem_id generated from update of old stems to plotsplants
dbGetQuery(con,"
ALTER TABLE stems_temp.plotsplants
ADD COLUMN stem_id_old integer;")

dbGetQuery(con,"
UPDATE stems_temp.plotsplants
SET stem_id_old = urbancndep.stems.id
FROM urbancndep.stems
WHERE
  urbancndep.stems.shrub_id = plotsplants.shrub_id AND
  urbancndep.stems.direction = plotsplants.direction AND
  urbancndep.stems.post_date = plotsplants.survey_date;") # note post_date used here!

# prep for and add stem_id generated from update of stems to old
dbGetQuery(con,"
ALTER TABLE stems_temp.old
ADD COLUMN stem_id integer;")

dbGetQuery(con,'
UPDATE stems_temp.old
SET stem_id = plotsplants.stem_id_old
FROM stems_temp.plotsplants
WHERE
  plotsplants.superkey = old."PARENT_KEY" AND
  plotsplants.dir = old.old_direction;')

# prep and add boolean TRUE to denote post measurement
dbGetQuery(con,'
ALTER TABLE stems_temp.old
ADD COLUMN postmeas boolean;')

dbGetQuery(con,'
UPDATE stems_temp.old
SET postmeas=TRUE;')

# insert old lengths into pg.stem_lengths
dbGetQuery(con,'
INSERT INTO urbancndep.stem_lengths
(
  stem_id,
  length_in_mm,
  post_measurement
)
(
 SELECT
  stem_id,
  "oldLength",
  postmeas
 FROM stems_temp.old
 WHERE "oldLength" IS NOT NULL
);')

# prep for and add stem_id generated from insert of stems to new
dbGetQuery(con,'
ALTER TABLE stems_temp.new
ADD COLUMN stem_id integer;')

dbGetQuery(con,'
UPDATE stems_temp.new
SET stem_id = plotsplants.stem_id_new
FROM stems_temp.plotsplants
WHERE
  plotsplants.superkey = new."PARENT_KEY" AND
  plotsplants.dir = new.new_direction;')

# prep and add boolean FALSE to denote post measurement
dbGetQuery(con,'
ALTER TABLE stems_temp.new
ADD COLUMN postmeas boolean;')

dbGetQuery(con,'
UPDATE stems_temp.new
SET postmeas=FALSE;')

# insert new lengths into pg.stem_lengths
dbGetQuery(con,'
INSERT INTO urbancndep.stem_lengths
(
  stem_id,
  length_in_mm,
  post_measurement
)
(
 SELECT
  stem_id,
  "newLength",
  postmeas
 FROM stems_temp.new
 WHERE "newLength" is not null
);')

# insert any notes about the plants into the stems_comment table
# by setting post_measuremnt to TRUE, the query assumes the comment was made upon old collection (post visit)
# this is reasonable and it is hard to imagine a comment corresponding to the new collection,
# though dead or new plant might be an example. Just be cognizant of this and edit as required.
dbGetQuery(con,"
INSERT INTO urbancndep.stem_comment
(
  stem_id,
  comment,
  post_measurement
)
(
 SELECT
   stem_id_old, -- forces association with old lenghts (post visit)
   plant_note,
   'TRUE' -- forces association with old lengths (post visit)
 FROM stems_temp.plotsplants
 WHERE plant_note not like '');")

# note that comments about the plot are not considered in this workflow and will need to be
# addressed if it ever comes up

# do post-entry error checking ----
# using oldLengths and newLengths computed above, compare to what you put in PG
SELECT
  s.code AS site_code,
  p.id AS plot_id,
  t.code AS treatment_code,
  sp.scientific_name,
  sh.code AS shrub_code,
  sh.note AS shrub_note,
  st.direction,
  st.pre_date,
  st.post_date,
  st.post_note,
  sl.post_measurement,
  sl.length_in_mm as stem_length,
  sc.comment
FROM urbancndep.stems st
JOIN urbancndep.shrubs sh ON sh.id = st.shrub_id
JOIN urbancndep.plots p ON sh.plot_id = p.id
JOIN urbancndep.sites s ON p.site_id = s.id
JOIN urbancndep.treatments t ON p.treatment_id = t.id
JOIN urbancndep.shrub_species sp ON sh.shrub_species_id = sp.id
LEFT JOIN urbancndep.stem_lengths sl ON st.id = sl.stem_id
LEFT JOIN urbancndep.stem_comment sc ON (sc.stem_id = st.id AND sl.post_measurement = sc.post_measurement)
WHERE
  EXTRACT (YEAR FROM st.pre_date) = YEAR_OF_INTEREST
ORDER BY sl.post_measurement, p.id, st.pre_date, sh.code, st.direction;