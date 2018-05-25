  
# libraries ----

library(RPostgreSQL)
library(tidyverse)
library(stringr)

# database connections ----
source('~/Documents/localSettings/pg_prod.R')
source('~/Documents/localSettings/pg_local.R')

pg <- pg_prod
pg <- pg_local

dbGetInfo(pg)

# bring in processed files ----

plots <- read_csv('cndep_stems.csv')
plants <- read_csv('cndep_stems_shrub.csv')
new <- read_csv('cndep_stems_new_new_stems.csv')
old <- read_csv('cndep_stems_old_old_stems.csv')

# begin data manipulation ----
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
direction <- c('North', 'South', 'West', 'East')
direction <- data.frame(direction, stringsAsFactors = F)
plotsplants <- merge(plotsplants, direction, all=T)

# merge notes about missing stems into a single column
plotsplants <- plotsplants %>% 
  mutate(`old-old_stems_missing-north_missing` = replace(`old-old_stems_missing-north_missing`, direction != 'North', '')) %>% 
  mutate(`old-old_stems_missing-south_missing` = replace(`old-old_stems_missing-south_missing`, direction != 'South', '')) %>% 
  mutate(`old-old_stems_missing-west_missing` = replace(`old-old_stems_missing-west_missing`, direction != 'West', '')) %>% 
  mutate(`old-old_stems_missing-east_missing` = replace(`old-old_stems_missing-east_missing`, direction != 'East', '')) %>% 
  mutate(postnote = paste(`old-old_stems_missing-north_missing`,
                          `old-old_stems_missing-south_missing`,
                          `old-old_stems_missing-west_missing`, 
                          `old-old_stems_missing-east_missing`)) %>% 
  mutate(postnote = replace(postnote, postnote == "", NA)) %>% 
  mutate(postnote = str_trim(postnote, side = 'both')) %>% 
  mutate(postnote = replace(postnote, postnote == "NA", NA)) %>% 
  mutate(dir = str_extract(direction, "^\\w{1}")) %>% 
  select('plot_id',
         'plot_notes',
         'survey_date',
         'plant_id',
         'plant_note',
         'KEY',
         'superkey',
         'dir',
         'direction',
         'postnote')

# preliminary error checking ----

# should be n sites * 16 per plant ID
plotsplants %>%
  count(plant_id)

# should be n sites * 4 plots * 5 plants, and each count should be 4
plotsplants %>%
  count(plot_id, plant_id) %>%
  ungroup() %>%
  arrange(desc(n), plot_id, plant_id) %>%
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

oldLengths <- inner_join(plots, plants, by = c('KEY' = 'PARENT_KEY')) %>% 
  inner_join(old, by = c('superkey' = 'PARENT_KEY')) %>%
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

newLengths <- inner_join(plots, plants, by = c('KEY' = 'PARENT_KEY')) %>% 
  inner_join(new, by = c('superkey' = 'PARENT_KEY')) %>%
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
# direction. Run the following to checks to see if there is a problem and
# possible fix with the raw data but see the section 'address missing new
# values' about how to address situations where there simply are no new
# measurements for a particular direction. Addressing this includes specifying
# that values for the missing directions are NULL and adding a comment to the
# stems comment table.
newLengths %>%
  group_by(plot_id, plant_id) %>%
  summarise(count = n_distinct(new_direction)) %>%
  filter(count < 4)

# looking for new-length NAs
newLengths %>%
  filter(is.na(newLength))

# looking for old-length NAs
# really nothing to be done with these
oldLengths %>%
  filter(is.na(oldLength))

# add data to database ----

# put the data in a list for faster writing to pg
data <- list(new, old, plotsplants)
names(data) <- c('new', 'old', 'plotsplants')

# create a new schema called stems_temp in pg then loop through list items to push them to postgresql
dbExecute(pg, 'DROP SCHEMA IF EXISTS stems_temp CASCADE;')
dbExecute(pg, 'CREATE SCHEMA IF NOT EXISTS stems_temp;')

# add the data tables to the schema
for (i in 1:length(data)) {
  
  # make sure tbl does not exist the write temp table
  if (dbExistsTable(pg, c('stems_temp', names(data)[[i]]))) dbRemoveTable(pg, c('stems_temp', names(data)[[i]])) 
  dbWriteTable(pg, c('stems_temp', names(data)[[i]]), value = data[[i]], row.names = F)

}

# SQL run sequentially! ----

# change datetime from R to type date (but see note below); 
# prep for and add shrub_ids to the plotsplants tablet-derived data; 
# prep for and add stem_id generated from insert of new stems to plotsplants (a later step); 
# add stem_id generated from update of old stems to plotsplants (a later step)

# survey_date may already be type data, check first and comment/uncomment
# statement as appropariate
dbExecute(pg,'
ALTER TABLE stems_temp.plotsplants
  -- ALTER COLUMN survey_date TYPE DATE,
  ADD COLUMN shrub_id INTEGER,
  ADD COLUMN stem_id_new INTEGER,
  ADD COLUMN stem_id_old integer')

dbExecute(pg,'
UPDATE stems_temp.plotsplants
SET shrub_id = urbancndep.shrubs.id
FROM urbancndep.shrubs
WHERE
  urbancndep.shrubs.plot_id = plotsplants.plot_id AND
  urbancndep.shrubs.code = plotsplants.plant_id;')

# update existing pg.stems with this survey's post date & notes

# BE CERTAIN to set month & year to the correct pre date, which should be the most
# recent set (may include multiple months)
dbExecute(pg,"
UPDATE urbancndep.stems
SET
  post_date = plotsplants.survey_date,
  post_note = plotsplants.postnote
FROM stems_temp.plotsplants
WHERE
  plotsplants.shrub_id = urbancndep.stems.shrub_id AND
  plotsplants.direction = urbancndep.stems.direction AND
  # EXTRACT(MONTH from urbancndep.stems.pre_date) = __SET_PREVIOUS_PERIOD_MONTH__ AND
  # EXTRACT(YEAR from urbancndep.stems.pre_date) = __SET_PREVIOUS_PERIOD_YEAR;") # set appropriatly !

  # EXTRACT(MONTH from urbancndep.stems.pre_date) = __SET_PREVIOUS_PERIOD_MONTH__ AND
  # EXTRACT(YEAR from urbancndep.stems.pre_date) = __SET_PREVIOUS_PERIOD_YEAR;") # set appropriatly !

# insert pre data into pg.stems
dbExecute(pg,"
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

# add stem_id generated from insert of new stems to plotsplants
dbExecute(pg,"
UPDATE stems_temp.plotsplants
SET stem_id_new = urbancndep.stems.id
FROM urbancndep.stems
WHERE
  urbancndep.stems.shrub_id = plotsplants.shrub_id AND
  urbancndep.stems.direction = plotsplants.direction AND
  urbancndep.stems.pre_date = plotsplants.survey_date;") # note pre_date used here!

# add stem_id generated from update of old stems to plotsplants
dbExecute(pg,"
UPDATE stems_temp.plotsplants
SET stem_id_old = urbancndep.stems.id
FROM urbancndep.stems
WHERE
  urbancndep.stems.shrub_id = plotsplants.shrub_id AND
  urbancndep.stems.direction = plotsplants.direction AND
  urbancndep.stems.post_date = plotsplants.survey_date;") # note post_date used here!

# prep for and add stem_id generated from update of stems to old;
# prep it add boolean to denote post measurement
dbExecute(pg,"
ALTER TABLE stems_temp.old
  ADD COLUMN stem_id INTEGER,
  ADD COLUMN postmeas boolean;")

dbExecute(pg,'
UPDATE stems_temp.old
SET stem_id = plotsplants.stem_id_old
FROM stems_temp.plotsplants
WHERE
  plotsplants.superkey = old."PARENT_KEY" AND
  plotsplants.dir = old.old_direction;')

# add boolean TRUE to denote post measurement
dbExecute(pg,'
UPDATE stems_temp.old
SET postmeas=TRUE;')

# insert old lengths into pg.stem_lengths
dbExecute(pg,'
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

# prep for and add stem_id generated from insert of stems to new;
# prep boolean FALSE to denote post measurement (later step)
dbExecute(pg,'
ALTER TABLE stems_temp.new
  ADD COLUMN stem_id integer,
  ADD COLUMN postmeas boolean;')

dbExecute(pg,'
UPDATE stems_temp.new
SET stem_id = plotsplants.stem_id_new
FROM stems_temp.plotsplants
WHERE
  plotsplants.superkey = new."PARENT_KEY" AND
  plotsplants.dir = new.new_direction;')

dbExecute(pg,'
UPDATE stems_temp.new
SET postmeas=FALSE;')

# insert new lengths into pg.stem_lengths
dbExecute(pg,'
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

# insert any notes about the plants into the stems_comment table by setting
# post_measuremnt to TRUE, the query assumes the comment was made upon old
# collection (post visit) this is reasonable and it is hard to imagine a comment
# corresponding to the new collection, though dead or new plant might be an
# example. Just be cognizant of this and edit as required.
dbExecute(pg,"
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

# note that comments about the plot are not considered in this workflow and will
# need to be addressed if it ever comes up

# address missing new values ----

# identify the details of any NULL value new stems using new_null(). The trick
# here is that this should only be an issue if there is a new stem with a single
# null value and no other values. If that is the case, you may have to treat
# that stem like a missing new value. However, if there are two new values for a
# single direction, then one can be NULL but the other one will be entered into
# the database and thus avoid having a missing direction. So, for example, in
# May 2017, there was a NULL value at WTM-71-L1-E but there was another
# measurement for E so we could simple ignore that NULL value
new_null <- function() {
  
  dirSym <- c('N', 'S', 'W', 'E')
  dirSym <- data.frame(dirSym, stringsAsFactors = F)
  
  nullvalues <- newLengths %>%
    filter(is.na(newLength))
  
  aframe <- data.frame(stringsAsFactors = F)
  
  for (i in 1:nrow(nullvalues)) {
    
    plot <- nullvalues[i,]$plot_id
    plant <- nullvalues[i,]$plant_id
    
    aframe <- bind_rows(aframe, 
                        newLengths %>% 
                          filter(plot_id == plot, plant_id == plant) %>% 
                          right_join(dirSym, by = c("new_direction" = "dirSym")) %>% 
                          mutate(plot_id = replace(plot_id, is.na(newLength), plot)) %>% 
                          mutate(plant_id = replace(plant_id, is.na(newLength), plant))
    )
    
  }
  
  aframe <- aframe %>% 
    arrange(plot_id, plant_id, new_direction)
  
  return(aframe)
  
}

# identify the details of any missing new stems using new_missing(). We need to 
# document cases where there are not any data for a particular direction of a 
# plant. Here we find the details of the missing plant using new_missing(). We 
# can then use these details to construct our update statements, two for 
# direction without a new measurment: one to document the NULL value for that 
# direction, and two to add a comment in the comments table. The next step would
# be to generate the update statement based on new_missing() output instead of
# manually constructing each update statement as done here.
new_missing <- function() {
  
  dirSym <- c('N', 'S', 'W', 'E')
  dirSym <- data.frame(dirSym, stringsAsFactors = F)
  
  countlessthanfour <- newLengths %>%
    group_by(plot_id, plant_id) %>%
    summarise(count = n_distinct(new_direction)) %>%
    filter(count < 4)
  
  aframe <- data.frame(stringsAsFactors = F)
  
  for (i in 1:nrow(countlessthanfour)) {
    
    plot <- countlessthanfour[i,]$plot_id
    plant <- countlessthanfour[i,]$plant_id
    
    aframe <- bind_rows(aframe, 
                        newLengths %>% 
                          filter(plot_id == plot, plant_id == plant) %>% 
                          right_join(dirSym, by = c("new_direction" = "dirSym")) %>% 
                          mutate(plot_id = replace(plot_id, is.na(newLength), plot)) %>% 
                          mutate(plant_id = replace(plant_id, is.na(newLength), plant))
    )
    
  }
  
  aframe <- aframe %>%
    arrange(plot_id, plant_id, new_direction)
  
  return(aframe)
  
}

# where missing, add NULL value to stem length
dbExecute(pg, "
INSERT INTO urbancndep.stem_lengths(stem_id, length_in_mm, post_measurement)
(
  SELECT id, NULL, FALSE
  FROM urbancndep.stems
  WHERE 
  	EXTRACT (YEAR FROM pre_date) = 2018 AND
  	EXTRACT (MONTH FROM pre_date) = 5 AND
  	shrub_id IN (SELECT id FROM urbancndep.shrubs WHERE plot_id = 34 AND code LIKE 'L5') AND
  	direction ILIKE 'n%'
);")
  
dbExecute(pg, "
INSERT INTO urbancndep.stem_lengths(stem_id, length_in_mm, post_measurement)
(
  SELECT id, NULL, FALSE
  FROM urbancndep.stems
  WHERE EXTRACT (YEAR FROM pre_date) = 2018 AND
  	EXTRACT (MONTH FROM pre_date) = 5 AND
  	shrub_id IN (SELECT id FROM urbancndep.shrubs WHERE plot_id = 67 AND code LIKE 'L3') AND
  	direction ILIKE 'w%'
);")

# add comment(s) about missing value(s)
dbExecute(pg, "
INSERT INTO urbancndep.stem_comment(stem_id, post_measurement, comment)
(
  SELECT id, FALSE, 'missing value'
  FROM urbancndep.stems
  WHERE 
  	EXTRACT (YEAR FROM pre_date) = 2018 AND
  	EXTRACT (MONTH FROM pre_date) = 5 AND
  	shrub_id IN (SELECT id FROM urbancndep.shrubs WHERE plot_id = 34 AND code LIKE 'L5') AND
  	direction ILIKE 'n%'
);")

dbExecute(pg, "
INSERT INTO urbancndep.stem_comment(stem_id, post_measurement, comment)
(
  SELECT id, FALSE, 'missing value'
  FROM urbancndep.stems
  WHERE EXTRACT (YEAR FROM pre_date) = 2018 AND
  	EXTRACT (MONTH FROM pre_date) = 5 AND
  	shrub_id IN (SELECT id FROM urbancndep.shrubs WHERE plot_id = 67 AND code LIKE 'L3') AND
  	direction ILIKE 'w%'
);")
          
          
          




# post-entry error checking ----

# using oldLengths and newLengths computed above, compare to what you put in PG;
# do some comparison with raw tablet data
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

# clean up (if you want) ----

# or just leave it as a backup until the next set of stems
dbExecute(pg, 'DROP SCHEMA IF EXISTS stems_temp CASCADE;')
