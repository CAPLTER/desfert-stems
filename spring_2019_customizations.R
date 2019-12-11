
# README ------------------------------------------------------------------

# There were two L1 plants but not an L4 plant at plot 68 during the spring 2019
# collection. The plant id of one set of the L1s was set to L4, and a comment
# was added to both sets regarding the uncertainty and change.

plotsplants <- plotsplants %>% 
  mutate(
    plant_id = case_when(
      superkey == 'uuid:8832ba58-02e3-4374-a8e3-9406610f0794/shrub[2]' ~ 'L4',
      TRUE ~ plant_id
    ),
    plant_note = case_when(
      superkey == 'uuid:70096a84-127a-49db-9bc5-08c4d856ccf8/shrub[1]' ~ '70% dead wood; WARNING: suspect plant ID, this could be L4',
      superkey == 'uuid:8832ba58-02e3-4374-a8e3-9406610f0794/shrub[2]' ~ 'WARNING: suspect plant ID, this could be L1',
      TRUE ~ plant_note
    )
  )

# uuids of the two L1 plants at plot 68, spring 2019
# uuid:70096a84-127a-49db-9bc5-08c4d856ccf8/shrub[1]
# uuid:8832ba58-02e3-4374-a8e3-9406610f0794/shrub[2]


# I added those stem comments to the pre stem lengths (in addition to the old
# stem lengths, which is the norm).
dbExecute(pg,"
INSERT INTO urbancndep.stem_comment
(
  stem_id,
  comment,
  post_measurement
)
(
 SELECT
   stem_id_new, -- forces association with new lenghts (pre visit)
   plant_note,
   'FALSE' -- forces association with new lengths (pre visit)
 FROM stems_temp.plotsplants
 WHERE plant_note ~* '.*warning.*'
);"
)
          
          