# README -----------------------------------------------------------------------

# Workflow to extract stem-length data collected using the KoboCollect
# desfert_stems app. Data are harvested from a data export from KoBoConnect in
# the form of an Excel file containing multiple sheets. Note that application
# logging data are accessed separately.

# Data-processing steps should be run sequentially (generally, anyway, as some
# error-fixing steps may not be required). Once extracted and formatted
# according to this workflow, follow the `populate_database.qmd` workflow to
# add the data to the urbancndep database.

# This workflow is new as of the spring 2022 collection, the first time using
# the KoBo suite of tools; also new are biovolume measurements of the
# stem-measurement plants, the results of which are added to the
# urbancndep.shrubs_measurement table.


# helper functions -------------------------------------------------------------

source("helper_read_data.R")
source("helper_remove_ambiguous.R")
source("helper_complete_matrix.R")
source("helper_manage_post_notes.R")
source("helper_build_plots_plants.R")
source("helper_check_kobo_data.R")


# workflow ---------------------------------------------------------------------

# path to KoBo download

path <- "~/Desktop/desfert_stems_-_all_versions_-_English_en_-_2026-08-01-19-31-30.xlsx"


# STEP 1: read data from KoBo download

plots  <- read_kobo_stems(path_to_file = path, worksheet = "desfert_stems")
plants <- read_kobo_stems(path_to_file = path, worksheet = "measurements_repeat")
old    <- read_kobo_stems(path_to_file = path, worksheet = "old_stems_repeat")
new    <- read_kobo_stems(path_to_file = path, worksheet = "new_stems_repeat")


# STEP 2: build plots + plants

plots_plants <- build_plots_plants(
  plots_data  = plots,
  plants_data = plants
)


# STEP 3: error checking

## all plot * plants should be 1

plots_plants |>
  dplyr::count(plot_id, plant_id) |>
  dplyr::filter(n > 1)

## check for duplicate rows in the plots_plants data frame, which would indicate
## that there are multiple entries for the same combination of survey_date,
## plot_id, and plant_id. This is important to ensure that each plant measurement
## is unique and can be accurately associated with its corresponding plot and
## survey date.

check_kobo_key_uniqueness(
  plots_plants_data = plots_plants,
  keys              = c("survey_date", "plot_id", "plant_id"),
  check_name        = "pre-direction plots_plants",
  stop_on_error     = FALSE
)

## evaluate a matrix that encompasses all combinations of plots and plants that
## should be measured versus those that were actually measured to identify
## potential errors

complete_matrix <- generate_complete_matrix(plots_plants_data = plots_plants)

# fix plot-level errors (if needed)

# Example, here fixing data for a plot so we need to go back and recreate
# plots_plants after running this; this error was identified from the
# complete_matrix:

# plots <- plots |>
#   dplyr::mutate(
#     plot_id = dplyr::case_when(
#       uuid == "0a467ea9-c373-4803-8316-cbe01d62e020" ~ 5,
#       TRUE ~ plot_id
#     )
#   )

# Example, here fixing data for a plant in the October 2025 collection; this
# error was identified from the complete_matrix:

# plants <- plants |>
#   dplyr::mutate(
#     plant_id = dplyr::case_when(
#       index == 56 ~ 'L5',
#       TRUE ~ plant_id
#     )
#   )


# Depending on the correction, we will want to make a note about any data edits,
# particularly if there is any uncertainty or subjectivity to them. In most
# cases, the note, even if at the plot level, should be addressed at the plant
# level so that the information is starkly visible to a user assessing length
# data. Continuing with the above example, where we are altering the identity of
# a plot (1 to 5 in this case), we need to associate the change to all plots in
# the numbers 1 and 5 plots.

# plots_1_5 <- c(
#   "0a467ea9-c373-4803-8316-cbe01d62e020",
#   "8dbdc0e1-600d-43a7-9f78-ffb20d581834",
#   "8fa78a97-438c-4232-965c-4a59d854ef79",
#   "1319abfc-daca-4907-8a01-b6b016400e2a"
# )

# plants <- plants |>
#   dplyr::mutate(
#     note_about_plant = dplyr::case_when(
#       submission_uuid %in% plots_1_5 ~ "plot id was miscoded; ID assigned reflects a best guess",
#       TRUE ~ note_about_plant
#     )
#   )

# Example, here it seems that the E-W dimension of a shrub was entered into the
# notes field:

# plots_plants <- plots_plants |>
#   dplyr::mutate(
#     width_of_plant_at_widest_point_e_w = dplyr::case_when(
#       id == 400565687 & plant_id == "L1" ~ 2.16,
#       TRUE ~ width_of_plant_at_widest_point_e_w
#     ),
#     note_about_plant = dplyr::case_when(
#       id == 400565687 & plant_id == "L1" ~
#         "width_of_plant_at_widest_point_e_w was miscoded; value reflects a likely measurement based on field notes",
#       TRUE ~ note_about_plant
#     )
#   )

# In most cases, we need to rebuild plots plants:

plots_plants <- build_plots_plants(
  plots_data  = plots,
  plants_data = plants
)


# Always, re-check the data:

check_kobo_key_uniqueness(
  plots_plants_data = plots_plants,
  keys              = c("survey_date", "plot_id", "plant_id"),
  check_name        = "post-fix pre-direction plots_plants",
  stop_on_error     = FALSE
)


# STEP 4: apply appropriate formatting and metadata to new and old stem lengths

old <- dplyr::left_join(
  x  = old,
  y  = plots_plants,
  by = c(
    "submission_id" = "id",
    "parent_index"  = "plants_index"
  )
) |>
  dplyr::select(
    plant_id,
    old_direction = direction,
    old_length,
    plot_id,
    plants_index  = parent_index
  )

new <- dplyr::left_join(
  x  = new,
  y  = plots_plants,
  by = c(
    "submission_id" = "id",
    "parent_index"  = "plants_index"
  )
) |>
  dplyr::select(
    plant_id,
    new_direction = direction,
    new_length,
    plot_id,
    plants_index  = parent_index
  )


# STEP 5: fix errors: new, old, and plots_plants (if necessary)

# fixing errors identified with the complete_matrix; see
# remove_ambiguous_plants description for function details; these in addition
# to the plot-level error and fix addressed at STEP 3

# remove_ambiguous_plants(
#   plot             = 64,
#   duplicated_plant = "L2",
#   missing_plant    = "L3",
#   survey_date      = "2024-10-22"
# )

# However, below is an example where we need to remove duplicate, ambiguous
# plants and stems from plots_plants, in which case fixes are applied to
# plots_plants and old; plots_plants is NOT rebuilt.

ambiguous_plants_index <- c(31, 133)

plots_plants <- plots_plants |>
  dplyr::filter(!plants_index %in% ambiguous_plants_index)

old <- old |>
  dplyr::filter(!plants_index %in% ambiguous_plants_index)


# STEP 6: shrub dimensions

# isolate shrub dimension data before adding cardinal directions in the next
# step

# the spring 2022 (inaugural) collection interspersed measurements in units of
# cm and m; those are standardized in this workflow but the app will be updated
# to prevent this in the future so this should not be needed doing forward

shrub_dimensions <- plots_plants |>
  dplyr::filter(
    complete.cases(
      dplyr::across(contains(c("width", "height")))
    )
  ) |>
  dplyr::rename(
    n_s = width_of_plant_at_widest_point_n_s,
    e_w = width_of_plant_at_widest_point_e_w,
  ) |>
  dplyr::mutate(
    n_s = dplyr::case_when(
      n_s > 50 ~ n_s / 100,
      TRUE ~ n_s
    ),
    e_w = dplyr::case_when(
      e_w > 50 ~ e_w / 100,
      TRUE ~ e_w
    ),
    height_of_plant = dplyr::case_when(
      height_of_plant > 50 ~ height_of_plant / 100,
      TRUE ~ height_of_plant
    )
  )


# STEP 7

# add directions to plots_plants
# run a final uniqueness check that will stop execution

directions_frame <- tibble::tibble(
  direction = c("North", "South", "West", "East")
)

plots_plants <- plots_plants |>
  merge(directions_frame, all = TRUE) |>
  dplyr::mutate(
    survey_date = as.Date(survey_date)
  ) |>
  dplyr::select(
    survey_date,
    plot_id,
    id,
    uuid,
    plant_id,
    note_about_plant,
    plots_index,
    plants_index,
    direction
  )

check_kobo_key_uniqueness(
  plots_plants_data = plots_plants,
  keys              = c("survey_date", "plot_id", "plant_id", "direction"),
  check_name        = "direction-expanded plots_plants",
  stop_on_error     = TRUE
)


# STEP 8: add old notes

directions_vector <- c("North", "South", "West", "East")

post_note <- purrr::map_df(
  .x = directions_vector,
  .f = ~ coalesce_old_notes(
    source_data = plants,
    cardinal_direction = .x
  )
)

plots_plants <- plots_plants |>
  dplyr::left_join(
    post_note,
    by = c(
      "plants_index" = "index",
      "direction"    = "direction"
    )
  )


# STEP 9: harvest plot-level notes

plot_notes <- plots |>
  dplyr::select(
    site,
    plot_id,
    survey_date = date,
    plot_notes  = note_about_plot
  )

print("completed kobo_workflow.R")
