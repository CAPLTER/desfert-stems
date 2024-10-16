# README -----------------------------------------------------------------------

# Workflow to extract stem-length data collected using the KoboCollect
# desfert_stems app. Data are harvested from a data export from KoBoConnect in
# the form of an Excel file containing multiple sheets. Note that application
# logging data are accessed separately.

# Data-processing steps should be run sequentially (generally, anyway, as some
# error-fixing steps may not be required). Once extracted and formatted
# according to this workflow, follow the populate_database workflow to add the
# data to the urbancndep database.

# This workflow is new as of the spring 2022 collection, the first time using
# the KoBo suite of tools; also new are biovolume measurements of the
# stem-measurement plants, the results of which are added to the
# urbancndep.shrubs_measurement table.


# helper functions -------------------------------------------------------------

source("helper_read_data.R")
source("helper_remove_ambiguous.R")
source("helper_complete_matrix.R")
source("helper_manage_post_notes.R")


# workflow ---------------------------------------------------------------------

# path to KoBo download

path <- "~/Desktop/desfert_stems_-_all_versions_-_English_en_-_2024-10-16-19-25-51.xlsx"


# STEP 1: read data from KoBo download

plots  <- read_kobo_stems(path_to_file = path, worksheet = "desfert_stems")
plants <- read_kobo_stems(path_to_file = path, worksheet = "measurements_repeat")
old    <- read_kobo_stems(path_to_file = path, worksheet = "old_stems_repeat")
new    <- read_kobo_stems(path_to_file = path, worksheet = "new_stems_repeat")


# STEP 2: build plots + plants

plots_plants <- dplyr::left_join(
  x  = plots |> dplyr::rename(plots_index = index),
  y  = plants |> dplyr::rename(plants_index = index),
  by = c("uuid" = "submission_uuid")
) |>
  dplyr::mutate(
    note_about_plant = gsub(",", " ", note_about_plant),
    note_about_plant = gsub("[\n\r]", " ", note_about_plant),
    note_about_plant = stringr::str_trim(note_about_plant, side = c("both"))
  ) |>
  dplyr::select(
    survey_date = today,
    plot_id,
    id,
    uuid,
    plant_id,
    note_about_plant,
    plots_index,
    plants_index,
    dplyr::contains(c("width", "height"))
  )

plots_plants |>
  dplyr::count(plot_id, plant_id) |>
  dplyr::filter(n > 1)


# STEP 3: error checking

# evaluate a matrix that encompasses all combinations of plots and plants that
# should be measured versus those that were actually measured to identify
# potential errors

complete_matrix <- generate_complete_matrix(plots_plants_data = plots_plants)


# fix plot-level errors (if needed)

# Example, here fixing data for a plot so we need to go back and recreate
# plots_plants after running this; this error was identified from the
# complete_matrix.

# plots <- plots |>
#   dplyr::mutate(
#     plot_id = dplyr::case_when(
#       uuid == "0a467ea9-c373-4803-8316-cbe01d62e020" ~ 5,
#       TRUE ~ plot_id
#     )
#   )

# Generally, we will want to make a note about any data edits, particularly if
# there is any uncertainty or subjectivity to them. In most cases, the note,
# even if at the plot level, should be addressed at the plant level so that the
# information is starkly visible to a user assessing length data. Continuing
# with the above example, where we are altering the identity of a plot (1 to 5
# in this case), we need to associate the change to all plots in the numbers 1
# and 5 plots.

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

# rebuild PLOTS_PLANTS after running above fix !!


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
#   plot             = 75,
#   duplicated_plant = "L3",
#   missing_plant    = "L4",
#   survey_date      = "2022-10-14"
# )


# STEP 6: shrub dimensions

# isolate shrub dimension data before adding cardinal directions in the next step

# the spring 2022 (inaugural) collection interspersed measurements in units of
# cm and m; those are standardized in this workflow but the app will be updated
# to prevent this in the future so this should not be needed doing forward

shrub_dimensions <- plots_plants |>
  dplyr::filter(complete.cases(dplyr::across(contains(c("width", "height"))))) |>
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

directions_frame <- tibble::tibble(
  direction = c("North", "South", "West", "East")
)

plots_plants <- plots_plants |>
  merge(directions_frame, all = TRUE) |>
  dplyr::mutate(
    survey_date = as.Date(survey_date)
    # survey_date = dplyr::case_when(
    #   is.na(survey_date) & !is.na(today) ~ today,
    #   TRUE ~ survey_date
    # )
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
  ) |>
  assertr::assert(
    assertr::not_na, c(survey_date, plant_id, direction)
  )


# STEP 8: add old notes

directions_vector <- c("North", "South", "West", "East")

post_note <- purrr::map_df(.x = directions_vector, ~ coalesce_old_notes(source_data = plants, cardinal_direction = .x))

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