#' @title helper: generate a matrix of all possible combinations of currently
#' sampled plots and plants married to data from sampling campaign
#'
#' @description Inluded here is a workflow for generating a complete matrix of
#' all combinations of plots and plants that are surveyed as part of the Desert
#' Fertilization stem monitoring effort. The matrix is married to data
#' collected from a given campain to facilitate finding and addressing
#' data-recording errors.
#'
#' @note Because the plots and plants that are surveyed are static (i.e., we
#' survey the same nine sites), the part of the workflow to constuct that
#' matrix is for reference only with a static file (active_plots_plants.csv)
#' representing these data, the contents of which can be read from file and
#' married to the data from a sampling campaign.The worfklow is for reference
#' only.
#'
# active_plots <- DBI::dbGetQuery(
#   conn      = pg,
#   statement = "
#   SELECT
#     plots.id AS plot_id,
#     sites.code AS site,
#     treatments.code AS treatment
#   FROM urbancndep.sites
#   JOIN urbancndep.plots ON (plots.site_id = sites.id)
#   JOIN urbancndep.treatments ON (treatments.id = plots.treatment_id)
#   WHERE
#   sites.code IN (
#     'PWP',
#     'SMW',
#     'SME',
#     'SRR',
#     'LDP',
#     'MCS',
#     'WTM',
#     'EMW',
#     'EME'
#     ) AND
#   treatments.code !~ '2'
#   ;
#   ")

# plant_id <- paste0("L", rep(1:5))

# active_plots_plants <- tidyr::expand_grid(
#   active_plots,
#   plant_id
# )

#' @export

active_plots_plants <- readr::read_csv("active_plots_plants.csv")

generate_complete_matrix <- function(
  plots_plants_data,
  all_plots_plants = active_plots_plants
  ) {

  all_combinations <- dplyr::full_join(
    x  = plots_plants_data,
    y  = all_plots_plants,
    by = c("plot_id", "plant_id")
    ) |>
  dplyr::arrange(
    plot_id,
    plant_id
  )

  return(all_combinations)

}
