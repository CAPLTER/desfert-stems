#' @title helper: remove records where plant identity is uncertain
#'
#' @description On occasion, plants are mislabeled at the time of record (e.g.,
#' two plots within a plot are labeled L2). We can sometimes distinguish the
#' identity of the plants (e.g., which one is actually L2 and which is whatever
#' plant was mislabeled as L2). However, more typically, this is an
#' unresolvable error, in which case we need to remove all the records (e.g.,
#' all records labeled as L2) since we cannot know which records are the true
#' L2 and which are the mislabeled plant. The remove_ambiguous_plants function
#' removes records associated with a twice-labled plant at a given plot,
#' including removing old and new measurements and adding a notation in the
#' plots_plants object: old and new length measurements are removed, while
#' appropriately labeled and noted plants are added.
#'
#' @param plot
#' (integer) The plot (as plot id) where the plant are mislabled.
#' @param duplicated_plant
#' (character) The identity of the plant that is duplicated. For example, if
#' there are two plants labled L2 for a given plot, that would be indicated
#' with this parameter as "L2".
#' @param missing_plant
#' (character) The identity of the plant that has been omitted. For example, if
#' there are two plants labled L2 for a given plot, this parameter would
#' correspond to the identify of the plant that was mislabeled (i.e., that is
#' missing from the record for the plot). Extending the example, if there were
#' two plants labeled L2 and none of the plants were labeled L3 then L3 would
#' be the missing plant and would be passed as an argument to this parameter.
#' @param survey_date
#' (character) The date (ISO format as character) that the plot was surveyed.
#'
#' @note The function does not return an object, rather the old, new, and
#' plots_plants data entities in the global environment are edited as
#' appropriate.
#'
#' @export
#'
remove_ambiguous_plants <- function(plot, duplicated_plant, missing_plant, survey_date) {

  tryCatch({

    old <<- dplyr::bind_rows(
      old |> dplyr::filter(plot_id == plot & plant_id != duplicated_plant),
      old |> dplyr::filter(plot_id != plot)
    )

    new <<- dplyr::bind_rows(
      new |> dplyr::filter(plot_id == plot & plant_id != duplicated_plant),
      new |> dplyr::filter(plot_id != plot)
    )

    plots_plants <<- dplyr::bind_rows(
      plots_plants |> dplyr::filter(plot_id == plot & plant_id != duplicated_plant),
      plots_plants |> dplyr::filter(plot_id != plot),
      dplyr::inner_join(
        tibble::tibble(
          plot_id          = plot,
          today            = as.Date(survey_date),
          note_about_plant = "missing field data"
          ),
        tibble::tibble(
          plant_id    = c(duplicated_plant, missing_plant),
          plot_id     = plot,
          ),
        by = c("plot_id")
      )
    )
  },
  error = function(cond) {
    message(cond)
  },
  warning = function(cond) {
    message(cond)
  },
  finally = { 
    message("edited")
  }
  )   

}
