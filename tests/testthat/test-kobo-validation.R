testthat::local_reproducible_output(width = 120)

source(base::file.path("..", "..", "helper_check_kobo_data.R"))
source(base::file.path("..", "..", "helper_identify_new_missing.R"))

testthat::test_that("intermediate checks report failure without stopping", {
  duplicate_data <- tibble::tibble(
    survey_date = base::as.Date(base::c("2026-05-01", "2026-05-01")),
    plot_id = base::c(1, 1),
    plant_id = base::c("L1", "L1")
  )

  output <- testthat::capture_output(
    result <- check_kobo_key_uniqueness(
      plots_plants_data = duplicate_data,
      keys = base::c("survey_date", "plot_id", "plant_id"),
      check_name = "intermediate duplicate check",
      stop_on_error = FALSE
    )
  )

  testthat::expect_false(result)
  testthat::expect_match(output, "RESULT: FAILED", fixed = TRUE)
  testthat::expect_match(output, "WORKFLOW: CONTINUING", fixed = TRUE)
})

testthat::test_that("the final gate reports every failed check", {
  testthat::expect_error(
    stop_on_failed_kobo_checks(
      base::c(required_values = TRUE, unique_keys = FALSE)
    ),
    "unique_keys",
    fixed = TRUE
  )
})

testthat::test_that("required values reject missing and blank keys", {
  source_data <- tibble::tibble(
    plot_id = base::c(1, NA),
    plant_id = base::c("L1", " ")
  )

  testthat::capture_output(
    result <- check_kobo_required_values(
      plots_plants_data = source_data,
      required_columns = base::c("plot_id", "plant_id"),
      check_name = "required values",
      stop_on_error = FALSE
    )
  )

  testthat::expect_false(result)
})

testthat::test_that("missing directions retain their plant lineage", {
  new_lengths <- tibble::tibble(
    submission_id = base::rep(101, 3),
    submission_uuid = base::rep("submission-uuid", 3),
    survey_date = base::as.Date(base::rep("2026-05-01", 3)),
    plot_id = base::rep(1, 3),
    plant_id = base::rep("L1", 3),
    plants_index = base::rep(7, 3),
    mapping_submission_id = base::rep(100, 3),
    mapping_plants_index = base::rep(6, 3),
    stem_index = 1:3,
    new_direction = base::c("N", "S", "E"),
    new_length = base::c(10, 11, 12)
  )

  missing <- identify_new_missing(new_lengths)

  testthat::expect_equal(base::nrow(missing), 1)
  testthat::expect_equal(missing$new_direction, "W")
  testthat::expect_equal(missing$submission_id, 101)
  testthat::expect_equal(missing$plants_index, 7)
  testthat::expect_equal(missing$mapping_submission_id, 100)
  testthat::expect_equal(missing$mapping_plants_index, 6)
  testthat::expect_true(base::is.na(missing$stem_index))
})
