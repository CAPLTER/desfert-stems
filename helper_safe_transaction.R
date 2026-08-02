#' @title helper: execute a checked database action
#'
#' @description Executes one database action and compares the affected row count
#' with an explicit expectation. New calls do not begin or commit a transaction;
#' the calling workflow owns the transaction boundary so dependent operations
#' can commit or roll back together. The legacy `ref_object` call form retains
#' its historical per-action transaction behavior.
#'
#' @param db_connection (character) Unquoted database connection identifier.
#' @param action (character) Database action that is to be performed, should be
#' in the form of plain text or a glue object.
#' @param expected_rows Integer number of rows the action must affect.
#' @param action_name Label included in transaction diagnostics.
#' @param ref_object Optional legacy reference object. When supplied instead of
#' `expected_rows`, the action runs in its own transaction for compatibility
#' with archived workflows.
#'
#' @export
#'
safe_transaction <- function(
  db_connection = pg,
  action,
  expected_rows = NULL,
  action_name = "database action",
  ref_object = NULL
) {

  legacy_transaction <- base::is.null(expected_rows)

  if (legacy_transaction) {
    if (base::is.null(ref_object)) {
      base::stop(
        "supply expected_rows or the legacy ref_object argument",
        call. = FALSE
      )
    }

    expected_rows <- base::nrow(ref_object)
  }

  if (
    base::length(expected_rows) != 1 ||
      base::is.na(expected_rows) ||
      expected_rows < 0
  ) {
    base::stop("expected_rows must be one nonnegative value", call. = FALSE)
  }

  execute_checked_action <- function() {
    num_edits <- DBI::dbExecute(
      conn      = db_connection,
      statement = action
    )

    row_count_matches <- base::identical(
      base::as.integer(num_edits),
      base::as.integer(expected_rows)
    )

    if (!row_count_matches) {
      base::stop(
        action_name,
        " affected ",
        num_edits,
        " rows; expected ",
        expected_rows,
        call. = FALSE
      )
    }

    base::message(action_name, ": affected ", num_edits, " rows as expected")
    base::invisible(num_edits)
  }

  if (legacy_transaction) {
    DBI::dbWithTransaction(db_connection, execute_checked_action())
  } else {
    execute_checked_action()
  }

}
