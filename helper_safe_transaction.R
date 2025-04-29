#' @title helper: wrap database actions in a safe transaction
#'
#' @description Where feasible, wraps database actions in a transaction and
#' compares the number of affected records to a reference object. The
#' transaction is committed if the number of records affected and the number of
#' rows of the reference object are congruent, and rolled back if not.
#'
#' @param db_connection (character) Unquoted database connection identifier.
#' @param action (character) Database action that is to be performed, should be
#' in the form of plain text or a glue object.
#' @param ref_object (character) Unquoted name of the reference object. This
#' object must exist in the R environment.
#'
#' @export
#'
safe_transaction <- function(
  db_connection = pg,
  action,
  ref_object
) {

  # start transaction
  DBI::dbBegin(conn = db_connection)

  # execute action
  num_edits <- DBI::dbExecute(
    conn      = db_connection,
    statement = action 
  )

  if (num_edits == nrow(ref_object)) {

    DBI::dbCommit(conn = db_connection)
    message("executed action on ", num_edits)

  } else {

    DBI::dbRollback(conn = db_connection)
    message("numer of rows affected does not match object; rolling back")

  }

}
