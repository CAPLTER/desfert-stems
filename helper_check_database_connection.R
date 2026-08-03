#' @title helper: validate the PostgreSQL workflow connection
#'
#' @description Confirms that a live connection was created by RPostgres. The
#' workflow relies on RPostgres support for schema-qualified `DBI::Id` objects,
#' savepoints, and transaction handling; legacy RPostgreSQL connections are not
#' accepted.
#'
#' @param connection An open DBI database connection.
#' @param connection_name Label included in diagnostic messages.
#'
#' @return `TRUE`, invisibly, when the connection is valid.
#' @export
#'
check_rpostgres_connection <- function(
  connection,
  connection_name = "database connection"
) {

  connection_classes <- base::class(connection)

  if (!base::inherits(connection, "PqConnection")) {
    base::stop(
      connection_name,
      " must be created by RPostgres::Postgres(); observed class: ",
      base::paste(connection_classes, collapse = ", "),
      call. = FALSE
    )
  }

  if (!DBI::dbIsValid(connection)) {
    base::stop(connection_name, " is not valid", call. = FALSE)
  }

  base::message(
    connection_name,
    " uses RPostgres (",
    base::paste(connection_classes, collapse = ", "),
    ")"
  )

  base::invisible(TRUE)

}
