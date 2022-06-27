#' @title helper: load table to postgres schema
#'
#' @description A simple helper function to load a data table from the R
#' environment to the postgres stems_temp (or otherwise) schema.
#'
#' @export
#'
helper_load_table <- function(
  connection  = pg,
  schema_name = "stems_temp",
  table_name
  ) {

  if (
    DBI::dbExistsTable(
      conn = connection,
      name = c(schema_name, table_name)
    )
    ) {

    DBI::dbRemoveTable(
      conn = connection,
      name = c(schema_name, table_name)
    )
  }

  DBI::dbWriteTable(
    conn      = connection,
    name      = c(schema_name, table_name),
    value     = get(table_name),
    row.names = FALSE
  )

}
