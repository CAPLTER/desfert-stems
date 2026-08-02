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

  table_identifier <- DBI::Id(
    schema = schema_name,
    table  = table_name
  )

  if (
    DBI::dbExistsTable(
      conn = connection,
      name = table_identifier
    )
    ) {

    DBI::dbRemoveTable(
      conn = connection,
      name = table_identifier
    )
  }

  DBI::dbWriteTable(
    conn      = connection,
    name      = table_identifier,
    value     = get(table_name),
    row.names = FALSE
  )

}
