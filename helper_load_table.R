#' @title helper: load table to postgres schema
#'
#' @description Writes one explicitly supplied data frame to a schema-qualified
#' PostgreSQL table and verifies that the persistent table contains the expected
#' number of rows.
#'
#' @param connection An open RPostgres connection.
#' @param schema_name Destination schema name.
#' @param table_name Destination table name.
#' @param table_data Data frame to write. When omitted, the helper resolves
#' `table_name` from the calling environment for archived workflow compatibility.
#'
#' @return A one-row tibble containing the verified staging row counts.
#'
#' @export
#'
helper_load_table <- function(
  connection  = pg,
  schema_name = "stems_temp",
  table_name,
  table_data = NULL
  ) {

  if (!base::inherits(connection, "PqConnection")) {
    base::stop(
      "staging connection must be created by RPostgres::Postgres(); ",
      "observed class: ",
      base::paste(base::class(connection), collapse = ", "),
      call. = FALSE
    )
  }

  if (!DBI::dbIsValid(connection)) {
    base::stop("staging connection is not valid", call. = FALSE)
  }

  if (base::is.null(table_data)) {
    table_data <- base::get(
      table_name,
      envir = base::parent.frame(),
      inherits = TRUE
    )
  }

  if (!base::is.data.frame(table_data)) {
    base::stop(table_name, " is not a data frame", call. = FALSE)
  }

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

  write_succeeded <- DBI::dbWriteTable(
    conn      = connection,
    name      = table_identifier,
    value     = table_data,
    row.names = FALSE
  )

  table_exists <- DBI::dbExistsTable(
    conn = connection,
    name = table_identifier
  )

  if (!base::isTRUE(write_succeeded) || !base::isTRUE(table_exists)) {
    base::stop(
      "failed to create persistent staging table ",
      schema_name,
      ".",
      table_name,
      call. = FALSE
    )
  }

  quoted_table <- DBI::dbQuoteIdentifier(connection, table_identifier)
  row_count_query <- base::paste0(
    "SELECT COUNT(*) AS row_count FROM ",
    quoted_table,
    ";"
  )
  database_rows <- DBI::dbGetQuery(
    conn = connection,
    statement = row_count_query
  )$row_count[[1]]
  expected_rows <- base::nrow(table_data)

  if (!base::identical(
    base::as.integer(database_rows),
    base::as.integer(expected_rows)
  )) {
    base::stop(
      schema_name,
      ".",
      table_name,
      " contains ",
      database_rows,
      " rows; expected ",
      expected_rows,
      call. = FALSE
    )
  }

  base::message(
    "staged ",
    schema_name,
    ".",
    table_name,
    ": ",
    database_rows,
    " rows verified"
  )

  tibble::tibble(
    table_name = table_name,
    expected_rows = expected_rows,
    database_rows = base::as.integer(database_rows)
  )

}
