#' Connect to an SQLite Database
#'
#' Establishes a connection to a local SQLite database file.
#'
#' @param path The file path to the SQLite database.
#' @return A DBI connection object.
#' @examples
#' \dontrun{
#' con <- connect_db("my_database.sqlite")
#' DBI::dbListTables(con)
#' }
#' @export
connect_db <- function(path) {
  DBI::dbConnect(RSQLite::SQLite(), dbname = path)
}

#' Preview Data from a Database Table
#'
#' Displays the first few rows of a table from an active DB connection.
#'
#' @param con A database connection object.
#' @param table The table name as a string.
#' @param n Number of rows to preview (default 5).
#' @return A data frame preview of the table.
#' @examples
#' \dontrun{
#' con <- connect_db("my_database.sqlite")
#' preview_table(con, "employees")
#' }
#' @export
preview_table <- function(con, table, n = 5) {
  query <- paste0("SELECT * FROM ", table, " LIMIT ", n)
  DBI::dbGetQuery(con, query)
}

#' Summarize a Table
#'
#' Returns basic summary statistics for a given table.
#'
#' @param con A database connection.
#' @param table The table name as a string.
#' @return A summary of numeric columns in the table.
#' @examples
#' \dontrun{
#' con <- connect_db("my_database.sqlite")
#' summarize_table(con, "employees")
#' }
#' @export
summarize_table <- function(con, table) {
  data <- DBI::dbReadTable(con, table)
  dplyr::summarise_if(data, is.numeric, mean, na.rm = TRUE)
}

#' Load and Summarize a CSV File
#'
#' Reads a CSV file into R, previews the first rows, and returns
#' a summary of numeric columns.
#'
#' @param file The path to a CSV file.
#' @param n Number of rows to preview (default 5).
#' @return A list with a preview and numeric summary.
#' @examples
#' \dontrun{
#' result <- load_csv("data/sample.csv")
#' result$preview
#' result$summary
#' }
#' @export
load_csv <- function(file, n = 5) {
  data <- read.csv(file, stringsAsFactors = FALSE)
  preview <- head(data, n)
  summary_stats <- dplyr::summarise_if(data, is.numeric, mean, na.rm = TRUE)
  list(preview = preview, summary = summary_stats)
}
