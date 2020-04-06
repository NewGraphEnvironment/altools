#' Clean up excel spreadsheet imports
#'
#' Clean up excel spreadsheet imports by triming the number of columns, specifying which row should
#' be the top row

#' @param df dataframe imported from excel that needs to be trimmed and cleaned
#' @param column_last numeric identifier for the last column to be imported
#' @param row_first numeric identifier for the first row to be imported
#'
#' @importFrom magrittr "%>%"
#' @export at_trim_xlsheet
#'
#'


at_trim_xlsheet <- function(df, column_last = ncol(df), row_first = 1) {
  df %>%
    dplyr::select(1:column_last) %>% ##get rid of the extra columns.  should be more abstract
    dplyr::slice(row_first:nrow(.)) %>%
    purrr::set_names(nm = tolower(unlist(slice(.,1)))) %>%
    dplyr::slice(., -1) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(., which = "rows")
}


#' Clean up excel spreadsheet imports
#'
#' Clean up excel spreadsheet imports by triming the number of columns, automatically chooses first complete row which row should
#' be the top row useing "complete.cases"

#' @param df dataframe imported from excel that needs to be trimmed and cleaned
#' @param column_last numeric identifier for the last column to be imported.  defaults to the last column present.
#'
#' @importFrom magrittr "%>%"
#' @export at_trim_xlsheet2
#'
#'


at_trim_xlsheet2 <- function(df, column_last = ncol(df)) {
  df %>%
    dplyr::select(1:column_last) %>% ##get rid of the extra columns.  should be more abstract
    janitor::row_to_names(which.max(complete.cases(.))) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(., which = "rows")
}
