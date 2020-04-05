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
    dplyr::slice(., -1)
}
