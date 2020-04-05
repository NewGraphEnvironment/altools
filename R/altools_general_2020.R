#' remove NAs.  Works to remove a na from a list to if they are a list element or are within list elements
#'
#' remove NAs. Needed this when NA was one of the unique variables in
#' a column that got turned into a list

#' @param x a list that needs NAs removed from

#'
#' @export at_na_remove
#'

at_na_remove <- function(x) x[!is.na(x)]



#' Count the number of words in a string.
#'
#' This actually counts the number of spaces so be sure to stringr::str_trim your whitespaces out from start and end. Check out
#' \href{https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html}{details on s and other regular expressions}

#' @param x string or vector of stings (in mutate case) to count words (or spaces in reality)
#' @param ... Not used

#'
#' @export at_count_words
#' @examples
#' at_count_words("test this little thing")
#' at_count_words(" watch out for white spaces ! ")

at_count_words <- function(x){
  stringr::str_count(x, '\\s+') + 1
}


#' remove columns that have only NAs from dataframe. Pipeable.
#'
#' Function came from
#' \href{https://stackoverflow.com/questions/15968494/how-to-delete-columns-that-contain-only-nas}{here}

#' @param df dataframe
#' @param ... Not used. Can be used to pass parameters to other functions.


#' @export at_na_remove_column

at_na_remove_column <- function(df){
  df[colSums(!is.na(df)) > 0]
}

#' Remove columns with a specific string anywhere in the name.
#'
#' Function came from
#' \href{https://stackoverflow.com/questions/41815039/remove-columns-that-contain-a-specific-word}{here}

#' @param df dataframe
#' @param string Character string in quotations contained within columns to be removed from dataframe
#' @param ... Not used. Can be used to pass parameters to other functions.
#'
#' @export at_remove_columns_named_this
#'

at_remove_columns_named_this <- function(df, string, ...){
  df[,!grepl(string, colnames(df))]
}
