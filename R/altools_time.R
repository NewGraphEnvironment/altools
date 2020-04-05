#' Converts an excel numeric representation of a date to YY-mm-dd.
#'
#' Converts an excel numeric representation of a date to YY-mm-dd. Got it from this Breza
#' \href{https://stackoverflow.com/questions/43230470/how-to-convert-excel-date-format-to-proper-date-with-lubridate}{kid}
#'

#' @param date Date to convert from number to posix
#' @param ... Not used

#'
#' @export at_time_excel_number
#' @examples
#' as.Date(42705, origin = "1899-12-30")

at_time_excel_number <-  function(date){
  as.Date(date, origin = "1899-12-30")
  }


#' Assign dates to a season
#'
#' Convert dates from any year to 2012 dates (leap year) and asssign a season. Stole it from
#' \href{https://stackoverflow.com/users/980833/josh-obrien}{Josh O'Brien} from this
#' \href{https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to}{post}

#' @param dates Dates to find season for
#' @param ... Not used

#'
#' @export at_getSeason
#' @examples
#' my.dates <- as.Date("2011-12-01", format = "%Y-%m-%d") + 0:60
#' head(at_getSeason(my.dates), 24)

at_getSeason <- function(dates, ...) {
  WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-19",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-20",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-22",  format = "%Y-%m-%d") # Fall Equinox

  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(dates, format="2012-%m-%d"))

  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}
