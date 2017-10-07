#' Read  in a file.
#' This function will check to see if a file exists based on the file path.
#' @param filename The name of the file to read.
#' @param filepath a character string with the directory of the package's data files.
#' @return This file returns the data in the csv file as a tibble.
#' @examples
#' \dontrun{
#' #' fars_read('accident_2013.csv.bz2') #only if in the same directory as data
#' fileName <- system.file("extdata","accident_2013.csv.bz2",package="fars") #when using provided data in package
#' fars_read(fileName)
#' }
#' @export
#' @import readr dplyr
fars_read <- function(filename,filepath = system.file("extdata",filename,package="fars")) {
  if(!file.exists(filepath))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filepath, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Make a file name
#' This function will make a filename string.
#' @param year The year value to paste in the file string
#' @return This function returns a string denoting the file name with the form "accident_[year].csv.bz2"
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename('2013')
#' }
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read multiple years of data
#' This function will take a vector of years and return a list of data frames.
#' @param years A vector of years.
#' @return If the years are valid, the function will return a data frame. Otherwise the function will return null.
#' @examples
#' \dontrun{
#' fars_read_years(c(2013,2014,2015))
#' example_years <- 2013:2015
#' fars_read_years(example_years)
#' }
#' @export
#' @importFrom magrittr "%>%"
#' @import dplyr
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Read multiple years of data and summarize.
#' This function will take a vector of years and return a summary of the data by month and year.
#' @param years A vector of years.
#' @return If the years are valid, the function will return a summary of the total events by month and year. Otherwise the function will return null.
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013,2014,2015))
#' example_years <- 2013:2015
#' fars_summarize_years(example_years)
#' }
#' @export
#' @import dplyr tidyr
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Create a map of the events
#' This function will read in a state (by number index) and a year and return a map of the event locations.
#' @param state.num State number index in the fars data
#' @param year Four digit year
#' @return a map of the event locations for the inputted state.
#' @examples
#' \dontrun{
#' fars_map_state(1,2013) #Print events in Alabama for 2013
#' }
#' @export
#' @import maps graphics
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
