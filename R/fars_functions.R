#' Read a FARS datafile
#' 
#' This function loads a FARS datafile contained in a csv (or compressed csv)
#' into a dbplyr tibble.
#' 
#' @param filename Either a path to a file, a connection, or literal data (either a single string or a raw vector).
#' Files ending in .gz, .bz2, .xz, or .zip will be automatically uncompressed. 
#' Files starting with http://, https://, ftp://, or ftps:// will be automatically downloaded. 
#' Remote gz files can also be automatically downloaded and decompressed.
#' Literal data is most useful for examples and tests. 
#' It must contain at least one new line to be recognised as data (instead of a path)
#' 
#' @return Returns a tibble with the loaded FARS data.
#' 
#' @importFrom dplr, readr
#' 
#' @examples
#' tb_fars <- fars_read('./data/accident_2013.csv.bz2')
#' 
#' @references See /href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{NHTSA FARS Reporting System}
#' 
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'Make a FARS data file name based on year
#'
#'This function creates a character file name for a  based on an entered year 
#'for use in the fars_read function to read from csv files in bz2 compressed files
#'
#'@param year An integer scalar year value
#'
#'@return Returns a string with a file name based on the entered year.
#'
#'@examples
#'fname <-make_filename(1989)
#'
#'@references See /href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{NHTSA FARS Reporting System}
#'
#'
#'@export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#'Extract the month field from FARS data by year
#'
#'This function takes a vector of year values , creates filenames based on the years,
#'reads the FARS files and creates a list (one entry per year) of tibbles containing 
#'the MONTH and year of the reported accident. If an error occurs while reading the 
#'files the function will throw a warning
#'
#'@param years A vector of integer years
#'
#'@return A list of tibbles (one for each year ) containing MONTH and year data for 
#'each reported incident in that year.
#'
#'@importFrom dplyr, readr
#'
#'@examples
#'y <- fars_read_years(c(2013,2014,2015))
#'x <- fars_read_years(2014)
#'
#'@references See /href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{NHTSA FARS Reporting System}
#'
#'@export
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

#'Summarize the number of incidents by year and month
#'
#'This function takes a vector of years and produces a table of accidents from 
#'the FARS data showing the number of reported accidents by month for each of the years
#'passed in.
#'
#'@inheritParams fars_read_years
#'
#'@return A tibble showing the number of accidents in FARS database by MONTH (rows) for
#'each entered year.
#'
#'@importFrom dplyr, readr, tidyr
#'
#'@examples
#'y <- fars_summarize_years(c(2013,2014,2015))
#'x <- fars_read_years(2014)
#'
#'@references See /href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{NHTSA FARS Reporting System}
#'
#'@export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#'Plot a map of accidents for a state by year
#'
#'This function plots a map of the stae number passed with the latitude and longitude 
#'of the accidents for the passed year plotted.
#'
#'@param state.num An integer state number. For mapping of state numbers to state see
#'page 23 of \href{chrome-extension://oemmndcbldboiebfnladdacbdfmadadm/file:///C:/Users/MattW/Downloads/USERGUIDE-2014.pdf}{FARS Analytical Users Manual}
#'
#'@inheritParams make_filename
#'
#'@return NULL. 
#'
#'@importFrom dplyr, readr, tidyr, maps
#'
#'@examples
#'# Map Utah for 2013
#'fars_map_state(49,2013) 
#'
#'@references See /href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{NHTSA FARS Reporting System}
#'
#'@export
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