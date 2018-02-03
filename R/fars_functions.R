#' The function \code{fars_read} is used to read the data from a .csv file.
#' It assumes that the input file is a is in a csv formar. In cases the input
#' file name is incorrect, it outputs an error.
#'
#' @param filename a name of the input file in the current folder or a full path to the file
#'
#' @note An error may result if the file is in a format that cannot be correctly read by the read_csv function
#'
#' @return The function returns a data frame in a tibble format as a result of an application of tbl_df function
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples fars_read("data/accident_2013.csv.bz2")
#'
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' The function \code{make_filename} automatically generates a name for a new file based on the input year.
#'
#' @param year An integer value for the year in a user defined format, e.g. 17 or 2017
#'
#' @return The function returns a string for the file name in the format "accident_%d.csv.bz2", where %d
#' is a placeholder for the integer value of the year
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' The function \code{fars_read_years} reads the values of month and year from individual reports
#' based on a vector of year values and generates a list of data frames for further processing by another function.
#'
#' @param years a vector of integer year values
#'
#' @note The function deals effectively with inappropriate input values.
#'
#' @return The function returns a list of data frames, each of which consists of Month and year columns
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples fars_read_years(c(2014, 2015, 2016))
#'
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

#' The function \code{fars_summarize_years} takes a list of data frames produced by the function \code{fars_read_years}
#' and produces a summary table of the number of observations attributed to each month over the years, for which data are
#' available.
#'
#' @param years a vector of integer year values
#'
#' @return A data frame containing months as observations, years as column names and number of individual
#' observations as values
#'
#' @import dplyr
#' @importFrom tidyr spread
#'
#' @examples fars_summarize_years(c(2014, 2015, 2016))
#'
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' The function \code{fars_map_state} takes the value of state number and year to produce a map with
#' accidents marked
#'
#' @param state.num numerical ID of a state
#' @param year integer value of the year
#'
#' @note This function does not have any data ourput, instead, it produces a plot as a side effect of the function
#'
#' @import dplyr
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples fars_map_state(10, 2015)
#'
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
