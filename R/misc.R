# miscellaneous functions for use in package

#' check if dependency is installed
#'
#' @keywords internal
#' @noRd
check_dependency <- function(library_name) {
    flag <- requireNamespace(library_name, quietly = TRUE)
    if (!isTRUE(flag)) {
        msg <- sprintf("Please install the `%s` package.", library_name)
        return(msg)
    } else {
        return(TRUE)
    }
}
assert_dependency <- checkmate::makeAssertionFunction(check_dependency)

#' NULL-coalescing operator
#'
#'
#' @param x The primary value to check
#' @param y The fallback value to use if x is NULL
#'
#' @return Returns x if x is not NULL, otherwise returns y
#'
#' @examples
#' # Not run (internal function):
#' # x %||% y  # returns x if x is not NULL, otherwise y
#' # NULL %||% "default"  # returns "default"
#' # "value" %||% "default"  # returns "value"
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
    if (is.null(x)) {
        y
    } else {
        x
    }
}

#' char
#'
#' Converts values into characters
#' @param x vector
#' @return character vector
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
char <- function(x) {
    as.character(x)
}


#' num
#'
#' Converts values into character then numeric
#' @param x vector
#' @return numeric vector
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
num <- function(x) {
    as.numeric(char(x))
}

#' Convert various time formats to numeric
#'
#' Internal function to convert Date, POSIXct, character, etc. to numeric
#' values that can be used for creating networks
#'
#' @param time_data Vector of time values
#' @param time_col Name of the time column (for error messages)
#'
#' @return A list with:
#'   - numeric_time: Numeric version of the time variable
#'   - time_labels: Character labels for the time periods
#'   - original_class: The original class of the time data
#'
#' @keywords internal
#' @noRd

convert_time_to_numeric <- function(time_data, time_col = "time") {
    original_class <- class(time_data)[1]

    if (is.numeric(time_data) || is.integer(time_data)) {
        # already numeric
        unique_times <- sort(unique(time_data))
        return(list(
            numeric_time = time_data,
            time_labels = char(unique_times),
            original_class = original_class
        ))
    }

    if (inherits(time_data, "Date")) {
        # convert dates to numeric (days since origin)
        # but we'll use unique indices for the network
        unique_dates <- sort(unique(time_data))
        time_mapping <- setNames(seq_along(unique_dates), char(unique_dates))

        return(list(
            numeric_time = time_mapping[char(time_data)],
            time_labels = char(unique_dates),
            original_class = original_class
        ))
    }

    if (inherits(time_data, c("POSIXct", "POSIXlt"))) {
        # convert to date first (ignore time of day)
        date_data <- as.Date(time_data)
        unique_dates <- sort(unique(date_data))
        time_mapping <- setNames(seq_along(unique_dates), char(unique_dates))

        return(list(
            numeric_time = time_mapping[char(date_data)],
            time_labels = char(unique_dates),
            original_class = original_class
        ))
    }

    if (is.character(time_data)) {
        # for character, create ordered mapping
        unique_times <- sort(unique(time_data))
        time_mapping <- setNames(seq_along(unique_times), unique_times)

        # check if they look like dates and warn if so
        if (any(grepl("^\\d{4}-\\d{2}-\\d{2}$", utils::head(unique_times, 5)))) {
            cli::cli_alert_warning(
                "Character time variable looks like dates. Consider converting to Date class for better handling."
            )
        }

        return(list(
            numeric_time = time_mapping[time_data],
            time_labels = unique_times,
            original_class = original_class
        ))
    }

    # this shouldn't happen if time_check is working correctly
    cli::cli_abort("Unsupported time variable type: {original_class}")
}

#' unique_vector
#'
#' Get unique vector from
#' multiple vector inputs
#' @param ... vector inputs
#' @return numeric vector
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
unique_vector <- function(...) {
    u_vec <- unique(c(...))
    u_vec <- sort(u_vec)
    return(u_vec)
}

#' identical_recursive
#'
#' Recursively check if two or more objects are identical
#' @param ... objects to check
#' @return logical indicating whether objects are identical
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
identical_recursive <- function(...) {
    # org objects to compare
    to_compare <- c(...)

    # if just two, then just compare
    if (length(to_compare) == 2) {
        ident_logic <- identical(
            to_compare[1], to_compare[2]
        )
    }

    # if more than two, then recursively call fn
    if (length(to_compare) > 2) {
        ident_logic <- c(
            identical(to_compare[1], to_compare[2]),
            identical_recursive(to_compare[-1])
        )
    }

    # return TRUE if all are identical
    return(all(ident_logic))
}

#' Break string into list of strings by some fixed character
#' and then extract the desired values around that fixed
#' character
#'
#' @param string_to_split character: string to be split
#' @param break_by character: character to break string by
#' @param to_extract integer: index of the string to be extracted
#' @param fixed If `TRUE` match exactly, otherwise use regular expressions
#' @return a character vector of the extracted strings
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
split_string <- function(string_to_split, break_by, to_extract, fixed = TRUE) {
    str_to_list <- strsplit(string_to_split, break_by, fixed = fixed)
    extract_relev <- lapply(str_to_list, function(x) {
        x[to_extract]
    })
    return(unlist(extract_relev))
}

#' array_to_list
#'
#' This function converts a three dimensional array
#' into a list of matrices
#' @param arr three dimensional array to list
#' @param preserveAttr logical indicating whether to preserve attributes
#' @return list object
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
array_to_list <- function(arr, preserveAttr = TRUE) {
    # convert to list elements along third mode
    l <- lapply(1:dim(arr)[3], function(ii) {
        arr[, , ii]
    })
    names(l) <- dimnames(arr)[[3]]

    # if we want to preserve attrs and keep netify object
    if (preserveAttr) {
        # add back in netify attribs to top level list
        class(l) <- "netify"
        attr(l, "netify_type") <- "longit_list"
        arrAttr <- attributes(arr)
        for (ii in 5:length(arrAttr)) {
            attr(l, names(arrAttr)[ii]) <- arrAttr[[ii]]
        }

        # adjust array attributes for cross_sec
        arrAttr_cross <- arrAttr[1:15]
        arrAttr_cross["dim"]$dim <- arrAttr_cross["dim"]$dim[1:2]
        arrAttr_cross["dimnames"]$dimnames <- arrAttr_cross["dimnames"]$dimnames[1:2]
        arrAttr_cross["netify_type"] <- "cross_sec"
        arrAttr_cross["actor_pds"] <- NULL

        # add attributes to every element in list
        for (ii in seq_along(l)) {
            attributes(l[[ii]]) <- arrAttr_cross
        }
    }

    #
    return(l)
}

#' list_to_array
#'
#' This function converts a list of matrices
#' into a three dimensional array
#' @param list_of_mats list object
#' @return three dimensional array
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
list_to_array <- function(list_of_mats) {
    # get dim info to create array
    row_actors <- unique(unlist(lapply(list_of_mats, rownames)))
    col_actors <- unique(unlist(lapply(list_of_mats, colnames)))
    time_points <- names(list_of_mats)

    # create array
    arr <- array(NA,
        dim = c(length(row_actors), length(col_actors), length(time_points)),
        dimnames = list(row_actors, col_actors, time_points)
    )

    # fill array
    for (ii in 1:length(list_of_mats)) {
        list_of_mats_slice <- list_of_mats[[ii]]
        rows_sl <- rownames(list_of_mats_slice)
        cols_sl <- colnames(list_of_mats_slice)
        arr[rows_sl, cols_sl, ii] <- list_of_mats_slice
    }

    #
    return(arr)
}
