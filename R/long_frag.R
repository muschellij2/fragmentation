#' Fragmentation of Mulitple Subjects
#'
#' @param counts A data.frame of multiple (or 1) rows with 1441 or 1442 columns.
#' @param weartime data.frame of same dimension as \code{counts} and must
#' consist of 1's and 0's.  If unspecified, assumed to be when
#' \code{counts > 0}.
#' @param id column name/variable in \code{counts} and \code{weartime}
#' corresponding to an ID.
#' @param visit column name/variable in \code{counts} and \code{weartime}
#'  corresponding to a visit.
#' If not only one visit per \code{id}, set this to \code{NULL}
#' @param thresh.lower Lower cut-off for count values in intensity range.
#' Passed to \code{\link{accel.bouts}}
#' @param bout.length minimum length of an activity bout.
#' Passed to \code{\link{accel.bouts}}
#' @param ... additional arguments passed to  \code{\link{accel.bouts}}
#'
#' @return List of mean rest bout, mean activity bout, the total data,
#' and the reciprocal of the bouts
#' @export
#'
#' @importFrom tidyr spread
#' @importFrom dplyr do summarize recode
#' @importFrom rlang "!!"
#'
#' @examples
#' counts = example_activity_data$counts
#' weartime = example_activity_data$weartime
#' res = multi_frag(counts, weartime = weartime, thresh.lower = 100)
multi_frag = function(
  counts, weartime,
  id = "ID",
  visit = "visit",
  thresh.lower = 100,
  bout.length = 1) {

  # stupid NSE problem with dplyr
  ID = values = value = mean_bout = NULL
  rm(list = c("ID", "values", "mean_bout", "value"))


  . = y = mean_active = mean_rest = NULL
  rm(list = c("mean_active", "mean_rest", "y", "."))


  long_counts = wide_to_long(counts, id = id, visit = visit)
  long_weartime = wide_to_long(weartime, id = id, visit = visit)
  # long_counts$ID = long_counts[, id]
  # long_weartime$ID = long_weartime[, id]
  #
  # long_weartime[, id] = NULL
  # long_counts[, id] = NULL

  uwear = unique(c(long_weartime$value))
  uwear = as.integer(uwear)
  if (!all(uwear %in% c(0, 1))) {
    stop("weartime data has non 0-1 data! Set NA to 0 if missing")
  }

  long_weartime$value = long_weartime$value > 0


  na_count = !is.finite(long_counts$value)
  bad = na_count & (long_weartime$value %in% TRUE)
  if (any(bad)) {
    warning("NA/NaN/Inf in counts but weartime is TRUE, setting weartime to false")
    long_weartime$value[bad] = NA
    # need this because na.omit doesn't get rid of Inf
    long_counts$value[bad] = NA
  }

  long_counts = long_counts %>%
    filter(!is.na(value))
  long_weartime = long_weartime %>%
    filter(!is.na(value))
  long_weartime$value[ !long_weartime$value ] = NA


  long_counts$y = accel.bouts(
    counts = long_counts$value,
    thresh.lower = thresh.lower,
    bout.length = bout.length)

  long_counts$y = long_counts$y * long_weartime$value

  check_y = function(y) {
    uy = unique(na.omit(y))
    if (length(uy) == 1) {
      stop("Only one state found in the wear data, no transition defined.")
    }
    return(TRUE)
  }
  name_id = as.name(id)

  checker = long_counts %>%
    group_by(!!name_id) %>%
    summarize(check = check_y(y))

  rle_mat = long_counts %>%
    select(-value) %>%
    group_by(!!name_id) %>%
    do(as_data_frame(rle2(.$y)))

  rle_mat = rle_mat %>%
    filter(!is.na(values))

  summ = rle_mat %>%
    group_by(!!name_id, values) %>%
    summarize( mean_bout = mean(lengths) )
  summ = summ %>%
    mutate(values = recode(values, `0` = "mean_rest", `1` = "mean_active"))
  summ = summ %>%
    spread(key = values, value = mean_bout)
  summ = summ %>% mutate(
    lambda_rest = 1/mean_rest,
    lambda_active = 1/mean_active)
  L = list(summarized = summ,
           bout_data = rle_mat)
  return(L)
}



#' Convert Wide 1440+ data to Long
#'
#' @param x A \code{data.frame} of and ID variable, 1440 measurements, and
#' an optional visit variable
#' @param id column name/variable in \code{x} corresponding to an ID
#' @param visit column name/variable in \code{x} corresponding to a visit.
#' If not only one visit per \code{id}, set this to \code{NULL}
#'
#' @return A \code{data.frame} of long values
#' @export
#'
#' @importFrom tidyr gather_
#' @importFrom dplyr mutate arrange
#' @examples
#'long_counts = wide_to_long(example_activity_data$counts, id = "ID",
#'visit = "visit")
wide_to_long = function(x, id = "ID", visit = NULL) {

  # stupid NSE problem with dplyr
  minute = NULL
  rm(list = c("minute"))

  x = as.data.frame(x)
  cn = colnames(x)
  gather_cols = cn[ !(cn %in% c(id, visit)) ]

  x = x %>%
    gather_(key_col="minute", value_col = "value",
            gather_cols = gather_cols ) %>%
    mutate(minute = gsub("[[:alpha:]]", "", minute),
           minute = trimws(minute),
           minute = as.numeric(minute))
  if (any(is.na(x$minute))) {
    stop("Minute data cannot be changed to numeic")
  }
  name_id = as.name(id)

  if (is.null(visit)) {
    x = x %>%
      arrange(!!name_id, minute)
  } else {
    name_vis = as.name(visit)
    x = x %>%
      arrange(!!name_id, !!name_vis, minute)
  }
  return(x)
}
