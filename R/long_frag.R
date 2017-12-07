#' Fragmentation
#'
#' @examples
#'
#' library(dplyr)
#' library(tidyr)
#' counts = example_activity_data$counts
#' weartime = example_activity_data$weartime
#' counts = counts %>%
#' gather(key=minute, value= value, -ID, -visit ) %>%
#' mutate(minute = gsub("[[:alpha:]]", "", minute),
#'        minute = trimws(minute),
#'         minute = as.numeric(minute)) %>%
#'        arrange(ID, visit, minute)
#' id_to_run = counts$ID[1]
#' counts = counts[ counts$ID == id_to_run, ]
#' counts$ID = counts$visit = NULL
#' weartime = weartime[ weartime$ID == id_to_run, ]
#' weartime$ID = weartime$visit = NULL
#'
#' res = frag(counts, weartime = weartime, thresh.lower = 100)
#'
long_frag = function(counts, weartime, id = "ID", visit = "visit") {
#
#   library(dplyr)
#   library(tidyr)

  counts = example_activity_data$counts
  weartime = example_activity_data$weartime
  long_counts = wide_to_long(counts, id = id, visit = visit)
  long_weartime = wide_to_long(weartime, id = id, visit = visit)
  long_weartime$value = long_weartime$value> 0
  thresh.lower = 100
  bout.length = 1

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
  checker = long_counts %>%
    group_by(ID) %>%
    summarize(check = check_y(y))

  rle_mat = long_counts %>%
    select(-value) %>%
    group_by(ID) %>%
    do(as_data_frame(rle2(.$y)))

  rle_mat = rle_mat %>%
    filter(!is.na(values))

  summ = rle_mat %>%
    group_by(ID, values) %>%
    summarize( mean_bout = mean(lengths) )
  summ = summ %>%
    mutate(values = recode(values, `0` = "rest", `1` = "active"))
  summ = summ %>%
    spread(key = values, value = mean_bout)
}



wide_to_long = function(x, id = "ID", visit = NULL) {
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
  if (is.null(visit)) {
    x = x %>%
      arrange(ID, minute)
  } else {

    x = x %>%
      arrange(ID, visit, minute)
  }
  return(x)
}
