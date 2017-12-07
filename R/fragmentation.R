#' Fragmentation
#'
#' @param counts Matrix of multiple (or 1) rows with 1440 columns.
#' @param weartime Matrix of same dimension as \code{counts} and must
#' consist of 1's and 0's.  If unspecified, assumed to be when
#' \code{counts > 0}.
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
#' @importFrom accelerometry accel.bouts rle2
#' @importFrom dplyr data_frame filter %>% group_by
#' @importFrom dplyr summarize select as_data_frame
#' @importFrom stats na.omit
#' @examples
#' counts = matrix(rpois(1440*5, lambda = 5), ncol = 1440)
#' res = frag(counts, thresh.lower = 2)
#'
#' counts = example_activity_data$counts
#' weartime = example_activity_data$weartime
#' id_to_run = counts$ID[1]
#' counts = counts[ counts$ID == id_to_run, ]
#' counts$ID = counts$visit = NULL
#' weartime = weartime[ weartime$ID == id_to_run, ]
#' weartime$ID = weartime$visit = NULL
#'
#' res = frag(counts, weartime = weartime, thresh.lower = 100)
#'
frag = function(
  counts,
  weartime = NULL,
  thresh.lower = 100,
  bout.length = 1,
  ...) {

  counts = as.matrix(counts)
  # stupid NSE problem with dplyr
  mean_bout = values = lengths = NULL
  rm(list = c("mean_bout", "values", "lengths"))

  check_1440 = function(mat){
    n_time = ncol(mat)
    stopifnot(n_time == 1440)
  }
  check_1440(counts)
  if (is.null(weartime)) {
    weartime = counts > 0
  }
  weartime = as.matrix(weartime)
  uwear = unique(c(weartime))
  uwear = as.integer(uwear)
  if (!all(uwear %in% c(0, 1))) {
    stop("weartime matrix has non 0-1 data! Set NA to 0 if missing")
  }
  weartime = weartime > 0

  # transpose in case of multiple days
  # collapses correctly (by column, where column is now different days)
  data = c(t(counts))

  ### see if any missing data
  #
  na_count = !is.finite(data)
  bad = na_count & (weartime %in% TRUE)
  if (any(bad)) {
    warning("NA/NaN/Inf in counts but weartime is TRUE, setting weartime to false")
    weartime[bad] = NA
    # need this because na.omit doesn't get rid of Inf
    data[bad] = NA
  }

  data = na.omit(data)
  weartime = c(t(weartime))
  weartime = na.omit(weartime)

  # doing this for later
  weartime[ !weartime ] = NA

  y = accel.bouts(
    counts = data,
    thresh.lower = thresh.lower,
    bout.length = bout.length)

  y = y * weartime
  uy = unique(na.omit(y))
  if (length(uy) == 1) {
    stop("Only one state found in the wear data, no transition defined.")
  }
  mat = rle2(y)

  mat = as_data_frame(mat)
  mat = mat %>%
    filter(!is.na(values))


  summ = mat %>%
    group_by(values) %>%
    summarize( mean_bout = mean(lengths) )
  rest = summ %>% filter(values == 0) %>%
    select(mean_bout) %>% as.numeric
  act = summ %>% filter(values == 1) %>%
    select(mean_bout) %>% as.numeric

  frag = list(mean_rest = rest,
              mean_active = act,
              lambda_rest = 1/rest,
              lambda_active = 1/act)
  frag$bout_data = mat

  return(frag)

}

