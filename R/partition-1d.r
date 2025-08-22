rotate <- function(data) {
  name_mapping <- c("l" = "b", "r" = "t", "b" = "l", "t" = "r")
  mapped_idx <- names(data) %in% names(name_mapping)
  if (!all(mapped_idx)) {
    message(
      "Did not find these columns to rename: ",
      toString(setdiff(names(data), names(name_mapping)))
    )
  }
  rename_idx <- names(data) %in% names(name_mapping)
  names(data)[rename_idx] <- name_mapping[names(data)[rename_idx]]
  dup_id <- anyDuplicated(names(data))
  if (dup_id > 0L) {
    warning("Rename operation created duplicate names, e.g. ", names(data)[dup_id])
  }
  data
}

#' Spine partition: divide longest dimesion.
#'
#' @param data bounds data frame
#' @param bounds bounds of space to partition
#' @param offset space between spines
#' @param max maximum value
#' @export
spine <- function(data, bounds, offset = 0.01, max = NULL) {
  w <- bounds$r - bounds$l
  h <- bounds$t - bounds$b

  if (w > h) {
    hspine(data, bounds, offset, max)
  } else {
    vspine(data, bounds, offset, max)
  }
}


#' Horizontal spine partition: height constant, width varies.
#'
#' @param data bounds data frame
#' @param bounds bounds of space to partition
#' @param offset space between spines
#' @param max maximum value
#' @export
hspine <- function(data, bounds, offset = 0.01, max = NULL) {
  n <- length(data)
  # n + 1 offsets
  offsets <- c(0, rep(1, n - 1), 0) * offset

  data <- data * (1 - sum(offsets))

  widths <- as.vector(t(cbind(data, offsets[-1])))
  widths[is.na(widths)] <- 0

  pos <- c(offsets[1], cumsum(widths)) / sum(widths)
  locations <- data.frame(
    l = pos[seq(1, 2 * n - 1, by = 2)],
    r = pos[seq(2, 2 * n, by = 2)],
    b = 0,
    t = 1
  )
  squeeze(locations, bounds)
}

#' Vertical spine partition: width constant, height varies.
#'
#' @param data bounds data frame
#' @param bounds bounds of space to partition
#' @param offset space between spines
#' @param max maximum value
#' @export
vspine <- function(data, bounds, offset = 0.01, max = NULL) {
  rotate(hspine(data, rotate(bounds), offset, max = max))
}

#' Horizontal bar partition: width constant, height varies.
#'
#' @param data bounds data frame
#' @param bounds bounds of space to partition
#' @param offset space between spines
#' @param max maximum value
#' @export
hbar <- function(data, bounds, offset = 0.02, max = NULL) {
  if (is.null(max)) max <- 1

  n <- length(data)
  # n + 1 offsets
  offsets <- c(0, rep(1, n - 1), 0) * offset

  width <- (1 - sum(offsets)) / n
  heights <- data / max

  widths <- as.vector(t(cbind(width, offsets[-1])))
  pos <- c(offsets[1], cumsum(widths)) / sum(widths)
  locations <- data.frame(
    l = pos[seq(1, 2 * n - 1, by = 2)],
    r = pos[seq(2, 2 * n, by = 2)],
    b = 0,
    t = heights
  )
  squeeze(locations, bounds)
}

#' Vertical bar partition: height constant, width varies.
#'
#' @param data bounds data frame
#' @param bounds bounds of space to partition
#' @param offset space between spines
#' @param max maximum value
#' @export
vbar <- function(data, bounds, offset = 0.02, max = NULL) {
  rotate(hbar(data, rotate(bounds), offset, max = max))
}
