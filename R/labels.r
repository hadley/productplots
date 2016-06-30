# Want to label first set of columns and rows.
#
# A block of rectangles occupies a column if they line up with a gap
# (possibly zero width) between them.  Should be within a single level.
#
# A row/column can be created with a bar, fluct, or spine with constant p.
#
# Find r vals of col 1 should be less than l vals of col 2

#' Generate an x-scale for ggplot2 graphics.
#'
#' @param df list of data frame produced by \code{\link{prodcalc}}, formula and divider
#' @importFrom ggplot2 scale_x_continuous
#' @export
scale_x_product <- function(df) {
  l <- b <- r <- NULL # initialization
  data <- df$data
  vars <- parse_product_formula(df$formula)

  ## horizontal axis there if dividers contain "h":
  col <- c(vars$marg, vars$cond)[grep("h", df$divider)]
  ##  col <- find_col_level(data)
  if (length(col) == 0) {
    # No columns, so just scatter a few tick marks around
    breaks <- seq(0, 1, length = 5)
#    labels <- rep("", 5)
    scale_x_continuous("", breaks = breaks, labels = round(breaks,2))
  } else {
#    labels <- col_labels(data[data$level == col, ])
    labels <- subset(data, (level = max(level)) & (b==0))
    labels$pos <- with(labels, (l+r)/2)
    labels$label <- ldply(1:nrow(labels), function(x) paste(unlist(labels[x,col]), collapse=":"))$V1
    xlabel <- paste(col, collapse=":")

    scale_x_continuous(xlabel, breaks = labels$pos, labels =labels$label)
  }
}

#' Find the first level which has columns.
#'
#' Returns \code{NA} if no columns at any level.
#' @param df data frame of rectangle positions
#' @export
find_col_level <- function(df) {
  levels <- unique(df$level)
  cols <- sapply(levels, function(i) has_cols(df[df$level == i, ]))

  levels[which(cols)[1]]
}

#' Calculate column labels.
#'
#' @keywords  internal
#' @param df data frame produced by \code{\link{prodcalc}}
#' @export
col_labels <- function(df) {
  vars <- setdiff(names(df), c(".wt", "l", "r", "t", "b", "level"))

  ddply(df, "l", function(df) {
    # If width is constant, draw in the middle, otherwise draw on the left.
    widths <- df$r - df$l
    widths <- widths[widths != 0]
    constant <- length(widths) != 0 &&
      (length(unique(widths)) <= 1 || cv(widths, T) < 0.01)

    if (constant) {
      pos <- df$l[1] + widths[1] / 2
    } else {
      pos <- df$l[1]
    }

    data.frame(pos, label = uniquecols(df[vars])[, 1])
  })
}

has_cols <- function(df) {
  vars <- setdiff(names(df), c(".wt", "l", "r", "t", "b", "level"))

  cols <- ddply(df, "l", function(df) {
    data.frame(r = max(df$r), nvars = ncol(uniquecols(df[vars])))
  })

  n <- nrow(cols)

  # Has colums if:
  #  * more than 1 column
  #  * right boundary of each column less than left boundary of next column
  #  * number of variables in each column is the same
  n > 1 &&
    all(cols$l[-1] >= cols$r[-n]) &&
    length(unique(cols$nvars)) == 1
}

# Functions for rows

#' Generate a y-scale for ggplot2 graphics.
#'
#' @param df list of data frame produced by \code{\link{prodcalc}}, formula and divider
#' @importFrom ggplot2 scale_y_continuous
#' @export
# scale_y_product <- function(df) {
#   scale <- scale_x_product(rotate(df))
#   scale$.input <- "y"
#   scale$.output <- "y"
#   scale
# }
scale_y_product <- function(df) {
#  browser()
  l <- b <- r <- NULL # initialization
  data <- df$data
  vars <- parse_product_formula(df$formula)

  ## horizontal axis there if dividers contain "v":
  col <- c(vars$marg, vars$cond)[grep("v", df$divider)]
  ##  col <- find_col_level(data)
  if (length(col) == 0) {
    # No columns, so just scatter a few tick marks around
    breaks <- seq(0, 1, length = 5)
    scale_y_continuous("", breaks = breaks, labels = round(breaks,2))
  } else {
    labels <- subset(data, (level = max(level)) & (l==0))
    labels$pos <- with(labels, (b+t)/2)
    labels$label <- ldply(1:nrow(labels), function(x) paste(unlist(labels[x,col]), collapse=":"))$V1
    ylabel <- paste(col, collapse=":")

    scale_y_continuous(ylabel, breaks = labels$pos, labels =labels$label)
  }
}

#' Find the first level which has rows.
#'
#' Returns \code{NA} if no rows at any level.
#' @param df data frame of rectangle positions
#' @export
find_row_level <- function(df) find_col_level(rotate(df))

#' Calculate row labels.
#'
#' @param df data frame produced by \code{\link{prodcalc}}
#' @keywords  internal
#' @export
row_labels <- function(df) col_labels(rotate(df))
has_rows <- function(df) has_cols(rotate(df))

