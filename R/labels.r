# Want to label first set of columns and rows.
# 
# A block of rectangles occupies a column if they line up with a gap 
# (possibly zero width) between them.  Should be within a single level.
# 
# A row/column can be created with a bar, fluct, or spine with constant p.
# 
# Find r vals of col 1 should be less than l vals of col 2

scale_x_product <- function(df) {
  col <- find_col_level(df)
  
  if (is.na(col)) {
    # No columns, so just scatter a few tick marks around
    breaks <- seq(0, 1, length = 4)
    labels <- rep("", 4)
    scale_x_continuous("", breaks = breaks, labels = labels)
  } else {
    labels <- col_labels(df[df$level == col, ])
    
    scale_x_continuous("", breaks = labels$pos, labels =labels$label)
  }
}

#' Find the first level which has columns.
#'
#' Returns \code{NA} if no columns at any level.
#' @param df data frame of rectangle positions
find_col_level <- function(df) {
  levels <- unique(df$level)
  cols <- sapply(levels, function(i) has_cols(df[df$level == i, ]))
  
  levels[which(cols)[1]]
}

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
    with(cols, all(l[-1] >= r[-n])) &&
    length(unique(cols$nvars)) == 1
}

# Functions for rows
scale_y_product <- function(df) {
  scale <- scale_x_product(rotate(df))
  scale$.input <- "y"
  scale$.output <- "y"
  scale
}
find_row_level <- function(df) find_col_level(rotate(df))
row_labels <- function(df) col_labels(rotate(df))
has_rows <- function(df) has_cols(rotate(df))

