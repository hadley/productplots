# Want to label first set of columns and rows.
# 
# A block of rectangles occupies a column if they line up with a gap 
# (possibly zero width) between them.  Should be within a single level.
# 
# A row/column can be created with a bar, fluct, or spine with constant p.
# 
# Find r vals of col 1 should be less than l vals of col 2

scale_x_product <- function(df) {
  levels <- unique(df$level)
  cols <- sapply(levels, function(i) has_cols(df[df$level == i, ]))
  
  col <- levels[which(cols)[1]]
  
  if (is.na(col)) {
    scale_x_continuous(breaks = seq(0, 1, length = 4), labels = rep("", 4))
  } else {
    labels <- col_labels(df[df$level == col, ])
    
    scale_x_continuous("", breaks = labels$pos, labels = as.character(labels$label))
  }
}

scale_y_product <- function(df) {
  scale <- scale_x_product(rotate(df))
  scale$.input <- "y"
  scale$.output <- "y"
  scale
}

col_labels <- function(df) {
  vars <- setdiff(names(df), c(".wt", "l", "r", "t", "b", "level"))
  
  df <- ddply(df, "l", function(df) {
    # If width is constant, draw in the middle, otherwise draw on the left.
    widths <- df$r - df$l
    w <- unique(widths[widths != 0])
    if (length(w) == 1) {
      pos <- df$l[1] + w / 2
    } else {
      pos <- df$l[1]
    }
     
    data.frame(pos, label = df[vars[1], ])
  })
}

row_labels <- function(df) {
  col_labels(rotate(df))
}

has_cols <- function(df) {
  cols <- ddply(df, "l", summarise, r = max(r))
  nrow(cols) > 1 && all(cols$l[-1] - cols$r[-nrow(cols)] >= 0)  
}

has_rows <- function(df) {
  has_cols(rotate(df))
}

# Find the first levels for have rows or columns
find_label_levels <- function(df) {
  levels <- seq_len(max(df$level))
  
  rows <- sapply(levels, function(i) has_rows(df[df$level == i, ]))
  cols <- sapply(levels, function(i) has_cols(df[df$level == i, ]))
  
  c(row = which(rows)[1], col = which(cols)[1])
}
