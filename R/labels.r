# hbar: constant width, varying height
# vbar: constant height, varying width
# hspine: single height, varying width
# vspine: single width, varying height

make_labels <- function(df) { 
  df$level <- df$level - min(df$level) + 1
  top <- subset(df, level == 1)
  
  w_single <- is_constant(ninteraction(top[c("l", "r")]))
  h_single <- is_constant(ninteraction(top[c("b", "t")]))

  w_constant <- with(top, is_constant(r - l)) && !w_single
  h_constant <- with(top, is_constant(t - b)) && !h_single

  if (w_constant && h_constant) {
    list(xlab(NULL), ylab(NULL))
  } else if (w_constant  || h_single) {
    # hbar or hspine: label x axis
    pos <- with(top, (l + r) / 2)
    list(
      scale_x_continuous(breaks = pos, labels = top[, 1]),
      xlab(NULL)
    )
  } else if (h_constant || w_single) {
    # vbar or vspine: label y axis    
    pos <- with(top, (b + t) / 2)
    list(
      scale_y_continuous(breaks = pos, labels = top[, 1]),
      ylab(NULL)
    )
  } else {
    # Fluctuation diagram or treemap
    list(xlab(NULL), ylab(NULL))
    
  }
}

is_constant <- function(x) sd(x, na.rm = TRUE) < 1e-8

# Alternative: use original data and partition specification to compute.
# Probably need to make partitions objects, so can ask for labelling, or
# at least name or direction.