source("templates.r")
source("partition.r")

margin <- function(table, marginals = c(), conditionals = c()) {
  if (is.numeric(marginals))    marginals    <- names(table)[marginals]
  if (is.numeric(conditionals)) conditionals <- names(table)[conditionals]
  
  marginals <- as.quoted(marginals)
  conditionals <- as.quoted(conditionals)
  
  # If no weight column, give constant weight
  if (is.null(table$.wt)) {
    table$.wt <- 1 / nrow(table)
  }
  
  marg <- ddply(table, c(conditionals, marginals), colwise(sum, c(".wt")))
  if (length(conditionals) > 0) {
    ddply(marg, conditionals, transform, .wt = .wt / sum(.wt, na.rm = TRUE))
  } else {
    marg
  }
}


divide <- function(data, bounds = bound(), divider = hbar, level = 1, cascade = 0, max = NULL) {
  if (ncol(data) == 2) {
    partition <- divider[[1]](data$.wt, max = max)
    cbind(data, squeeze(partition, bounds), level = level)
  } else {
    parent <- divide(margin(data, 1), bounds, divider, level, max = max)
    parentc <- transform(parent, 
      xmin = xmin + cascade, ymin = ymin + cascade, 
      xmax = xmax + cascade, ymax = ymax + cascade)
    
    max_wt <- max(margin(data, 2, 1)$.wt)
    
    pieces <- as.list(dlply(data, 1))
    children <- ldply(seq_along(pieces), function(i) {
      piece <- pieces[[i]]
      partition <- divide(piece[, -1], parentc[i, ], divider[-1], 
        level = level + 1, cascade = cascade, max = max_wt)
      cbind(piece[rep(1, nrow(partition)), 1, drop = FALSE], partition)
    })
    rbind.fill(parent, children)
  }
}


prodcalc <- function(data, vars, conditions = c(), divider = mosaic(), cascade = 0) {
  wt <- margin(data, vars, conditions)

  if (is.function(divider)) divider <- divider(ncol(wt) - 1)
  if (is.character(divider)) divider <- llply(divider, match.fun)
  
  divide(wt, divider = divider, cascade = cascade)
}

prodplot <- function(data, vars, conditions = c(), divider = mosaic(), cascade = 0, ...) {
  res <- prodcalc(data, vars, conditions, divider, cascade)
  draw(res, ...)
}

prodcheck <- function(data, vars, conditions = c(), divider = mosaic(), cascade = 0, ...) {
  res <- add_area(prodcalc(data, vars, conditions, divider, cascade))
  qplot(.wt, area, data = res) + 
    facet_wrap(~ level, scales = "free") +
    geom_smooth(method = lm, colour = "grey50")
}


# hdata <- read.csv("happy.csv")
# prodplot(hdata, ~ happy, divider = list(hbar))
# prodplot(hdata, ~ happy + sex, divider = list(hbar,  hbar)) + aes(fill=sex)
# prodplot(hdata, ~ sex, ~happy, divider = list(hspline,  hbar))
# prodplot(hdata, ~ happy + sex, divider = list(hbar,  vspline)) +
#  aes(fill=sex)
# prodplot(hdata, ~ happy + sex + degree, divider = mosaic())
# prodplot(hdata, ~ happy + sex + degree, divider = mosaic(), cascade = 0.01) 

# prodplot(hdata, ~ happy + sex + degree, divider = mosaic(), 
#   cascade = 0.02, alpha = 0.8) + 
#   aes(fill= factor(level)) + scale_fill_grey()
