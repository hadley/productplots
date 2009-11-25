margin <- function(table, marginals = c(), conditionals = c()) {
  if (is.numeric(marginals))    marginals    <- names(table)[marginals]
  if (is.numeric(conditionals)) conditionals <- names(table)[conditionals]
  
  marginals <- as.quoted(marginals)
  conditionals <- as.quoted(conditionals)
  
  # If no weight column, give constant weight
  if (is.null(table$.wt)) {
    table$.wt <- 1 / nrow(table)
  }
  
  marg <- ddply(table, c(conditionals, marginals), colwise(sum, c(".wt")), .drop = FALSE)
  if (length(conditionals) > 0) {
    ddply(marg, conditionals, transform, .wt = .wt / sum(.wt, na.rm = TRUE), .drop = FALSE)
  } else {
    marg
  }
}


divide <- function(data, bounds = bound(), divider = list(hbar), level = 1, cascade = 0, max_wt = NULL) {
  d <- partd(divider[[1]])
  if (ncol(data) == d + 1) {
    # End case of recursion
    
    if (d > 1) {
      margins <- paste(names(data)[-ncol(data)], collapse = " ~ ")
      wt <- as.matrix(cast(data, margins, value = ".wt"))      
    } else {
      wt <- data$.wt
    }
    
    if (is.null(max_wt)) max_wt <- max(wt)

    partition <- divider[[1]](wt, max = max_wt)
    cbind(data, squeeze(partition, bounds), level = level)
  } else {
    parent <- divide(margin(data, seq_len(d)), bounds, divider, level,
      max = max_wt)
    parentc <- transform(parent, 
      xmin = xmin + cascade, ymin = ymin + cascade, 
      xmax = xmax + cascade, ymax = ymax + cascade)
    
    if (is.null(max_wt)) {
      max_wt <- max(margin(data, d + 1, seq_len(d))$.wt)      
    }
    
    pieces <- as.list(dlply(data, seq_len(d)))
    children <- ldply(seq_along(pieces), function(i) {
      piece <- pieces[[i]]
      partition <- divide(piece[, -seq_len(d)], parentc[i, ], divider[-1], 
        level = level + 1, cascade = cascade, max = max_wt)
      cbind(piece[rep(1, nrow(partition)), 1, drop = FALSE], partition)
    })
    rbind.fill(parent, children)
  }
}


prodcalc <- function(data, vars, conditions = c(), divider = mosaic(), cascade = 0, scale_max = TRUE) {
  wt <- margin(data, vars, conditions)

  if (is.function(divider)) divider <- divider(ncol(wt) - 1)
  if (is.character(divider)) divider <- llply(divider, match.fun)
  
  max_wt <- if (scale_max) NULL else 1
  
  divide(wt, divider = divider, cascade = cascade, max_wt = max_wt)
}

prodplot <- function(data, vars, conditions = c(), divider = mosaic(), cascade = 0, scale_max = TRUE, ...) {
  res <- prodcalc(data, vars, conditions, divider, cascade, scale_max)
  draw(res, ...)
}

prodcheck <- function(data, vars, conditions = c(), divider = mosaic(), cascade = 0, scale_max = TRUE, ...) {
  res <- add_area(prodcalc(data, vars, conditions, divider, cascade, scale_max))
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
