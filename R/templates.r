.directions <- c("vertical", "horizontal")

#' @export
mosaic <- function(direction = "h") {
  direction <- match.arg(direction, .directions)
  if (direction == "horizontal") {
    splits <- c("hspine", "vspine")
  } else {
    splits <- c("vspine", "hspine")
  }
  
  function(n) rep(splits, length = n)
}

#' @export
stacked <- function(direction = "h") {
  direction <- match.arg(direction, .directions)
  if (direction == "horizontal") {
    splits <- c("hbar", "vspine")
  } else {
    splits <- c("vbar", "hspine")
  }

  function(n) c(splits[1], rep(splits[2], length = n - 1))
}

#' @export
nested <- function(direction = "h") {
  direction <- match.arg(direction, .directions)
  if (direction == "horizontal") {
    splits <- c("hbar")
  } else {
    splits <- c("vbar")
  }

  function(n) rep(splits, length = n)
}

#' @export
ddecker <- function(direction = "h") {
  direction <- match.arg(direction, .directions)
  if (direction == "horizontal") {
    splits <- c("hspine", "vspine")
  } else {
    splits <- c("vspine", "hspine")
  }

  function(n) c(rep(splits[1], length = n - 1), splits[2])
}

#' @export
flucts <- function(direction = "h") {
  direction <- match.arg(direction, .directions)
  if (direction == "horizontal") {
    splits <- c("hspine", "vspine")
  } else {
    splits <- c("vspine", "hspine")
  }
  function(n) c(rep(splits, length = n - 2), "fluct")
}