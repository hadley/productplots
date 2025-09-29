.directions <- c("vertical", "horizontal")

#' Template for a mosaic plot.
#' A mosaic plot is composed of spines in alternating directions.
#'
#' @param direction direction of first split
#' @export
mosaic <- function(direction = "v") {
  direction <- match.arg(direction, .directions)
  if (direction == "horizontal") {
    splits <- c("hspine", "vspine")
  } else {
    splits <- c("vspine", "hspine")
  }

  function(n) rep(splits, length.out = n)
}

#' Template for a stacked bar chart.
#' A stacked bar chart starts with a bar and then continues with spines in the
#' opposite direction.
#'
#' @param direction direction of first split
#' @export
stacked <- function(direction = "h") {
  direction <- match.arg(direction, .directions)
  if (direction == "horizontal") {
    splits <- c("hbar", "vspine")
  } else {
    splits <- c("vbar", "hspine")
  }

  function(n) c(splits[1], rep(splits[2], length.out = n - 1))
}

#' Template for a nested barchart.
#' A nested bar is just a sequence of bars in the same direction.
#'
#' @param direction direction of first split
#' @export
nested <- function(direction = "h") {
  direction <- match.arg(direction, .directions)
  if (direction == "horizontal") {
    splits <- c("hbar")
  } else {
    splits <- c("vbar")
  }

  function(n) rep(splits, length.out = n)
}

#' Template for a double decker plot.
#' A double decker plot is composed of a sequence of spines in the same
#' direction, with the final spine in the opposite direction.
#'
#' @param direction direction of first split
#' @export
ddecker <- function(direction = "h") {
  direction <- match.arg(direction, .directions)
  if (direction == "horizontal") {
    splits <- c("hspine", "vspine")
  } else {
    splits <- c("vspine", "hspine")
  }

  function(n) c(rep(splits[1], length.out = n - 1), splits[2])
}

#' Template for a fluctuation diagram.
#'
#' @param direction direction of first split
#' @export
flucts <- function(direction = "h") {
  direction <- match.arg(direction, .directions)
  if (direction == "horizontal") {
    splits <- c("hspine", "vspine")
  } else {
    splits <- c("vspine", "hspine")
  }
  function(n) c(rep(splits, length.out = n - 2), "fluct")
}
