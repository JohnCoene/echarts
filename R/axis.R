#' Customise axis
#'
#' @name axis
#' @rdname axis
#' @export
NULL

#' @rdname axis
#' @export
eyAxis_category <- function(p, data, show = TRUE, zlevel = 0, z = 0, boundaryGap = FALSE, ...){

  opts <- list(...)
  opts$type <- "category"
  opts$show <- show
  opts$zlevel <- zlevel
  opts$z <- z
  opts$boundaryGap <- boundaryGap
  opts$data <- if(!missing(data)) data

  p$x$options$yAxis <- opts

  p
}

#' @rdname axis
#' @export
eyAxis_value <- function(p, data, show = TRUE, zlevel = 0, z = 0, position = "left", name = NULL,
                         nameLocation = "end", nameTextStyle = list(), boundaryGap = list(0, 0),
                         min = NULL, max = NULL, scale = FALSE, splitNumber = NULL, ...){

  opts <- list(...)
  opts$type <- "value"
  opts$show <- show
  opts$zlevel <- zlevel
  opts$z <- z
  opts$position <- position
  opts$data <- if(!missing(data)) data
  opts$name <- name
  opts$nameLocation <- nameLocation
  opts$nameTextStyle <- nameTextStyle
  opts$boundaryGap <- boundaryGap
  opts$min <- min
  opts$max <- max
  opts$scale <- scale
  opts$splitNumber <- splitNumber

  p$x$options$yAxis <- opts

  p
}

#' @rdname axis
#' @export
eyAxis_time <- function(p, data, show = TRUE, zlevel = 0, z = 0, position = "left", name = NULL,
                         nameLocation = "end", nameTextStyle = list(), boundaryGap = list(0, 0),
                         min = NULL, max = NULL, scale = FALSE, splitNumber = NULL, ...){

  opts <- list(...)
  opts$type <- "time"
  opts$show <- show
  opts$zlevel <- zlevel
  opts$z <- z
  opts$position <- position
  opts$data <- if(!missing(data)) data
  opts$name <- name
  opts$nameLocation <- nameLocation
  opts$nameTextStyle <- nameTextStyle
  opts$boundaryGap <- boundaryGap
  opts$min <- min
  opts$max <- max
  opts$scale <- scale
  opts$splitNumber <- splitNumber

  p$x$options$yAxis <- opts

  p
}
