#' Customise axis
#'
#' @examples
#' df <- data.frame(x = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
#'   y = runif(7, 1, 5))
#'
#' df %>%
#'   echart(x) %>%
#'   eline(y) %>%
#'   exAxis_category(boundaryGap = FALSE)
#'
#' df %>%
#'   echart(x) %>%
#'   ebar(y) %>%
#'   eyAxis_log()
#'
#' @name yAxis
#' @rdname yAxis
#' @export
eyAxis <- function(p, show = TRUE, type = "value", append = FALSE, ...){

  opts <- list(...)
  opts$type <- type
  opts$show <- show

  p <- add_axis(p, opts, append)

  p
}

#' @rdname yAxis
#' @export
eyAxis_category <- function(p, show = TRUE, zlevel = 0, z = 0, boundaryGap = FALSE, append = FALSE, ...){

  opts <- axis_category(show, zlevel, z, boundaryGap, ...)

  p$x$options$yAxis <- list(opts)

  p
}

#' @rdname yAxis
#' @export
eyAxis_value <- function(p, show = TRUE, zlevel = 0, z = 0, position = "left", name = NULL,
                         nameLocation = "end", nameTextStyle = list(), boundaryGap = list(0, 0),
                         min = NULL, max = NULL, scale = FALSE, splitNumber = NULL, append = FALSE, ...){

  opts <- axis_value(show, zlevel, z, position, name, nameLocation, nameTextStyle, boundaryGap,
                     min, max, scale, splitNumber, ...)

  p <- add_axis(p, opts, append)

  p
}

#' @rdname yAxis
#' @export
eyAxis_time <- function(p, show = TRUE, zlevel = 0, z = 0, position = "left", name = NULL,
                        nameLocation = "end", nameTextStyle = list(), boundaryGap = list(0, 0),
                        min = NULL, max = NULL, scale = FALSE, splitNumber = NULL, append = FALSE, ...){

  opts <- axis_time(show, zlevel, z, position, name, nameLocation, nameTextStyle, boundaryGap,
                    min, max, scale, splitNumber, ...)

  p <- add_axis(p, opts, append)

  p
}

#' @rdname yAxis
#' @export
eyAxis_log <- function(p, show = TRUE, zlevel = 0, z = 0, position = "left", logLabelBase = NULL,
                       logPositive = NULL, append = FALSE, ...){

  opts <- opts <- axis_log(show, zlevel, z, position, logLabelBase, logPositive, ...)

  p$x$options$yAxis <- list(opts)

  p
}

#' Customize X axis
#'
#' @name xAxis
#' @rdname xAxis
#' @export
exAxis <- function(show = TRUE, type = "value", ...){

  opts <- list(...)
  opts$type <- type
  opts$show <- show
  opts$data <- p$x$options$xAxis[[1]]$data

  p <- add_axis(p, opts, append)

  p
}

#' @rdname xAxis
#' @export
exAxis_category <- function(p, show = TRUE, zlevel = 0, z = 0, boundaryGap = FALSE, append = FALSE, ...){

  opts <- axis_category(show, zlevel, z, boundaryGap, ...)
  opts$data <- p$x$options$xAxis[[1]]$data

  p <- add_axis(p, opts, append)

  p
}


#' @rdname xAxis
#' @export
exAxis_value <- function(p, show = TRUE, zlevel = 0, z = 0, position = "bottom", name = NULL,
                         nameLocation = "end", nameTextStyle = list(), boundaryGap = list(0, 0),
                         min = NULL, max = NULL, scale = FALSE, splitNumber = NULL, append = FALSE, ...){

  opts <- axis_value(show, zlevel, z, position, name, nameLocation, nameTextStyle, boundaryGap,
                     min, max, scale, splitNumber, ...)
  opts$data <- p$x$options$xAxis[[1]]$data

  p <- add_axis(p, opts, append)

  p

}

#' @rdname xAxis
#' @export
exAxis_time <- function(p, show = TRUE, zlevel = 0, z = 0, position = "bottom", name = NULL,
                        nameLocation = "end", nameTextStyle = list(), boundaryGap = list(0, 0),
                        min = NULL, max = NULL, scale = FALSE, splitNumber = NULL, append = FALSE, ...){

  opts <- axis_time(show, zlevel, z, position, name, nameLocation, nameTextStyle, boundaryGap,
                    min, max, scale, splitNumber, ...)
  opts$data <- p$x$options$xAxis[[1]]$data

  p <- add_axis(p, opts, append)

  p

}

#' @rdname xAxis
#' @export
exAxis_log <- function(p, show = TRUE, zlevel = 0, z = 0, position = "bottom", logLabelBase = NULL,
                       logPositive = NULL, append = FALSE, ...){

  opts <- axis_log(show, zlevel, z, position, logLabelBase, logPositive, ...)
  opts$data <- p$x$options$xAxis[[1]]$data

  p <- add_axis(p, opts, append)

  p
}
