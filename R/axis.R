#' Customise axis
#'
#' @param p an echart object.
#' @param show whether to show the axis.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param type type of axis takes, \code{value}, \code{category}, \code{time}, \code{log}.
#' @param append whether to append options to current axis or override.
#' @param boundaryGap whether to plot on axis line or between.
#' @param position position of axis, takes \code{bottom}, \code{top}, \code{left} or \code{right}.
#' @param name name of the axis.
#' @param nameLocation location of \code{name}, takes \code{start} or \code{end}.
#' @param nameTextStyle style of \code{name}.
#' @param min,max min and max values.
#' @param scale If \code{FALSE}, the value axis must start with 0. If \code{TRUE}, you can set the minimum and maximum value
#' of value axis as the starting and ending value of your value axis.
#' @param ... any other parameter to pass to the axis.
#' @param splitNumber number of segments, defaults to auto split along with the min/max.
#' @param logLabelBase base log.
#' @param logPositive if \code{FALSE} negative values are not supported.
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
NULL

#' @rdname yAxis
#' @export
eyAxis <- function(p, show = TRUE, type = "value", append = FALSE, ...){

  opts <- list(...)
  opts$type <- type
  opts$show <- show

  p <- add_axis(p, opts, append, axis = "yAxis")

  p
}

#' @rdname yAxis
#' @export
eyAxis_category <- function(p, show = TRUE, zlevel = 0, z = 0, boundaryGap = FALSE, append = FALSE, ...){

  opts <- axis_category(show, zlevel, z, boundaryGap, ...)

  p <- add_axis(p, opts, append, axis = "yAxis")

  p
}

#' @rdname yAxis
#' @export
eyAxis_value <- function(p, show = TRUE, zlevel = 0, z = 0, position = "left", name = NULL,
                         nameLocation = "end", nameTextStyle = list(), boundaryGap = list(0, 0),
                         min = NULL, max = NULL, scale = FALSE, splitNumber = NULL, append = FALSE, ...){

  opts <- axis_value(show, zlevel, z, position, name, nameLocation, nameTextStyle, boundaryGap,
                     min, max, scale, splitNumber, ...)

  p <- add_axis(p, opts, append, axis = "yAxis")

  p
}

#' @rdname yAxis
#' @export
eyAxis_time <- function(p, show = TRUE, zlevel = 0, z = 0, position = "left", name = NULL,
                        nameLocation = "end", nameTextStyle = list(), boundaryGap = list(0, 0),
                        min = NULL, max = NULL, scale = FALSE, splitNumber = NULL, append = FALSE, ...){

  opts <- axis_time(show, zlevel, z, position, name, nameLocation, nameTextStyle, boundaryGap,
                    min, max, scale, splitNumber, ...)

  p <- add_axis(p, opts, append, axis = "yAxis")

  p
}

#' @rdname yAxis
#' @export
eyAxis_log <- function(p, show = TRUE, zlevel = 0, z = 0, position = "left", logLabelBase = NULL,
                       logPositive = NULL, append = FALSE, ...){

  opts <- opts <- axis_log(show, zlevel, z, position, logLabelBase, logPositive, ...)

  p <- add_axis(p, opts, append, axis = "yAxis")

  p
}

#' Customize X axis
#'
#' Customise x axis.
#'
#' @param p an echart object.
#' @param show whether to show the axis.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param type type of axis takes, \code{value}, \code{category}, \code{time}, \code{log}.
#' @param append whether to append options to current axis or override.
#' @param boundaryGap whether to plot on axis line or between.
#' @param position position of axis, takes \code{bottom}, \code{top}, \code{left} or \code{right}.
#' @param name name of the axis.
#' @param nameLocation location of \code{name}, takes \code{start} or \code{end}.
#' @param nameTextStyle style of \code{name}.
#' @param min,max min and max values.
#' @param scale If \code{FALSE}, the value axis must start with 0. If \code{TRUE}, you can set the minimum and maximum value
#' of value axis as the starting and ending value of your value axis.
#' @param ... any other parameter to pass to the axis.
#' @param splitNumber number of segments, defaults to auto split along with the min/max.
#' @param logLabelBase base log.
#' @param logPositive if \code{FALSE} negative values are not supported.
#'
#' @examples
#' mtcars$models <- row.names(mtcars)
#'
#' mtcars[1:10,] %>%
#'   echart(models) %>%
#'   eline(mpg) %>%
#'   exAxis_category() %>%
#'   eyAxis_value(min = 10, append = FALSE, scale = TRUE)
#'
#' @name xAxis
#' @rdname xAxis
NULL

#' Customize X axis
#'
#' @rdname xAxis
#' @export
exAxis <- function(p, show = TRUE, type = "value", append = FALSE, ...){

  opts <- list(...)
  opts$type <- type
  opts$show <- show

  p <- add_axis(p, opts, append, "xAxis")

  p
}

#' @rdname xAxis
#' @export
exAxis_category <- function(p, show = TRUE, zlevel = 0, z = 0, boundaryGap = FALSE, append = FALSE, ...){

  opts <- axis_category(show, zlevel, z, boundaryGap, ...)
  opts$data <- p$x$options$xAxis[[1]]$data

  p <- add_axis(p, opts, append, axis = "xAxis")

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

  p <- add_axis(p, opts, append, axis = "xAxis")

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

  p <- add_axis(p, opts, append, axis = "xAxis")

  p

}

#' @rdname xAxis
#' @export
exAxis_log <- function(p, show = TRUE, zlevel = 0, z = 0, position = "bottom", logLabelBase = NULL,
                       logPositive = NULL, append = FALSE, ...){

  opts <- axis_log(show, zlevel, z, position, logLabelBase, logPositive, ...)
  opts$data <- p$x$options$xAxis[[1]]$data

  p <- add_axis(p, opts, append, axis = "xAxis")

  p
}
