#' add bars
#'
#' @export
e_bar <- function(p, serie, type = "bar", ...){

  serie_name <- deparse(substitute(serie))
  serie <- vector_data(serie)

  # build $serie
  opts <- list(...)
  opts$name <- serie_name
  opts$type <- type
  opts$data <- serie

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' add lines
#'
#' @export
e_line <- function(p, serie, ...){

  serie_name <- deparse(substitute(serie))
  serie <- vector_data(serie)

  # build $serie
  opts <- list(...)
  opts$name <- serie_name
  opts$type <- "line"
  opts$data <- serie

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' add scatter
#'
#' @export
e_scatter <- function(p, serie, size, ...){

  serie_name <- deparse(substitute(serie))
  serie <- scatter_data(serie, size)

  # build $serie
  opts <- list(...)
  opts$name <- serie_name
  opts$type <- "scatter"
  opts$data <- serie
  opts$symbolSize <- if(!missing(size)){htmlwidgets::JS("function (value){ return Math.round(value[2] / 5);}")}

  p$x$options$xAxis[[1]]$data <- NULL
  p$x$options$xAxis[[1]]$type <- "value"
  p$x$options$yAxis <- list(list(type = "value"))
  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' add pie
#'
#' @export
e_pie <- function(p, serie, ...){

  serie_name <- deparse(substitute(serie))
  serie <- val_name_data(serie)

  # build $serie
  opts <- list(...)
  opts$name <- serie_name
  opts$type <- "pie"
  opts$data <- serie

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' add radar
#'
#' @export
e_radar <- function(p, serie, ...){

  serie_name <- deparse(substitute(serie))
  serie <- vector_data(serie)
  serie <- list(value = serie, name = serie_name)

  # build $serie
  opts <- list(...)
  opts$name <- serie_name
  opts$type <- "radar"
  opts$data <- list(serie)

  # set polar $indicator
  p$x$options$polar <- list(
    list(
      indicator = polar_indicator()
    )
  )

  # remove axis
  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' Add chord
#'
#' @export
e_chord <- function(p, sort = "ascending", sortSub = "descending", ...){

  opts <- list(...)
  opts$type <- "chord"
  opts$sort <- sort
  opts$sortSub <- sortSub
  opts$data <- chord_data()
  opts$matrix <- chord_matrix()

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' Add choropleth
#'
#' @export
e_choropleth <- function(p, serie, mapType = "USA"){

  serie_name <- deparse(substitute(serie))

  opts <- list(...)
  opts$name <- serie_name
  opts$type <- "map"
  opts$data <- val_name_data(serie)

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}
