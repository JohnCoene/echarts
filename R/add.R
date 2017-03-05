#' add bars
#'
#' @export
ebar <- function(p, serie, type = "bar", ...){

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
eline <- function(p, serie, ...){

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
escatter <- function(p, serie, size, ...){

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
epie <- function(p, serie, ...){

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
eradar <- function(p, serie, ...){

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
echord <- function(p, sort = "ascending", sortSub = "descending", ...){

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
echoropleth <- function(p, serie, mapType = "world", dataRange, ...){

  serie_name <- deparse(substitute(serie))

  opts <- list(...)
  opts$name <- serie_name
  opts$type <- "map"
  opts$mapType = mapType
  opts$data <- val_name_data(serie)

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  if(missing(dataRange)){
    dataRange <- default_dataRange(serie)
  }

  p$x$options$dataRange <- dataRange

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' Add map coordinates
#'
#' Add coordinates to map
#'
#' @export
emap_coords <- function(p, lon, lat, mapType = "world", ...){

  opts <- list(...)
  opts$name <- "coords"
  opts$type <- "map"
  opts$mapType = mapType
  opts$data <- list()
  opts$geoCoord <- build_coord(lon, lat)

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' Add map lines
#'
#' Add lines on map
#'
#' @export
emap_lines <- function(p, edges, source, target){

  opts <- list()
  opts$smooth <- TRUE
  opts$effect = list(
    show = TRUE,
    scaleSize = 1,
    period = 30,
    color = "#fff",
    shadowBlur = 10
  )
  opts$data <- map_lines(edges, source, target)

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  previous <- length(p$x$options$series)

  p$x$options$series[[previous]]$markLine = opts

  p
}

#' Add map points
#'
#' Add map points
#'
#' @export
emap_points <- function(p, serie){

  data <- get("data", envir = data_env)
  serie <- eval(substitute(serie), data)

  opts <- list()
  opts$symbol = 'emptyCircle'
  opts$symbolSize = htmlwidgets::JS(" function (v){ return 10 + v/10 }")
  opts$effect = list(show = TRUE, shadowBlur = 0)
  opts$itemStyle = list(normal = list(label = list(show = FALSE)))
  opts$data = val_name_data(serie)

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  previous <- length(p$x$options$series)

  p$x$options$series[[previous]]$markPoint = opts

  p
}

#' Add gauge
#'
#' Add gauge
#'
#' @export
egauge <- function(p, value, name = "gauge", ...){

  opts <- list(...)
  opts$name = name
  opts$type = "gauge"
  opts$data = list(list(value = value, name = name))

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' Add funnel
#'
#' Add funnel
#'
#' @export
efunnel <- function(p, serie, ...){

  data <- get("data", envir = data_env)
  series_name <- deparse(substitute(serie))
  serie <- eval(substitute(serie), data)

  opts <- list(...)
  opts$name = series_name
  opts$type = "funnel"
  opts$data = val_name_data(serie)

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' Add venn
#'
#' Add venn diagram
#'
#' @export
evenn <- function(p, val1, val2, overlap, ...){

  data <- get("data", envir = data_env)
  series_name <- deparse(substitute(serie))
  serie <- eval(substitute(serie), data)

  opts <- list(...)
  opts$name = series_name
  opts$type = "venn"
  opts$itemStyle = list(
    normal = list(
      label = list(
        show = TRUE
      )
    )
  )
  opts$data = val_name_data(serie)

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}
