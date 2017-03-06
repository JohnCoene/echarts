#' Add bars
#'
#' Add bar serie.
#'
#' @examples
#' mtcars %>%
#'   echart(mpg) %>%
#'   ebar(qsec)
#'
#' mtcars %>%
#'   echart(disp) %>%
#'   ebar(mpg, stack = "grp") %>%
#'   ebar(qsec, stack = "grp") %>%
#'   ebar(wt) %>%
#'   etooltip()
#'
#' @export
ebar <- function(p, serie, stack = NULL, ...){

  serie_name <- deparse(substitute(serie))
  serie <- vector_data(serie)

  # build $serie
  opts <- list(...)
  opts$name <- serie_name
  opts$type <- "bar"
  opts$data <- serie
  opts$stack <- if(!is.null(stack)) stack

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' Add lines
#'
#' Add line serie.
#'
#' @examples
#' mtcars %>%
#'   echart(mpg) %>%
#'   eline(qsec)
#'
#' mtcars %>%
#'   echart(disp) %>%
#'   eline(mpg, stack = "grp") %>%
#'   eline(qsec, stack = "grp") %>%
#'   eline(wt, fill = TRUE) %>%
#'   etooltip()
#'
#' @export
eline <- function(p, serie, fill = FALSE, stack = NULL, ...){

  serie_name <- deparse(substitute(serie))
  serie <- vector_data(serie)

  # build $serie
  opts <- list(...)
  opts$name <- serie_name
  opts$type <- "line"
  opts$data <- serie
  opts$stack <- if(!is.null(stack)) stack
  opts$itemStyle <- if(fill == TRUE) list(normal= list(areaStyle = list(type = 'default')))

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' Add area
#'
#' Add area serie
#'
#' @examples
#' mtcars %>%
#'   echart(mpg) %>%
#'   earea(qsec)
#'
#' mtcars %>%
#'   echart(disp) %>%
#'   ebar(mpg) %>%
#'   earea(qsec, stack = "grp") %>%
#'   earea(wt, stack = "grp") %>%
#'   etooltip()
#'
#' @export
earea <- function(p, serie, stack = NULL, ...){

  serie_name <- deparse(substitute(serie))
  serie <- vector_data(serie)

  # build $serie
  opts <- list(...)
  opts$name <- serie_name
  opts$type <- "line"
  opts$data <- serie
  opts$stack <- if(!is.null(stack)) stack
  opts$itemStyle <-  list(normal= list(areaStyle = list(type = 'default')))

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' Add scatter
#'
#' Add scatter serie.
#'
#' @examples
#' mtcars %>%
#'   echart(disp) %>%
#'   escatter(mpg, qsec * 3)
#'
#' mtcars %>%
#'   echart(disp) %>%
#'   escatter(mpg, disp / 8) %>%
#'   escatter(qsec, disp / 8)
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

#' Add pie
#'
#' Add pie chart
#'
#' @examples
#' pie <- data.frame(name = c("banana", "apple", "pineapple"),
#'   value = c(40, 10, 15))
#'
#' pie %>%
#'   echart(name) %>%
#'   epie(value)
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
#' @examples
#' radar <- data.frame(axis = LETTERS[1:6], value = runif(6, 2, 10))
#'
#' radar %>%
#'   echart(axis) %>%
#'   eradar(value)
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
#' @examples
#' set.seed(19880525)
#' matrix <- matrix(sample(0:1, 100, replace=TRUE, prob=c(0.9,0.6)), nc=10)
#'
#' matrix %>%
#'   echart(LETTERS[1:10]) %>%
#'   echord()
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
#' @examples
#' choropleth <- data.frame(countries = c("France", "Brazil", "China", "Russia", "Canada", "India"),
#'   values = runif(6, 10, 25))
#'
#' choropleth %>%
#'   echart(countries) %>%
#'   echoropleth(values)
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
#' @examples
#' coords <- data.frame(city = c("London", "New York", "Beijing", "Sydney"),
#'   lon = c(-0.1167218, -73.98002, 116.3883, 151.18518),
#'   lat = c(51.49999, 40.74998, 39.92889, -33.92001))
#'
#' edges <- data.frame(source = c("Beijing", "Beijing", "New York"),
#'   target = c("Sydney", "London", "London"))
#'
#' coords %>%
#'   echart(city) %>%
#'   emap_coords(lon, lat) %>%
#'   emap_lines(edges, source, target)
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
#' @examples
#' coords <- data.frame(city = c("London", "New York", "Beijing", "Sydney"),
#'   lon = c(-0.1167218, -73.98002, 116.3883, 151.18518),
#'   lat = c(51.49999, 40.74998, 39.92889, -33.92001),
#'   values = runif(4, 10, 20))
#'
#' coords %>%
#'   echart(city) %>%
#'   emap_coords(lon, lat) %>%
#'   emap_points(values)
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

#' Add heat on map
#'
#' Add heat on map
#'
#' @examples
#' data <- data.frame(lon = runif(200, 90, 120),
#'   lat = runif(200, 30, 39),
#'   z = runif(200, 50, 75))
#'
#' data %>%
#'   echart(lon) %>%
#'   emap_heat(lat, z, "china")
#'
#' @export
emap_heat <- function(p, lat, z, mapType = "world", ...){

  opts <- list(...)
  opts$name <- "heatmap"
  opts$type <- "map"
  opts$mapType = mapType
  opts$data <- list()
  opts$heatmap <- list(
    minAlpha = 0.1,
    data = heat_map_data(y, z)
  )

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' Add gauge
#'
#' Add gauge
#'
#' @examples
#' echart() %>%
#'   egauge(85, "SPEED")
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
#' @examples
#' funnel <- data.frame(stage = c("View", "Click", "Purchase"), value = c(80, 30, 20))
#'
#' funnel %>%
#'   echart(stage) %>%
#'   efunnel(value)
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
#' @param serie a named vector, see details.
#'
#' @examples
#' venn <- data.frame(name = c("banana", "pineapple", "overlap"),
#'   values = c(20, 50, 10))
#'
#' venn %>%
#'   echart(name) %>%
#'   evenn(values)
#'
#' @export
evenn <- function(p, serie, overlap, ...){

  series_name <- deparse(substitute(serie))

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

#' add wordcloud
#'
#' @examples
#' tf <- data.frame(terms = c("ECharts", "htmlwidgets", "rstats", "htmltools"),
#'   freq = c(20, 17, 15, 7), color = c("Red", "orange", "yellow", "grey"))
#'
#' tf %>%
#'   echart(terms) %>%
#'   ecloud(freq, color)
#'
#' @export
ecloud <- function(p, freq, color, ...){

  series_name <- deparse(substitute(freq))

  opts <- list(...)
  opts$name = series_name
  opts$type = "wordCloud"
  opts$data = cloud_data(freq, color)

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' add heatmap
#'
#' @examples
#' set.seed(19880525)
#' matrix <- data.frame(x = runif(100, 10, 200), y = runif(100, 10, 200), z = runif(100, 10 , 200))
#'
#' matrix %>%
#'   echart(x) %>%
#'   eheatmap(y, z)
#'
#' @export
eheatmap <- function(p, y, z, ...){

  series_name <- deparse(substitute(z))

  opts <- list(...)
  opts$name = series_name
  opts$type = "heatmap"
  opts$data = heat_data(y, z)

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}
