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
#'   ebar(mpg, stack = "grp") %>% # stack
#'   ebar(qsec, stack = "grp") %>% # stack
#'   ebar(wt) %>% # not stacked
#'   etooltip() %>%
#'   elegend()
#'
#' @export
ebar <- function(p, serie, name = NULL, stack = NULL, clickable = TRUE, xAxisIndex = 0, yAxisIndex = 0, barGap = "100%",
                 barCategoryGap = "20%", legendHoverLink = TRUE, z = 2, zlevel = 0, tooltip, itemStyle,
                 barWidth, barMaxWidth, ...){

  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "axis")

  name <- ifelse(is.null(name), deparse(substitute(serie)), name)

  # build $serie
  opts <- list(...)
  opts$name <- name
  opts$type <- "bar"
  opts$data <- vector_data(serie)
  opts$stack <- if(!is.null(stack)) stack
  opts$clickable <- clickable
  opts$xAxisIndex <- xAxisIndex
  opts$yAxisIndex <- yAxisIndex
  opts$barGap <- barGap
  opts$barCategory <- barCategoryGap
  opts$legendHoverLink <- legendHoverLink
  opts$z <- z
  opts$zlevel <- zlevel
  opts$tooltip <- tooltip
  opts$itemStyle <- if(!missing(itemStyle)) itemStyle
  opts$barWidth <- if(!missing(barWidth)) barWidth
  opts$barMaxWidth <- if(!missing(barMaxWidth)) barMaxWidth

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
eline <- function(p, serie, name = NULL, stack = NULL, clickable = TRUE, xAxisIndex = 0, yAxisIndex = 0, symbol = NULL,
                  symbolSize = "2 | 4", symbolRate = NULL, showAllSymbol = FALSE, smooth = TRUE, legendHoverLink = TRUE,
                  dataFilter = "nearest", z = 2, zlevel = 0, tooltip, markPoint, markLine, ...){

  name <- ifelse(is.null(name), deparse(substitute(serie)), name)
  serie <- vector_data(serie)

  # build $serie
  opts <- list(...)
  opts$name <- name
  opts$type <- "line"
  opts$data <- serie
  opts$stack <- if(!is.null(stack)) stack
  opts$clickable <- clickable
  opts$xAxisIndex <- xAxisIndex
  opts$yAxisIndex <- yAxisIndex
  opts$symbol <- symbol
  opts$symbolSize <- symbolSize
  opts$symbolRate <- symbolRate
  opts$showAllSymbol <- showAllSymbol
  opts$smooth <- smooth
  opts$dataFilter <- dataFilter
  opts$legendHoverLink <- legendHoverLink
  opts$z <- z
  opts$zlevel <- zlevel
  opts$tooltip <- if(!missing(tooltip)) tooltip
  opts$markPoint <- if(!missing(markPoint)) markPoint
  opts$markLine <- if(!missing(markLine)) markLine

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
earea <- function(p, serie, name = NULL, stack = NULL, ...){

  name <- ifelse(is.null(name), deparse(substitute(serie)), name)
  serie <- vector_data(serie)

  # build $serie
  opts <- list(...)
  opts$name <- name
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
#'   escatter(mpg)
#'
#' mtcars %>%
#'   echart(disp) %>%
#'   escatter(mpg, mpg * 2) %>%
#'   escatter(qsec, qsec * 2)
#'
#' @export
escatter <- function(p, serie, size, name = NULL, clickable = TRUE,  ...){

  # datarange
  data <- get("data", envir = data_env)
  if(!missing(size)){
    x <- eval(substitute(size), data)
  } else {
    x <- eval(substitute(serie), data)
  }

  p <- p %>%
    edatarange(min = get_min_(x), max = get_max_(x), calculable = is_calculable_(x), show = TRUE)

  name <- ifelse(is.null(name), deparse(substitute(serie)), name)
  serie <- scatter_data(serie, size)

  # build $serie
  opts <- list(...)
  opts$name <- name
  opts$type <- "scatter"
  opts$data <- serie
  opts$clickable
  opts$symbolSize <- if(!missing(size)){htmlwidgets::JS("function (value){ return Math.round(value[2] / 5);}")}

  p$x$options$xAxis[[1]]$data <- NULL
  p$x$options$xAxis[[1]]$type <- "value"
  p$x$options$yAxis <- list(list(type = "value"))
  p$x$options$series <- append(p$x$options$series, list(opts))

  p <- p %>%
    eyAxis(type = "value")

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
epie <- function(p, serie, name = NULL, ...){

  name <- ifelse(is.null(name), deparse(substitute(serie)), name)
  serie <- val_name_data(serie)

  # build $serie
  opts <- list(...)
  opts$name <- name
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
eradar <- function(p, serie, name = NULL, ...){

  name <- ifelse(is.null(name), deparse(substitute(serie)), name)
  serie <- vector_data(serie)
  serie <- list(value = serie, name = serie_name)

  # build $serie
  opts <- list(...)
  opts$name <- name
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
#' matrix %>%
#'   echart(LETTERS[1:10]) %>%
#'   echord(ribbonType = FALSE)
#'
#' @export
echord <- function(p, name = NULL, sort = "none", sortSub = "none", clickable = TRUE, z = 2, zlevel = 0,
                   symbol = NULL, symbolSize = NULL, clockWise = FALSE, minRadius = 10, maxRadius = 20,
                   ribbonType = TRUE, showScale = FALSE, showScaleText = FALSE, padding = 2, categories, tooltip,
                   markPoint, markLine, ...){

  name <- ifelse(is.null(name), deparse(substitute(serie)), name)

  opts <- list(...)
  opts$name <- name
  opts$type <- "chord"
  opts$sort <- sort
  opts$sortSub <- sortSub
  opts$clickable <- clickable
  opts$z <- z
  opts$zlevel <- zlevel
  opts$symbol <- if(!is.null(symbol)) symbol
  opts$symbolSize <- if(!is.null(symbolSize)) symbolSize
  opts$clockWise <- clockWise
  opts$minRadius <- minRadius
  opts$maxRadius <- maxRadius
  opts$ribbonType <- ribbonType
  opts$showScale <- showScale
  opts$showScaleText <- showScaleText
  opts$padding <- padding
  opts$categories <- if(!missing(categories)) categories
  opts$tooltip <- if(!missing(tooltip)) tooltip
  opts$markPoint <- if(!missing(markPoint)) markPoint
  opts$markLine <- if(!missing(markLine)) markLine
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
#'   values = round(runif(6, 10, 25)))
#'
#' choropleth %>%
#'   echart(countries) %>%
#'   emap() %>%
#'   emap_choropleth(values)
#'
#' @export
emap_choropleth <- function(p, serie, dataRange){

  dataRange <- if(missing(dataRange)) default_dataRange(serie)
  p$x$options$dataRange <- dataRange

  previous <- length(p$x$options$series)
  p$x$options$series[[previous]]$data <- val_name_data(serie)
  p$x$options$series[[previous]]$hoverable <- TRUE

  p
}

#' Add map coordinates
#'
#' Add coordinates to map
#'
#' @export
emap_coords <- function(p, lon, lat){

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  previous <- length(p$x$options$series)

  p$x$options$series[[previous]]$geoCoord <- build_coord(lon, lat)

  p

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
#'   emap() %>%
#'   emap_coords(lon, lat) %>%
#'   emap_lines(edges, source, target)
#'
#' edges2 <- data.frame(source = "London", target = "Sydney")
#'
#' coords %>%
#'   echart(city) %>%
#'   emap() %>%
#'   emap_coords(lon, lat) %>%
#'   emap_lines(edges, source, target) %>%
#'   emap() %>%
#'   emap_coords(lon, lat) %>%
#'   emap_lines(edges2, source, target, effect = emap_line_effect()) %>%
#'   etheme("macarons")
#'
#' coords2 <- data.frame(city = "Sydney", lon = 151.18518, lat = -33.92001, value = 20)
#'
#' coords %>%
#'   echart(city) %>%
#'   emap() %>%
#'   emap_coords(lon, lat) %>%
#'   emap_lines(edges, source, target) %>%
#'   edata(coords2, city) %>%
#'   emap() %>%
#'   emap_coords(lon, lat) %>%
#'   emap_lines(edges2, source, target, effect = emap_line_effect(scaleSize = 2)) %>%
#'   emap_coords(lon, lat) %>%
#'   emap_points(value, symbol = "emptyCircle", effect = list(show = TRUE, shadowBlur = 10)) %>%
#'   etheme("infographic")
#'
#' @seealso \code{\link{emap_coords}}
#'
#' @export
emap_lines <- function(p, edges, source, target, name = NULL, clickable = TRUE, symbol = list("circle", "arrow"),
                       symbolSize = list(2, 4), symbolRate = NULL, large = FALSE, smooth = TRUE, z = 2, zlevel = 0,
                       smoothness = 0.2, precision = 2, bundling = list(enable = FALSE, maxTurningAngle = 45),
                       effect, itemStyle, ...){

  name <- ifelse(is.null(name), "edges", name)

  opts <- list(...)
  opts$name <- name
  opts$clickable <- clickable
  opts$symbol <- symbol
  opts$symbolSize <- symbolSize
  opts$symbolRate <- symbolRate
  opts$large <- large
  opts$smooth <- smooth
  opts$z <- z
  opts$zlevel <- zlevel
  opts$smoothness <- smoothness
  opts$precision <- precision
  opts$bundling <- bundling
  opts$effect <- if(!missing(effect)) effect
  itemStyle <- if(!missing(itemStyle)) itemStyle

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
#'   emap() %>%
#'   emap_coords(lon, lat) %>%
#'   emap_points(values)
#'
#' coords2 <- data.frame(city = "Rio", lon = -43.172896, lat = -22.906847, value = 15)
#'
#' coords %>%
#'   echart(city) %>%
#'   emap() %>%
#'   emap_coords(lon, lat) %>%
#'   emap_points(values, symbolSize = 5) %>%
#'   edata(coords2, city) %>%
#'   emap() %>%
#'   emap_coords(lon, lat) %>%
#'   emap_points(value, symbol = "emptyCircle", effect = list(show = TRUE, shadowBlur = 10)) %>%
#'   etheme("infographic")
#'
#' @export
emap_points <- function(p, serie, clickable = TRUE, symbol = "pin", symbolSize = htmlwidgets::JS(" function (v){ return 10 + v/10 }"),
                        symbolRotate = NULL, large = FALSE, itemStyle, effect, ...){

  data <- get("data", envir = data_env)
  serie <- eval(substitute(serie), data)
  itemStyle <- if(missing(itemStyle)) list(normal = list(label = list(show = FALSE)))

  opts <- list()
  opts$symbol = symbol
  opts$symbolSize = symbolSize
  opts$symbolRotate <- symbolRotate
  opts$large <- large
  opts$effect = if(!missing(effect)) effect
  opts$itemStyle <- if(!missing(itemStyle)) itemStyle
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
#'   echart() %>%
#'   emap(mapType = "china") %>%
#'   emap_heat(lon, lat, z)
#'
#' data %>%
#'   echart() %>%
#'   emap(mapType = "china") %>%
#'   emap_heat(lon, lat, z, blurSize = 50, minAlpha = 0.3, opacity = 0.8,)
#'
#' @export
emap_heat <- function(p, lon, lat, z, blurSize = 30, minAlpha = 0.05, valueScale = 1, opacity = 1,
                      gradientColors, ...){

  gradientColors <- if(missing(gradientColors)) default_gradient()

  opts <- list(...)
  opts$blurSize <- blurSize
  opts$minAlpha <- minAlpha
  opts$valueScale <- valueScale
  opts$opacity <- opacity
  opts$data <- heat_map_data(lon, lat, z)
  opts$gradientColors <- gradientColors

  # append
  previous <- length(p$x$options$series)
  p$x$options$series[[previous]]$heatmap = opts

  p
}

#' Add blank map
#'
#' Setup map plot.
#'
#' @examples
#' coords <- data.frame(city = c("London", "New York", "Beijing", "Sydney"),
#'   lon = c(-0.1167218, -73.98002, 116.3883, 151.18518),
#'   lat = c(51.49999, 40.74998, 39.92889, -33.92001),
#'   values = runif(4, 10, 20))
#'
#' coords %>%
#'   echart(city) %>% # initialise chart
#'   emap() %>% # setup default map
#'   emap_coords(lon, lat) %>% # add coordinates
#'   emap_points(values) # plot values on coordinates
#'
#' edges <- data.frame(source = c("Beijing", "Beijing", "New York"),
#'   target = c("Sydney", "London", "London"))
#'
#' coords %>%
#'   echart(city) %>%
#'   emap() %>%
#'   emap_coords(lon, lat) %>%
#'   emap_lines(edges, source, target)
#'
#' data <- data.frame(lon = runif(200, 90, 120),
#'   lat = runif(200, 30, 39),
#'   z = runif(200, 50, 75))
#'
#' data %>%
#'   echart() %>%
#'   emap(mapType = "china") %>%
#'   emap_heat(lon, lat, z)
#'
#' @seealso \code{\link{emap_coords}}, \code{\link{emap_heat}}, \code{\link{emap_lines}}, \code{emap_choropleth},
#' \code{\link{emap_points}}
#'
#' @export
emap <- function(p, name = NULL, mapType = "world", clickable = TRUE, z = 2, zlevel = 0,
                 selectedMode = NULL, hoverable = FALSE, dataRangeHoverLink = TRUE,
                 mapLocation = list(x = "center", y = "center"), mapValueCalculation = "sum",
                 mapValuePrecision = 0, showLegendSymbol = TRUE, roam = FALSE, scaleLimit = NULL,
                 nameMap = NULL, textFixed = NULL, ...){

  xname <- get("x.name", envir = data_env)
  name <- ifelse(is.null(name), xname, name)

  opts <- list(...)
  opts$name <- name
  opts$type <- "map"
  opts$mapType <- mapType
  opts$clickable <- clickable
  opts$z <- z
  opts$zlevel <- zlevel
  opts$selectedMode <- selectedMode
  opts$hoverable <- hoverable
  opts$dataRangeHoverLink <- dataRangeHoverLink
  opts$mapLocation <- mapLocation
  opts$mapValueCalculation <- mapValueCalculation
  opts$mapValuePrecision <- mapValuePrecision
  opts$showLegendSymbol <- showLegendSymbol
  opts$roam <- roam
  opts$scaleLimit <- scaleLimit
  opts$nameMap <- nameMap
  opts$textFixed <- textFixed
  opts$data <- list()

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
egauge <- function(p, value, indicator = "", name = NULL, clickable = TRUE, legendHoverLink = TRUE, center = list("50%", "50%"),
                   radius = list("0%", "75%"), startAngle = 225, endAngle = -45, min = 0, max = 100,
                   splitNumber = 10, z = 2, zlevel = 0, tooltip, markPoint, markLine, axisLine, axisLabel,
                   splitLine, pointer, title, detail, ...){

  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item")
  markPoint <- if(missing(markPoint)) default_mark_point()
  markLine <- if(missing(markLine)) default_mark_line()
  name <- ifelse(is.null(name), indicator, name)

  opts <- list(...)
  opts$name <- name
  opts$type <- "gauge"
  opts$clickable <- clickable
  opts$legendHoverLink <- legendHoverLink
  opts$center <- center
  opts$radius <- radius
  opts$startAngle <- startAngle
  opts$endAngle <- endAngle
  opts$min <- min
  opts$max <- max
  opts$z <- z
  opts$zlevel <- zlevel
  opts$splitNumber <- splitNumber
  opts$tooltip <- tooltip
  opts$markPoint <- markPoint
  opts$markLine <- markLine
  opts$axisLine <- if(!missing(axisLine)) axisLine
  opts$axisLabel <- if(!missing(axisLabel)) axisLabel
  opts$splitLine <- if(!missing(splitLine)) splitLine
  opts$pointer <- if(!missing(pointer)) pointer
  opts$title <- if(!missing(title)) title
  opts$detail <- if(!missing(detail)) detail

  opts$data = list(list(value = value, name = indicator))

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
efunnel <- function(p, serie, name = NULL, clickable = TRUE, legendHoverLink = TRUE, sort = "descending",
                    min = 0, max = 100, x = 80, y = 60, x2 = 80, y2 = 60, width = NULL, height = NULL,
                    funnelAlign = "center", minSize = "0%", maxSize = "100%", gap = 0, markPoint,
                    markLine, tooltip, itemStyle,...){

  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item")
  markPoint <- if(missing(markPoint)) default_mark_point()
  markLine <- if(missing(markLine)) default_mark_line()
  name <- ifelse(is.null(name), deparse(substitute(serie)), name)

  data <- get("data", envir = data_env)
  serie <- eval(substitute(serie), data)

  opts <- list(...)
  opts$name <- name
  opts$type <- "funnel"
  opts$clickable <- clickable
  opts$legendHoverLink <- legendHoverLink
  opts$sort <- sort
  opts$min <- min
  opts$max <- max
  opts$x <- x
  opts$y <- y
  opts$x2 <- x2
  opts$y2 <- y2
  opts$width <- width
  opts$height <- height
  opts$funnelAlign <- funnelAlign
  opts$minSize <- minSize
  opts$maxSize <- maxSize
  opts$gap <- gap
  opts$markPoint <- markPoint
  opts$markLine <- markLine
  opts$tooltip <- tooltip
  opts$itemStyle <- if(!missing(itemStyle)) itemStyle
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
evenn <- function(p, serie, overlap, name = NULL, clickable = TRUE, z = 2, zlevel = 0, tooltip, itemStyle, markPoint, markLine, ...){

  name <- ifelse(is.null(name), deparse(substitute(serie)), name)
  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item")
  markPoint <- if(missing(markPoint)) default_mark_point()
  markLine <- if(missing(markLine)) default_mark_line()
  itemStyle <- list(normal = list(label = list(show = TRUE)))

  opts <- list(...)
  opts$name <- name
  opts$type <- "venn"
  opts$itemStyle <- itemStyle
  opts$clickable <- clickable
  opts$z <- z
  opts$zlevel <- zlevel
  opts$tooltip <- tooltip
  opts$markPoint <- markPoint
  opts$markLine <- markLine
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
ecloud <- function(p, freq, color, name = NULL, clickable = TRUE, center = list("50%", "50%"), size = list("40%", "40%"),
                   textRotation = list(0, 90), autoSize = list(enable = TRUE, minSize = 12), z = 2, zlevel = 0,
                   markPoint, markLine, tooltip, ...){

  name <- ifelse(is.null(name), deparse(substitute(serie)), name)
  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item")
  markPoint <- if(missing(markPoint)) default_mark_point()
  markLine <- if(missing(markLine)) default_mark_line()

  opts <- list(...)
  opts$name <- name
  opts$type <- "wordCloud"
  opts$clickable <- clickable
  opts$center <- center
  opts$size <- size
  opts$textRotation <- textRotation
  opts$autoSize <- autoSize
  opts$z <- z
  opts$zlevel <- zlevel
  opts$markPoint <- markPoint
  opts$markLine <- markLine
  opts$tooltip <- tooltip
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
eheatmap <- function(p, y, values, name = NULL, clickable = TRUE, blurSize = 30, minAlpha = 0.5, valueScale = 1,
                     opacity = 1, z = 2, zlevel = 0, gradientColors, markPoint, markLine, tooltip, ...){

  name <- ifelse(is.null(name), deparse(substitute(serie)), name)
  gradientColors <- if(missing(gradientColors)) default_gradient()
  markPoint <- if(missing(markPoint)) default_mark_point()
  markLine <- if(missing(markLine)) default_mark_line()
  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item")

  opts <- list(...)
  opts$name <- name
  opts$type <- "heatmap"
  opts$tooltip <- tooltip
  opts$clickable <- clickable
  opts$blurSize <- blurSize
  opts$minAlpha <- minAlpha
  opts$valueScale <- valueScale
  opts$opacity <- opacity
  opts$z <- z
  opts$zlevel <- zlevel
  opts$gradientColors <- gradientColors
  opts$markPoint <- markPoint
  opts$markLine <- markLine
  opts$data = heat_data(y, values)

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' Add data
#'
#' @export
edata <- function(p, data, x, ...){

  # x
  if(!missing(x)){
    xvar <- tryCatch(eval(substitute(x), data), error = function(e) e)
    if(is(xvar, "error")){
      xvar <- x
    }
  } else {
    xvar <- list()
  }

  if(!missing(data)){
    assign("data", data, envir = data_env)
  }

  # assign for future use
  assign("x", xvar, envir = data_env)
  if(length(xvar)) assign("x.name", deparse(substitute(x)), envir = data_env)

  # forward options using x
  x = list(
    theme = "default",
    options = list(
      xAxis = list(
        list(
          data = xvar
        )
      ),
      yAxis = list(),
      series = list()
    )
  )

  p
}
