#' Add bars
#'
#' Add bar serie.
#'
#' @examples
#' mtcars %>%
#'   echart_("mpg") %>%
#'   ebar_("qsec")
#'
#' mtcars %>%
#'   echart_("disp") %>%
#'   ebar_("mpg", stack = "grp") %>% # stack
#'   ebar_("qsec", stack = "grp") %>% # stack
#'   ebar_("wt") %>% # not stacked
#'   etooltip() %>%
#'   elegend()
#'
#' @export
ebar_ <- function(p, serie, name = NULL, stack = NULL, clickable = TRUE, xAxisIndex = 0, yAxisIndex = 0, barGap = "100%",
                 barCategoryGap = "20%", legendHoverLink = TRUE, z = 2, zlevel = 0, tooltip, itemStyle,
                 barWidth, barMaxWidth, ...){

  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "axis")

  name <- if(is.null(name)) serie

  # build $serie
  opts <- list(...)
  opts$name <- name
  opts$type <- "bar"
  opts$data <- vector_data_(serie)
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
#'   echart_("mpg") %>%
#'   eline_("qsec")
#'
#' mtcars %>%
#'   echart_("disp") %>%
#'   eline_("mpg", stack = "grp") %>%
#'   eline_("qsec", stack = "grp") %>%
#'   eline_("wt", fill = TRUE) %>%
#'   etooltip() %>%
#'   elegend() %>%
#'   etoolbox_magic(type = list("line", "bar"))
#'
#' @export
eline_ <- function(p, serie, name = NULL, stack = NULL, clickable = TRUE, xAxisIndex = 0, yAxisIndex = 0, symbol = NULL,
                  symbolSize = "2 | 4", symbolRate = NULL, showAllSymbol = FALSE, smooth = TRUE, legendHoverLink = TRUE,
                  dataFilter = "nearest", z = 2, zlevel = 0, tooltip, markPoint, markLine, ...){

  name <- if(is.null(name)) serie
  serie <- vector_data_(serie)

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
#'   echart_("mpg") %>%
#'   earea_("qsec")
#'
#' mtcars %>%
#'   echart_("disp") %>%
#'   earea_("mpg", stack = "grp") %>%
#'   earea_("qsec", stack = "grp") %>%
#'   earea_("wt", stack = "grp") %>%
#'   etooltip() %>%
#'   elegend()
#'
#' @export
earea_ <- function(p, serie, name = NULL, stack = NULL, ...){

  name <- if(is.null(name)) serie
  serie <- vector_data_(serie)

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
#'   echart_("disp") %>%
#'   escatter_("mpg")
#'
#' mtcars %>%
#'   echart_("disp") %>%
#'   escatter_("mpg", "mpg") %>%
#'   escatter_("qsec", "qsec")
#'
#' @export
escatter_ <- function(p, serie, size, name = NULL, clickable = TRUE,  ...){

  name <- if(is.null(name)) serie
  serie <- scatter_data_(serie, size)

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

  p
}

#' Add pie
#'
#' Add pie chart
#'
#' @examples
#' pie <- data.frame(name = c("banana", "apple", "pineapple", "onion"),
#'   value = c(26, 15, 12, 9))
#'
#' pie %>%
#'   echart_("name") %>%
#'   epie_("value")
#'
#' pie %>%
#'   echart_("name") %>%
#'   epie_("value", roseType = "area") %>%
#'   etheme("helianthus")
#'
#' pie %>%
#'   echart_("name") %>%
#'   epie_("value", roseType = "radius") %>%
#'   etheme("mint")
#'
#' @export
epie_ <- function(p, serie, name = NULL, ...){

  name <- if(is.null(name)) serie
  serie <- val_name_data_(serie)

  p$x$options$legend$data <- append(p$x$options$legend$data, serie)

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
#' radar <- data.frame(axis = LETTERS[1:6], value = runif(6, 2, 10), value2 = runif(6, 3, 11))
#'
#' radar %>%
#'   echart_("axis") %>%
#'   eradar_("value") %>%
#'   eradar_("value2") %>%
#'   elegend()
#'
#' @export
eradar_ <- function(p, serie, name = NULL, ...){

  name <- if(is.null(name)) serie
  serie <- vector_data_(serie)
  serie <- list(value = serie, name = name)

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

#' Add choropleth
#'
#' @examples
#' choropleth <- data.frame(countries = c("France", "Brazil", "China", "Russia", "Canada", "India"),
#'   values = round(runif(6, 10, 25)))
#'
#' choropleth %>%
#'   echart_("countries") %>%
#'   emap() %>%
#'   emap_choropleth_("values")
#'
#' @export
emap_choropleth_ <- function(p, serie, dataRange){

  dataRange <- if(missing(dataRange)) default_dataRange_(serie)
  p$x$options$dataRange <- dataRange

  previous <- length(p$x$options$series)
  p$x$options$series[[previous]]$data <- val_name_data_(serie)
  p$x$options$series[[previous]]$hoverable <- TRUE

  p
}

#' Add map coordinates
#'
#' Add coordinates to map
#'
#' @export
emap_coords_ <- function(p, lon, lat){

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  previous <- length(p$x$options$series)

  p$x$options$series[[previous]]$geoCoord <- build_coord_(lon, lat)

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
#'   echart_("city") %>%
#'   emap() %>%
#'   emap_coords_("lon", "lat") %>%
#'   emap_lines_(edges, "source", "target")
#'
#' edges2 <- data.frame(source = "London", target = "Sydney")
#'
#' coords %>%
#'   echart_("city") %>%
#'   emap() %>%
#'   emap_coords_("lon", "lat") %>%
#'   emap_lines_(edges, "source", "target") %>%
#'   emap() %>%
#'   emap_coords_("lon", "lat") %>%
#'   emap_lines_(edges2, "source", "target", effect = emap_line_effect()) %>%
#'   etheme("helianthus")
#'
#' coords2 <- data.frame(city = "Sydney", lon = 151.18518, lat = -33.92001, value = 20)
#'
#' coords %>%
#'   echart_("city") %>%
#'   emap() %>%
#'   emap_coords_("lon", "lat") %>%
#'   emap_lines_(edges, "source", "target") %>%
#'   edata_(coords2, "city") %>%
#'   emap() %>%
#'   emap_coords_("lon", "lat") %>%
#'   emap_lines_(edges2, "source", "target", effect = emap_line_effect(scaleSize = 2)) %>%
#'   emap_coords_("lon", "lat") %>%
#'   emap_points_("value", symbol = "emptyCircle", effect = list(show = TRUE, shadowBlur = 10)) %>%
#'   etheme("dark")
#'
#' @seealso \code{\link{emap_coords}}
#'
#' @export
emap_lines_ <- function(p, edges, source, target, name = NULL, clickable = TRUE, symbol = list("circle", "arrow"),
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

  opts$data <- map_lines_(edges, source, target)

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
#'   echart_("city") %>%
#'   emap() %>%
#'   emap_coords_("lon", "lat") %>%
#'   emap_points_("values")
#'
#' coords2 <- data.frame(city = "Rio", lon = -43.172896, lat = -22.906847, value = 15)
#'
#' coords %>%
#'   echart_("city") %>%
#'   emap() %>%
#'   emap_coords_("lon", "lat") %>%
#'   emap_points_("values", symbolSize = 5) %>%
#'   edata_(coords2, "city") %>%
#'   emap() %>%
#'   emap_coords_("lon", "lat") %>%
#'   emap_points_("value", symbol = "emptyCircle", effect = list(show = TRUE, shadowBlur = 10)) %>%
#'   etheme("helianthus")
#'
#' @export
emap_points_ <- function(p, serie, clickable = TRUE, symbol = "pin", symbolSize = htmlwidgets::JS(" function (v){ return 10 + v/10 }"),
                        symbolRotate = NULL, large = FALSE, itemStyle, effect, ...){

  data <- get("data", envir = data_env)
  itemStyle <- if(missing(itemStyle)) list(normal = list(label = list(show = FALSE)))

  opts <- list()
  opts$symbol = symbol
  opts$symbolSize = symbolSize
  opts$symbolRotate <- symbolRotate
  opts$large <- large
  opts$effect = if(!missing(effect)) effect
  opts$itemStyle <- if(!missing(itemStyle)) itemStyle
  opts$data = val_name_data_(serie)

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
#'   echart_() %>%
#'   emap(mapType = "china") %>%
#'   emap_heat_("lon", "lat", "z")
#'
#' data %>%
#'   echart() %>%
#'   emap(mapType = "china") %>%
#'   emap_heat_("lon", "lat", "z", blurSize = 50, minAlpha = 0.3, opacity = 0.8) %>%
#'   etheme("dark")
#'
#' @export
emap_heat_ <- function(p, lon, lat, z, blurSize = 30, minAlpha = 0.05, valueScale = 1, opacity = 1,
                      gradientColors, ...){

  gradientColors <- if(missing(gradientColors)) default_gradient()

  opts <- list(...)
  opts$blurSize <- blurSize
  opts$minAlpha <- minAlpha
  opts$valueScale <- valueScale
  opts$opacity <- opacity
  opts$data <- heat_map_data_(lon, lat, z)
  opts$gradientColors <- gradientColors

  # append
  previous <- length(p$x$options$series)
  p$x$options$series[[previous]]$heatmap = opts

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
#'   echart_("stage") %>%
#'   efunnel_("value")
#'
#' @export
efunnel_ <- function(p, serie, name = NULL, clickable = TRUE, legendHoverLink = TRUE, sort = "descending",
                    min = 0, max = 100, x = 80, y = 60, x2 = 80, y2 = 60, width = NULL, height = NULL,
                    funnelAlign = "center", minSize = "0%", maxSize = "100%", gap = 0, markPoint,
                    markLine, tooltip, itemStyle,...){

  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item")
  markPoint <- if(missing(markPoint)) default_mark_point()
  markLine <- if(missing(markLine)) default_mark_line()
  name <- if(is.null(name)) serie

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
  opts$data = val_name_data_(serie)

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
#'   echart_("name") %>%
#'   evenn_("values") %>%
#'   etheme("mint")
#'
#' @export
evenn_ <- function(p, serie, overlap, name = NULL, clickable = TRUE, z = 2, zlevel = 0, tooltip, itemStyle, markPoint, markLine, ...){

  name <- if(is.null(name)) serie
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
  opts$data = val_name_data_(serie)

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
#'   echart_("terms") %>%
#'   ecloud_("freq", "color")
#'
#' @export
ecloud_ <- function(p, freq, color, name = NULL, clickable = TRUE, center = list("50%", "50%"), size = list("40%", "40%"),
                   textRotation = list(0, 90), autoSize = list(enable = TRUE, minSize = 12), z = 2, zlevel = 0,
                   markPoint, markLine, tooltip, ...){

  name <- ifelse(is.null(name), "cloud", name)
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
  opts$data = cloud_data_(freq, color)

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
#'   echart_("x") %>%
#'   eheatmap_("y", "z")
#'
#' @export
eheatmap_ <- function(p, y, values, name = NULL, clickable = TRUE, blurSize = 30, minAlpha = 0.5, valueScale = 1,
                     opacity = 1, z = 2, zlevel = 0, gradientColors, markPoint, markLine, tooltip, ...){

  name <- ifelse(is.null(name), values, name)
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
  opts$data = heat_data_(y, values)

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' Add data
#'
#' @export
edata_ <- function(p, data, x, ...){

  # x
  if(!missing(x)){
    xvar <- data[, x]
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
  if(length(xvar)) assign("x.name", x, envir = data_env)

  # forward options using x
  x = list(
    theme = "default",
    options = list(
      xAxis = list(
        list(
          type = get_axis_type(xvar),
          data = xvar
        )
      ),
      yAxis = list(),
      series = list()
    )
  )

  p
}
