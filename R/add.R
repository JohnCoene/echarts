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
                 barCategoryGap = "20%", legendHoverLink = TRUE, z = 2, zlevel = 0, tooltip, ...){

  serie <- deparse(substitute(serie))
  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "axis")

  p %>%
    ebar_(serie, name, stack, clickable, xAxisIndex, yAxisIndex, barGap, barCategoryGap, legendHoverLink, z, zlevel,
          tooltip, ...)

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
                  dataFilter = "nearest", z = 2, zlevel = 0, tooltip, ...){

  serie <- deparse(substitute(serie))
  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "axis")

  p %>%
    eline_(serie, name, stack, clickable, xAxisIndex, yAxisIndex, symbol, symbolSize, symbolRate, showAllSymbol, smooth,
           legendHoverLink, dataFilter, z, zlevel, tooltip, ...)

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
earea <- function(p, serie, name = NULL, clickable = TRUE, legendHoverLink = TRUE, center = list("50%", "50%"),
                  radius = list(0, "75%"), startAngle = 90, minAngle = 0, clockWise = TRUE, roseType = NULL, selectedOffset = 10,
                  selectedMode = FALSE, z = 2, zlevel = 0, ...){

  serie <- deparse(substitute(serie))

  p %>%
    earea_(serie, name, clickable, legendHoverLink, center, radius, startAngle, minAngle, clockWise, roseType, selectedOffset,
           selectedMode, z, zlevel, ...)
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
#'   escatter(mpg, qsec)
#'
#' @export
escatter <- function(p, serie, size = NULL, name = NULL, clickable = TRUE, symbol = NULL, symbolSize = 4, symbolRotate = NULL,
                     large = FALSE, largeThreshold = 2000, legendHoverLink = TRUE, z = 2, zlevel = 0, ...){

  serie <- deparse(substitute(serie))
  size <- if(!missing(size)) deparse(substitute(size)) else NULL

  p %>%
    escatter_(serie, size, name, clickable, symbol, symbolSize, symbolRotate, large, largeThreshold, legendHoverLink, z, zlevel, ...)
}

#' Add pie
#'
#' Add pie chart
#'
#' @examples
#' pie <- data.frame(name = c("banana", "apple", "pineapple", "onion"),
#'   value = c(40, 15, 12, 9))
#'
#' pie %>%
#'   echart(name) %>%
#'   epie(value)
#'
#' pie %>%
#'   echart(name) %>%
#'   epie(value, roseType = "area") %>%
#'   etheme("helianthus")
#'
#' pie %>%
#'   echart(name) %>%
#'   epie(value, roseType = "radius") %>%
#'   etheme("mint")
#'
#' @export
epie <- function(p, serie, name = NULL, clickable = TRUE, legendHoverLink = TRUE, center = list("50%", "50%"),
                 radius = list(0, "75%"), startAngle = 90, minAngle = 0, clockWise = TRUE, roseType = NULL, selectedOffset = 10,
                 selectedMode = TRUE, z = 2, zlevel = 0, ...){

  serie <- deparse(substitute(serie))

  p %>%
    epie_(serie, name, clickable, legendHoverLink, center, radius, startAngle, minAngle, clockWise, roseType, selectedOffset,
          selectedMode, z, zlevel, ...)
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
eradar <- function(p, serie, name = NULL, clickable = TRUE, symbol = NULL, symbolSize = 4, symbolRotate = NULL,
                   legendHoverLink = TRUE, polarIndex = 0, z = 2, zlevel = 0, ...){

  serie <- deparse(substitute(serie))

  p %>%
    eradar_(serie, name, clickable, symbol, symbolSize, symbolRotate, legendHoverLink, polarIndex, z, zlevel, ...)
}


#' Add chord
#'
#' @examples
#' set.seed(19880525)
#' matrix <- matrix(sample(0:1, 100, replace=TRUE, prob=c(0.9,0.6)), nc=10)
#'
#' matrix %>%
#'   echart(LETTERS[1:10]) %>%
#'   echord_()
#'
#' matrix %>%
#'   echart(LETTERS[1:10]) %>%
#'   echord_(ribbonType = FALSE)
#'
#' @export
echord <- function(p, name = NULL, sort = "none", sortSub = "none", clickable = TRUE, z = 2, zlevel = 0,
                   symbol = NULL, symbolSize = NULL, clockWise = FALSE, minRadius = 10, maxRadius = 20,
                   ribbonType = TRUE, showScale = FALSE, showScaleText = FALSE, padding = 2, ...){

  p %>%
    echord_(name, sort, sortSub, clickable, z, zlevel, symbol, symbolSize, clockWise, minRadius, maxRadius,
            ribbonType, showScale, showScaleText, padding, ...)
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
emap_choropleth <- function(p, serie){

  serie <- deparse(substitute(serie))

  p %>%
    emap_choropleth_(serie)
}

#' Add map coordinates
#'
#' Add coordinates to map
#'
#' @export
emap_coords <- function(p, lon, lat){

  lon <- deparse(substitute(lon))
  lat <- deparse(substitute(lat))

  p %>%
    emap_coords_(lon, lat)
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
#'   etheme("helianthus")
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
emap_lines <- function(p, edges, source, target, name = NULL, clickable = TRUE, symbol = "arrow",
                       symbolSize = 2, symbolRotate = NULL, large = FALSE, smooth = TRUE, z = 2, zlevel = 0,
                       smoothness = 0.2, precision = 2, bundling = list(enable = FALSE, maxTurningAngle = 45), ...){

  source <- deparse(substitute(source))
  target <- deparse(substitute(target))

  p %>%
    emap_lines_(edges, source, target, name, clickable, symbol, symbolSize, symbolRotate, large, smooth, z, zlevel,
                smoothness, precision, bundling, ...)
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
emap_points <- function(p, serie, clickable = TRUE, symbol = "pin", symbolSize = 10,
                        symbolRotate = NULL, large = FALSE, itemStyle = NULL, ...){

  serie <- deparse(substitute(serie))
  itemStyle <- if(is.null(itemStyle)) list(normal = list(label = list(show = FALSE))) else itemStyle

  p %>%
    emap_points_(serie, clickable, symbol, symbolSize, symbolRotate, large, itemStyle, ...)
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
#'   emap_heat(lon, lat, z, blurSize = 50, minAlpha = 0.3, opacity = 0.8) %>%
#'   etheme("dark")
#'
#' @export
emap_heat <- function(p, lon, lat, z, blurSize = 30, minAlpha = 0.05, valueScale = 1, opacity = 1,
                      gradientColors = NULL, ...){

  gradientColors <- if(is.null(gradientColors)) default_gradient() else gradientColors
  lon <- deparse(substitute(lon))
  lat <- deparse(substitute(lat))
  z <- deparse(substitute(z))

  p %>%
    emap_heat_(lon, lat, z, blurSize, minAlpha, valueScale, opacity, gradientColors, ...)
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

  p %>%
    emap_(name, mapType, clickable, z, zlevel, selectedMode, hoverable, dataRangeHoverLink, mapLocation, mapValueCalculation,
         mapValuePrecision, showLegendSymbol, roam, scaleLimit, nameMap, textFixed, ...)

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
egauge <- function(p, value, indicator = "", name = NULL, clickable = FALSE, legendHoverLink = TRUE, center = list("50%", "50%"),
                   radius = list("0%", "75%"), startAngle = 225, endAngle = -45, min = 0, max = 100,
                   splitNumber = 10, z = 2, zlevel = 0, tooltip, ...){

  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item")
  name <- ifelse(is.null(name), indicator, name)

  p %>%
    egauge_(value, indicator, name, clickable, legendHoverLink, center, radius, startAngle, endAngle, min, max,
            splitNumber , z, zlevel, tooltip, ...)
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
                    min = NULL, max = NULL, x = 80, y = 60, x2 = 80, y2 = 60, width = NULL, height = NULL,
                    funnelAlign = "center", minSize = "0%", maxSize = "100%", gap = 0, tooltip, ...){

  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item")
  serie <- deparse(substitute(serie))

  p %>%
    efunnel_(serie, name, clickable, legendHoverLink, sort, min, max, x, y, x2, y2, width, height,
             funnelAlign, minSize, maxSize, gap, tooltip, ...)
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
#'   evenn(values) %>%
#'   etheme("infographic")
#'
#' @export
evenn <- function(p, serie, overlap, name = NULL, clickable = TRUE, z = 2, zlevel = 0, tooltip, ...){

  serie <- deparse(substitute(serie))
  overlap <- deparse(substitute(overlap))

  p %>%
    evenn_(serie, overlap, name, clickable, z, zlevel, tooltip, ...)
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
                   textRotation = list(0, 90), autoSize = list(enable = TRUE, minSize = 12), z = 2, zlevel = 0, tooltip, ...){

  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item")
  freq <- deparse(substitute(freq))
  color <- if(!missing(color)) deparse(substitute(color)) else NULL

  p %>%
    ecloud_(freq, color, name, clickable, center, size, textRotation, autoSize, z, zlevel, tooltip, ...)
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
                     opacity = 1, z = 2, zlevel = 0, gradientColors, tooltip, ...){

  gradientColors <- if(missing(gradientColors)) default_gradient()
  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item")
  y <- deparse(substitute(y))
  values <- deparse(substitute(values))

  p %>%
    eheatmap_(y, values, name, clickable, blurSize, minAlpha, valueScale, opacity, z, zlevel, gradientColors, tooltip, ...)
}

#' Add data
#'
#' @export
edata <- function(p, data, x){

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
    data <- map_grps_(data)
    assign("data", data, envir = data_env)
  }

  # assign for future use
  assign("x", xvar, envir = data_env)
  if(length(xvar)) assign("x.name", deparse(substitute(x)), envir = data_env)

  p

}
