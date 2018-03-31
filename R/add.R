#' @rdname ebar
#' @export
ebar <- function(p, serie, name = NULL, stack = NULL, clickable = TRUE, xAxisIndex = 0, yAxisIndex = 0, barGap = "100%",
                 barCategoryGap = "20%", legendHoverLink = TRUE, z = 2, zlevel = 0, ...){

  serie <- deparse(substitute(serie))

  p %>%
    ebar_(serie, name, stack, clickable, xAxisIndex, yAxisIndex, barGap, barCategoryGap, legendHoverLink, z, zlevel, ...)

}

#' @rdname eline
#' @export
eline <- function(p, serie, name = NULL, stack = NULL, clickable = TRUE, xAxisIndex = 0, yAxisIndex = 0, symbol = NULL,
                  symbolSize = "2 | 4", symbolRotate = NULL, showAllSymbol = FALSE, smooth = TRUE, legendHoverLink = TRUE,
                  dataFilter = "nearest", z = 2, zlevel = 0, tooltip, ...){

  serie <- deparse(substitute(serie))
  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "axis")

  p %>%
    eline_(serie, name, stack, clickable, xAxisIndex, yAxisIndex, symbol, symbolSize, symbolRotate, showAllSymbol, smooth,
           legendHoverLink, dataFilter, z, zlevel, tooltip, ...)

}

#' @rdname earea
#' @export
earea <- function(p, serie, name = NULL, stack = NULL, smooth = TRUE, ...){

  serie <- deparse(substitute(serie))

  p %>%
    earea_(serie, name, stack, smooth, ...)
}

#' @rdname escatter
#' @export
escatter <- function(p, serie, size = NULL, name = NULL, clickable = TRUE, symbol = NULL, symbolSize = 4, symbolRotate = NULL,
                     large = FALSE, largeThreshold = 2000, legendHoverLink = TRUE, z = 2, zlevel = 0, ...){

  serie <- deparse(substitute(serie))
  size <- if(!missing(size)) deparse(substitute(size)) else NULL

  p %>%
    escatter_(serie, size, name, clickable, symbol, symbolSize, symbolRotate, large, largeThreshold, legendHoverLink, z, zlevel, ...)
}

#' @rdname epie
#' @export
epie <- function(p, serie, name = NULL, clickable = TRUE, legendHoverLink = TRUE, center = list("50%", "50%"),
                 radius = list(0, "75%"), startAngle = 90, minAngle = 0, clockWise = TRUE, roseType = NULL, selectedOffset = 10,
                 selectedMode = TRUE, z = 2, zlevel = 0, ...){

  serie <- deparse(substitute(serie))

  p %>%
    epie_(serie, name, clickable, legendHoverLink, center, radius, startAngle, minAngle, clockWise, roseType, selectedOffset,
          selectedMode, z, zlevel, ...)
}

#' @rdname eradar
#' @export
eradar <- function(p, serie, name = NULL, clickable = TRUE, symbol = NULL, symbolSize = 4, symbolRotate = NULL,
                   legendHoverLink = TRUE, polarIndex = 0, z = 2, zlevel = 0, ...){

  serie <- deparse(substitute(serie))

  p %>%
    eradar_(serie, name, clickable, symbol, symbolSize, symbolRotate, legendHoverLink, polarIndex, z, zlevel, ...)
}

#' @rdname echord
#' @export
echord <- function(p, name = NULL, sort = "none", sortSub = "none", clickable = TRUE, z = 2, zlevel = 0,
                   symbol = NULL, symbolSize = NULL, clockWise = FALSE, minRadius = 10, maxRadius = 20,
                   ribbonType = TRUE, showScale = FALSE, showScaleText = FALSE, padding = 2, ...){

  p %>%
    echord_(name, sort, sortSub, clickable, z, zlevel, symbol, symbolSize, clockWise, minRadius, maxRadius,
            ribbonType, showScale, showScaleText, padding, ...)
}

#' @rdname emap_choropleth
#' @export
emap_choropleth <- function(p, serie){

  serie <- deparse(substitute(serie))

  p %>%
    emap_choropleth_(serie)
}

#' @rdname emap_coords
#' @export
emap_coords <- function(p, lon, lat){

  lon <- deparse(substitute(lon))
  lat <- deparse(substitute(lat))

  p %>%
    emap_coords_(lon, lat)
}

#'
#' @rdname emap_lines
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

#' @rdname emap_points
#' @export
emap_points <- function(p, serie, clickable = TRUE, symbol = "pin", symbolSize = 10,
                        symbolRotate = NULL, large = FALSE, itemStyle = NULL, ...){

  serie <- deparse(substitute(serie))
  itemStyle <- if(is.null(itemStyle)) list(normal = list(label = list(show = FALSE))) else itemStyle

  p %>%
    emap_points_(serie, clickable, symbol, symbolSize, symbolRotate, large, itemStyle, ...)
}

#' @rdname emap_heat
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

#' @rdname emap
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

#' @rdname egauge
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

#' @rdname efunnel
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

#' @rdname evenn
#' @export
evenn <- function(p,  serie, name = NULL, clickable = TRUE, z = 2, zlevel = 0, tooltip = NULL, ...){

  serie <- deparse(substitute(serie))
  tooltip <- if(is.null(tooltip)) default_tooltip(trigger = "item") else tooltip

  p %>%
    evenn_( serie, name, clickable, z, zlevel, tooltip, ...)
}

#' @rdname ecloud
#' @export
ecloud <- function(p, freq, color, name = NULL, clickable = TRUE, center = list("50%", "50%"), size = list("100%", "100%"),
                   textRotation = list(0, 90), autoSize = list(enable = TRUE, minSize = 12), z = 2, zlevel = 0, tooltip, ...){

  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item")
  freq <- deparse(substitute(freq))
  color <- if(!missing(color)) deparse(substitute(color)) else NULL

  p %>%
    ecloud_(freq, color, name, clickable, center, size, textRotation, autoSize, z, zlevel, tooltip, ...)
}

#' @rdname eheatmap
#' @export
eheatmap <- function(p, y, values, name = NULL, clickable = TRUE, blurSize = 30, minAlpha = 0.5, valueScale = 1,
                     opacity = 1, z = 2, zlevel = 0, gradientColors, tooltip, ...){

  gradientColors <- if(missing(gradientColors)) default_gradient() else gradientColors
  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item")
  y <- deparse(substitute(y))
  values <- deparse(substitute(values))

  p %>%
    eheatmap_(y, values, name, clickable, blurSize, minAlpha, valueScale, opacity, z, zlevel, gradientColors, tooltip, ...)
}

#' @rdname edata
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

#' @rdname etreemap
#' @export
etreemap <- function(p, serie, name = NULL, itemStyle = NULL, clickable = FALSE, center = list("50%", "50%"),
                     size = list("80%", "80%"), z = 2, zlevel = 0, ...){

  serie <- deparse(substitute(serie))

  p <- p %>%
    etreemap_(serie, name, itemStyle, clickable, center, size, z, zlevel, ...)

  p

}

#' @rdname candlestick
#' @export
ecandle <- function(p, opening, closing, low, high, name = NULL, clickable = TRUE, z = 2, zlevel = 0, ...){

  opening <- deparse(substitute(opening))
  closing <- deparse(substitute(closing))
  low <- deparse(substitute(low))
  high <- deparse(substitute(high))

  p <- p %>%
    ecandle_(opening, closing, low, high, name = NULL, clickable = TRUE, z = 2, zlevel = 0, ...)

  p

}
