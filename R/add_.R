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
#'   etooltip(trigger = "item") %>%
#'   elegend() %>%
#'   etoolbox_magic(type = list("stack", "tiled")) %>%
#'   etoolbox_restore()
#'
#' @export
ebar_ <- function(p, serie, name = NULL, stack = NULL, clickable = TRUE, xAxisIndex = 0, yAxisIndex = 0, barGap = "100%",
                 barCategoryGap = "20%", legendHoverLink = TRUE, z = 2, zlevel = 0, tooltip, ...){

  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "axis")

  data <- get_dat(serie)

  for(i in 1:length(data)){
    # build $serie
    opts <- list(...)
    opts$name <- if(is.null(name)) names(data)[i] else name
    opts$type <- "bar"
    opts$data <- vector_data_(data[[i]], serie)
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

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

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
                  dataFilter = "nearest", z = 2, zlevel = 0, tooltip, ...){

  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "axis")

  data <- get_dat(serie)

  for(i in 1:length(data)){
    # build $serie
    opts <- list(...)
    opts$name <- if(is.null(name)) names(data)[i] else name
    opts$type <- "line"
    opts$data <- vector_data_(data[[i]], serie)
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

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

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
earea_ <- function(p, serie, name = NULL, stack = NULL, smooth = TRUE, ...){

  data <- get_dat(serie)

  for(i in 1:length(data)){
    # build $serie
    opts <- list(...)
    opts$name <- if(is.null(name)) names(data)[i] else name
    opts$type <- "line"
    opts$data <- vector_data_(data[[i]], serie)
    opts$smooth <- smooth
    opts$stack <- if(!is.null(stack)) stack
    opts$itemStyle <-  list(normal= list(areaStyle = list(type = 'default')))

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

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
#'   escatter_("mpg", "qsec")
#'
#' @export
escatter_ <- function(p, serie, size = NULL, name = NULL, clickable = TRUE, ...){

  data <- get_dat(serie)

  for(i in 1:length(data)){
    # build $serie
    opts <- list(...)
    opts$name <- if(is.null(name)) names(data)[i] else name
    opts$type <- "scatter"
    opts$data <- scatter_data_(data[[i]], serie, size)
    opts$clickable
    opts$symbolSize <- if(!is.null(size)) htmlwidgets::JS("function (value){ return Math.round(value[2] / 5);}")

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

  p$x$options$xAxis[[1]]$data <- NULL
  p$x$options$xAxis[[1]]$type <- "value"
  p$x$options$yAxis <- list(list(type = "value"))

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

  data <- get_dat(serie)

  for(i in 1:length(data)){

    # build $serie
    opts <- list(...)
    opts$name <- if(is.null(name)) names(data)[i] else name
    opts$type <- "pie"
    opts$data <- val_name_data_(data[[i]], serie)

    p$x$options$xAxis <- NULL
    p$x$options$yAxis <- NULL

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

  p$x$options$legend$data <- append(p$x$options$legend$data, get_pie_legend())

  p
}

#' add radar
#'
#' @examples
#' radar <- data.frame(axis = rep(LETTERS[1:6], 4), grp = rep(LETTERS[4:9], 4),
#'   value = runif(24, 2, 10))
#'
#' radar %>%
#'   group_by_("grp") %>%
#'   echart_("axis") %>%
#'   eradar_("value") %>%
#'   elegend()
#'
#' @export
eradar_ <- function(p, serie, name = NULL, ...){

  data <- get_dat(serie)

  for(i in 1:length(data)){

    n <- if(is.null(name)) names(data)[i] else name

    # build $serie
    opts <- list(...)
    opts$name <- n
    opts$type <- "radar"
    opts$data <- list(list(value = vector_data_(data[[i]], serie), name = n))

    # set polar $indicator
    p$x$options$polar <- list(
      list(
        indicator = polar_indicator()
      )
    )

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

  # remove axis
  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p
}


#' Add chord
#'
#' @examples
#' set.seed(19880525)
#' matrix <- matrix(sample(0:1, 100, replace = TRUE, prob = c(0.9,0.6)), nc = 10)
#'
#' matrix %>%
#'   echart_(LETTERS[1:10]) %>%
#'   echord_()
#'
#' matrix %>%
#'   echart_(LETTERS[1:10]) %>%
#'   echord_(ribbonType = FALSE)
#'
#' @export
echord_ <- function(p, name = NULL, sort = "none", sortSub = "none", clickable = TRUE, z = 2, zlevel = 0,
                   symbol = NULL, symbolSize = NULL, clockWise = FALSE, minRadius = 10, maxRadius = 20,
                   ribbonType = TRUE, showScale = FALSE, showScaleText = FALSE, padding = 2, ...){

  opts <- list(...)
  opts$name <- name
  opts$type <- "chord"
  opts$sort <- sort
  opts$sortSub <- sortSub
  opts$clickable <- clickable
  opts$z <- z
  opts$zlevel <- zlevel
  opts$clockWise <- clockWise
  opts$minRadius <- minRadius
  opts$maxRadius <- maxRadius
  opts$ribbonType <- ribbonType
  opts$showScale <- showScale
  opts$showScaleText <- showScaleText
  opts$padding <- padding
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
#'   echart_("countries") %>%
#'   emap() %>%
#'   emap_choropleth_("values")
#'
#' @export
emap_choropleth_ <- function(p, serie, dataRange = NULL){

  data <- get_dat(serie)

  p$x$options$dataRange <- if(is.null(dataRange)) default_dataRange_(data[[1]], serie) else dataRange

  previous <- length(p$x$options$series)
  p$x$options$series[[previous]]$data <- val_name_data_(data[[1]], serie)
  p$x$options$series[[previous]]$hoverable <- TRUE

  p
}

#' Add map coordinates
#'
#' Add coordinates to map
#'
#' @export
emap_coords_ <- function(p, lon, lat){

  data <- get("data", envir = data_env)
  data <- clean_data_map(data)

  for(i in 1:length(data)){
    index <- get_map_index_(p, names(data)[i])
    p$x$options$series[[index]]$geoCoord <- build_coord_(data[[i]], lon, lat)
  }

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
                       smoothness = 0.2, precision = 2, bundling = list(enable = FALSE, maxTurningAngle = 45), ...){

  edges <- map_grps_(edges)

  for(i in 1:length(edges)){
    opts <- list(...)
    opts$name <- if(is.null(name)) names(edges)[i] else name
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

    opts$data <- map_lines_(edges[[i]], source, target)

    index <- get_map_index_(p, names(edges)[i])
    p$x$options$series[[index]]$markLine = opts
  }

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
emap_points_ <- function(p, serie, clickable = TRUE, symbol = "pin", symbolSize = 10, symbolRotate = NULL,
                         large = FALSE, itemStyle = NULL, ...){

  itemStyle <- if(is.null(itemStyle)) list(normal = list(label = list(show = FALSE))) else itemStyle

  data <- get("data", envir = data_env)
  data <- clean_data_map(data)

  for(i in 1:length(data)){
    opts <- list(...)
    opts$symbol = symbol
    opts$symbolSize = symbolSize
    opts$symbolRotate <- symbolRotate
    opts$large <- large
    opts$itemStyle <- itemStyle
    opts$data = val_name_data_map_(data[[i]], serie)

    index <- get_map_index_(p, names(data)[i])
    p$x$options$series[[index]]$markPoint = opts
  }

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
                      gradientColors = NULL, ...){

  opts <- list(...)
  opts$blurSize <- blurSize
  opts$minAlpha <- minAlpha
  opts$valueScale <- valueScale
  opts$opacity <- opacity
  opts$data <- heat_map_data_(lon, lat, z)
  opts$gradientColors <- if(is.null(gradientColors)) default_gradient() else gradientColors

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
#'   echart_("city") %>% # initialise chart
#'   emap_() %>% # setup default map
#'   emap_coords_("lon", "lat") %>% # add coordinates
#'   emap_points_("values") # plot values on coordinates
#'
#' edges <- data.frame(source = c("Beijing", "Beijing", "New York"),
#'   target = c("Sydney", "London", "London"))
#'
#' coords %>%
#'   echart_("city") %>%
#'   emap_() %>%
#'   emap_coords_("lon", "lat") %>%
#'   emap_lines_(edges, "source", "target")
#'
#' data <- data.frame(lon = runif(200, 90, 120),
#'   lat = runif(200, 30, 39),
#'   z = runif(200, 50, 75))
#'
#' data %>%
#'   echart_() %>%
#'   emap_(mapType = "china") %>%
#'   emap_heat_("lon", "lat", "z")
#'
#' @seealso \code{\link{emap_coords}}, \code{\link{emap_heat}}, \code{\link{emap_lines}}, \code{emap_choropleth},
#' \code{\link{emap_points}}
#'
#' @export
emap_ <- function(p, name = NULL, mapType = "world", clickable = TRUE, z = 2, zlevel = 0,
                 selectedMode = NULL, hoverable = FALSE, dataRangeHoverLink = TRUE,
                 mapLocation = list(x = "center", y = "center"), mapValueCalculation = "sum",
                 mapValuePrecision = 0, showLegendSymbol = TRUE, roam = FALSE, scaleLimit = NULL,
                 nameMap = NULL, textFixed = NULL, ...){

  # clean data for EC maps - on setup only
  data <- get("data", envir = data_env)
  data <- clean_data_map(data)

  for(i in 1:length(data)){
    opts <- list(...)
    opts$name <- if(is.null(name)) names(data)[i] else name
    opts$type <- "map"
    opts$mapType <- mapType # set to none if more than one map
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

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

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
egauge_ <- function(p, value, indicator = "", name = NULL, clickable = TRUE, legendHoverLink = TRUE, center = list("50%", "50%"),
                   radius = list("0%", "75%"), startAngle = 225, endAngle = -45, min = 0, max = 100,
                   splitNumber = 10, z = 2, zlevel = 0, tooltip, ...){

  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item")
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
#'   echart_("stage") %>%
#'   efunnel_("value")
#'
#' @export
efunnel_ <- function(p, serie, name = NULL, clickable = TRUE, legendHoverLink = TRUE, sort = "descending",
                    min = 0, max = 100, x = 80, y = 60, x2 = 80, y2 = 60, width = NULL, height = NULL,
                    funnelAlign = "center", minSize = "0%", maxSize = "100%", gap = 0, tooltip, ...){

  data <- get_dat(serie)

  for(i in 1:length(data)){
    opts <- list(...)
    opts$name <- if(is.null(name)) names(data)[i] else name
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
    opts$tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item") else tooltip
    opts$data = val_name_data_(data[[i]], serie)

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

  p$x$options$legend$data <- append(p$x$options$legend$data, get_pie_legend()) # legend

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

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
evenn_ <- function(p, serie, name = NULL, clickable = TRUE, z = 2, zlevel = 0, tooltip, ...){

  data <- get_dat(serie)

  name <- if(is.null(name)) names(data)[1] else name
  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item") else tooltip
  itemStyle <- list(normal = list(label = list(show = TRUE)))

  opts <- list(...)
  opts$name <- name
  opts$type <- "venn"
  opts$itemStyle <- itemStyle
  opts$clickable <- clickable
  opts$z <- z
  opts$zlevel <- zlevel
  opts$tooltip <- tooltip
  opts$data = val_name_data_(data[[1]], serie)

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$series <- append(p$x$options$series, list(opts))

  p
}

#' add wordcloud
#'
#' @examples
#' tf <- data.frame(terms = c("ECharts", "htmlwidgets", "rstats", "htmltools"),
#'   freq = c(20, 17, 15, 7), color = c("red", "orange", "yellow", "grey"))
#'
#' tf %>%
#'   echart_("terms") %>%
#'   ecloud_("freq", "color")
#'
#' @export
ecloud_ <- function(p, freq, color = NULL, name = NULL, clickable = TRUE, center = list("50%", "50%"), size = list("40%", "40%"),
                   textRotation = list(0, 90), autoSize = list(enable = TRUE, minSize = 12), z = 2, zlevel = 0, tooltip, ...){

  data <- get_dat(freq)

  for(i in 1:length(data)){
    opts <- list(...)
    opts$name <- if(missing(name)) names(data)[i] else name
    opts$type <- "wordCloud"
    opts$clickable <- clickable
    opts$center <- center
    opts$size <- size
    opts$textRotation <- textRotation
    opts$autoSize <- autoSize
    opts$z <- z
    opts$zlevel <- zlevel
    opts$tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item") else tooltip
    opts$data = cloud_data_(data[[i]], freq, color)

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p
}

#' add heatmap
#'
#' @examples
#' set.seed(19880525)
#' matrix <- data.frame(x = runif(150, 10, 500), y = runif(150, 10, 500), z = runif(150, 10 , 200))
#'
#' matrix %>%
#'   echart_("x") %>%
#'   eheatmap_("y", "z")
#'
#' @export
eheatmap_ <- function(p, y, values, name = NULL, clickable = TRUE, blurSize = 30, minAlpha = 0.5, valueScale = 1,
                     opacity = 1, z = 2, zlevel = 0, gradientColors, tooltip, ...){

  data <- get_dat(values)

  for(i in 1:length(data)){
    opts <- list(...)
    opts$name <- if(missing(name)) names(data)[i] else name
    opts$type <- "heatmap"
    opts$tooltip <- if(missing(tooltip)) default_tooltip(trigger = "item") else tooltip
    opts$clickable <- clickable
    opts$blurSize <- blurSize
    opts$minAlpha <- minAlpha
    opts$valueScale <- valueScale
    opts$opacity <- opacity
    opts$z <- z
    opts$zlevel <- zlevel
    opts$gradientColors <- if(missing(gradientColors)) default_gradient() else gradientColors
    opts$data = heat_data_(data[[i]], y, values)

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p
}

#' Add data
#'
#' @export
edata_ <- function(p, data, x){

  # x
  if(!missing(x)){
    xvar <- tryCatch(unlist(unname(data[, x])), error = function(e) e)
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
  if(length(xvar)) assign("x.name", x, envir = data_env)

  p
}
