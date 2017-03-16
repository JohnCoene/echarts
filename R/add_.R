#' Add bars
#'
#' Add bar serie.
#'
#' @param p an echart object.
#' @param serie value column name to plot.
#' @param name of serie.
#' @param stack name of the stack.
#' @param clickable whether plot is clickable.
#' @param xAxisIndex,yAxisIndex axis indexes.
#' @param barGap,barCategoryGap distance between each bar.
#' @param legendHoverLink enables legend hover links.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param tooltip style of tooltip.
#' @param ... any other argument to pass to the serie.
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
#'   ebar_("wt", stack = "grp2") %>% # not stacked
#'   etooltip(trigger = "item") %>%
#'   elegend() %>%
#'   etoolbox_magic(type = list("stack", "tiled")) %>%
#'   etoolbox_restore()
#'
#' df <- data.frame(x = LETTERS[1:4], y = runif(4, 0, 20), z = runif(4, 10, 15), w = runif(4, 15, 30))
#'
#' df %>%
#'   echart(x) %>%
#'   ebar(y, stack = "grp") %>%
#'   ebar(z, stack = "grp") %>%
#'   ebar(w, "grp2") %>%
#'   etheme("macarons") %>%
#'   etooltip(trigger = "axis")
#'
#' @seealso \href{http://echarts.baidu.com/echarts2/doc/option-en.html#series-i(bar)}{official bar options docs}
#'
#' @name ebar
#' @rdname ebar
#'
#' @export
ebar_ <- function(p, serie, name = NULL, stack = NULL, clickable = TRUE, xAxisIndex = 0, yAxisIndex = 0, barGap = "30%",
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
#' @param p an echart object.
#' @param serie value column name to plot.
#' @param name of serie.
#' @param stack name of the stack.
#' @param clickable whether plot is clickable.
#' @param xAxisIndex,yAxisIndex axis indexes.
#' @param symbol symbol for point marker, see details for valid values.
#' @param symbolSize of symbol.
#' @param symbolRotate angle by which symbol is rotated, i.e.: \code{30}.
#' @param showAllSymbol By default, a symbol will show only when its corresponding axis label does.
#' @param smooth whether to smooth line.
#' @param legendHoverLink enables legend hover link to the chart.
#' @param dataFilter ECharts data filtering strategy, see details.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param tooltip style of tooltip.
#' @param ... any other argument to pass to the serie.
#'
#' @details
#' Valid values for \code{symbol}:
#' \itemize{
#'   \item{\code{circle}}
#'   \item{\code{rectangle}}
#'   \item{\code{triangle}}
#'   \item{\code{diamond}}
#'   \item{\code{emptyCircle}}
#'   \item{\code{emptyRectangle}}
#'   \item{\code{emptyTriangle}}
#'   \item{\code{emptyDiamond}}
#'   \item{\code{heart}}
#'   \item{\code{droplet}}
#'   \item{\code{pin}}
#'   \item{\code{arrow}}
#'   \item{\code{star}}
#' }
#'
#' \code{dataFilter}: ECharts will optimize for the situation when data number is much larger than viewport width.
#' It will filter the data showed in one pixel width. And this option is for data filtering strategy.
#'
#' Valid values for \code{dataFilter} are:
#' \itemize{
#'   \item{\code{nearest} (default)}
#'   \item{\code{min}}
#'   \item{\code{max}}
#'   \item{\code{average}}
#' }
#'
#' @examples
#' df <- data.frame(x = 1:50, y = runif(50, 5, 10), z = runif(50, 7, 12), w = runif(50, 10, 13))
#'
#' df %>%
#'   echart(x) %>%
#'   eline(y) %>%
#'   eline(z)
#'
#' # JS sizing function
#' sizing <- htmlwidgets::JS("function(value){ return value[1]/1.5}")
#'
#' df %>%
#'   echart_("x") %>%
#'   eline_("y", "w",
#'          symbolSize = sizing,
#'          showAllSymbol = TRUE,
#'          symbol = "emptyCircle") %>%
#'   etooltip() %>%
#'   etheme("helianthus")
#'
#' df %>%
#'   echart_("x") %>%
#'   eline_("y", stack = "grp") %>%
#'   eline_("z", stack = "grp", symbol = "emptyDroplet", showAllSymbol = TRUE, symbolSize = 5) %>%
#'   eline_("w", showAllSymbol = TRUE, symbolSize = 4, symbol = "emptyHeart", stack = "grp2") %>%
#'   etooltip() %>%
#'   elegend() %>%
#'   etoolbox_magic(type = list("line", "bar"))
#'
#' @seealso \href{http://echarts.baidu.com/echarts2/doc/option-en.html#series-i(line)}{official line options docs}
#'
#' @name eline
#' @rdname eline
#'
#' @export
eline_ <- function(p, serie, name = NULL, stack = NULL, clickable = TRUE, xAxisIndex = 0, yAxisIndex = 0, symbol = NULL,
                  symbolSize = "4", symbolRotate = NULL, showAllSymbol = FALSE, smooth = TRUE, legendHoverLink = TRUE,
                  dataFilter = "nearest", z = 2, zlevel = 0, tooltip, ...){

  tooltip <- if(missing(tooltip)) default_tooltip(trigger = "axis")

  data <- get_dat(serie)

  for(i in 1:length(data)){

    # build $serie
    opts <- list(...)
    opts$name <- if(is.null(name)) names(data)[i] else name
    opts$type <- "line"
    opts$data <- xy_data_(data[[i]], serie, stack)
    opts$stack <- if(!is.null(stack)) stack
    opts$clickable <- clickable
    opts$xAxisIndex <- xAxisIndex
    opts$yAxisIndex <- yAxisIndex
    opts$symbol <- symbol
    opts$symbolSize <- symbolSize
    opts$symbolRotate <- symbolRotate
    opts$showAllSymbol <- showAllSymbol
    opts$smooth <- smooth
    opts$dataFilter <- dataFilter
    opts$legendHoverLink <- legendHoverLink
    opts$z <- z
    opts$zlevel <- zlevel

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

  p <- adjust_axis(p, data, stack)

  p
}

#' Add area
#'
#' Add area serie.
#'
#' @param p an echart object.
#' @param serie value column name to plot.
#' @param name of serie.
#' @param stack name of the stack.
#' @param smooth whether to smooth line.
#' @param ... any other argument to pass to the serie. i.e.: same parameters as \code{\link{eline}} or \code{\link{eline_}}
#'
#' @examples
#' df <- data.frame(x = LETTERS[1:10], y = runif(10, 30, 70), z = runif(10, 10, 50))
#'
#' df %>%
#'   echart_("x") %>%
#'   earea_("y", smooth = FALSE, symbol = "emptyRectangle", symbolSize = 5)
#'
#' df %>%
#'   echart(x) %>%
#'   earea(y, stack = "grp") %>%
#'   earea(z, stack = "grp") %>%
#'   etheme("roma")
#'
#' df <- data.frame(x = 1:10, y = runif(10, 30, 70), z = runif(10, 10, 50))
#'
#' df %>%
#'   echart(x) %>%
#'   earea(z, stack = "grp") %>%
#'   earea(y)
#'
#' @name earea
#' @rdname earea
#'
#' @export
earea_ <- function(p, serie, name = NULL, stack = NULL, smooth = TRUE, ...){

  data <- get_dat(serie)

  for(i in 1:length(data)){
    # build $serie
    opts <- list(...)
    opts$name <- if(is.null(name)) names(data)[i] else name
    opts$type <- "line"
    opts$data <- xy_data_(data[[i]], serie, stack)
    opts$smooth <- smooth
    opts$stack <- if(!is.null(stack)) stack
    opts$itemStyle <-  list(normal= list(areaStyle = list(type = 'default')))

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

  p <- adjust_axis(p, data, stack)

  p
}

#' Add scatter
#'
#' Add scatter serie.
#'
#' @param p an echart object.
#' @param serie value column name to plot.
#' @param name of serie.
#' @param size size of points/bubble.
#' @param clickable whether plot is clickable.
#' @param symbol marker, see details for valid values.
#' @param symbolSize of symbol.
#' @param symbolRotate angle by which symbol is rotated, i.e.: \code{30}.
#' @param large enables large scale scatter.
#' @param largeThreshold threshold of large scale scatter anto-switch.
#' @param legendHoverLink enables legend hover links.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param ... any other options to pass to the serie.
#'
#' @details
#' Valid values for \code{symbol}:
#' \itemize{
#'   \item{\code{circle}}
#'   \item{\code{rectangle}}
#'   \item{\code{triangle}}
#'   \item{\code{diamond}}
#'   \item{\code{emptyCircle}}
#'   \item{\code{emptyRectangle}}
#'   \item{\code{emptyTriangle}}
#'   \item{\code{emptyDiamond}}
#'   \item{\code{heart}}
#'   \item{\code{droplet}}
#'   \item{\code{pin}}
#'   \item{\code{arrow}}
#'   \item{\code{star}}
#' }
#'
#' @examples
#' mtcars %>%
#'   echart_("disp") %>%
#'   escatter_("mpg", symbol = "emptyCircle") %>%
#'   exAxis()
#'
#' mtcars %>%
#'   echart(disp) %>%
#'   escatter(mpg, qsec, symbolSize = 15) %>%
#'   exAxis_value(axisLabel = list(show = FALSE)) %>%
#'   etheme("mint") %>%
#'   eanimation(animationEasing = "ElasticOut")
#'
#' @seealso \href{http://echarts.baidu.com/echarts2/doc/option-en.html#series-i(scatter)}{official scatter options docs}
#'
#' @name escatter
#' @rdname escatter
#'
#' @export
escatter_ <- function(p, serie, size = NULL, name = NULL, clickable = TRUE, symbol = NULL, symbolSize = 4, symbolRotate = NULL,
                      large = FALSE, largeThreshold = 2000, legendHoverLink = TRUE, z = 2, zlevel = 0, ...){

  data <- get_dat(serie)

  for(i in 1:length(data)){
    # build $serie
    opts <- list(...)
    opts$name <- if(is.null(name)) names(data)[i] else name
    opts$type <- "scatter"
    opts$data <- scatter_data_(data[[i]], serie, size, symbolSize)
    opts$clickable <- clickable
    opts$symbol <- symbol
    opts$symbolSize <- if(!is.null(size)) scatter_size(size) else symbolSize
    opts$symbolRotate <- symbolRotate
    opts$large <- large
    opts$largeThreshold <- largeThreshold
    opts$legendHoverLink <- legendHoverLink
    opts$z <- z
    opts$zlevel <- 0

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

  p <- adjust_axis(p, data, NULL)

  # change axis type
  p$x$options$yAxis <- list(list(type = "value"))

  p
}

#' Add pie
#'
#' Add pie chart
#'
#' @param p an echart object.
#' @param serie value column name to plot.
#' @param name of serie.
#' @param clickable whether plot is clickable.
#' @param legendHoverLink enables legend hover links.
#' @param center coordinates of the center.
#' @param radius radius in pixels or percent.
#' @param startAngle,minAngle start and minimum angle.
#' @param clockWise whether to display slices in clockwise direction
#' @param roseType type of pie, takes \code{NULL}, \code{area} or \code{radius}, see examples.
#' @param selectedOffset offset of selected slice.
#' @param selectedMode whether slices are selectable.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param ... any other option to pass to serie.
#'
#' @examples
#' pie <- data.frame(name = c("banana", "apple", "pineapple", "onion"),
#'   value = c(26, 15, 12, 9))
#'
#' pie %>%
#'   echart_("name") %>%
#'   epie(value)
#'
#' pie %>%
#'   echart(name) %>%
#'   epie(value, roseType = "area") %>%
#'   etheme("helianthus")
#'
#' pie %>%
#'   echart_("name") %>%
#'   epie_("value", roseType = "radius") %>%
#'   etheme("blue")
#'
#' @seealso \href{http://echarts.baidu.com/echarts2/doc/option-en.html#series-i(pie)}{official pie options docs}
#'
#' @name epie
#' @rdname epie
#'
#' @export
epie_ <- function(p, serie, name = NULL, clickable = TRUE, legendHoverLink = TRUE, center = list("50%", "50%"),
                  radius = list(0, "75%"), startAngle = 90, minAngle = 0, clockWise = TRUE, roseType = NULL, selectedOffset = 10,
                  selectedMode = TRUE, z = 2, zlevel = 0, ...){

  data <- get_dat(serie)

  for(i in 1:length(data)){

    # build $serie
    opts <- list(...)
    opts$name <- if(is.null(name)) names(data)[i] else name
    opts$type <- "pie"
    opts$data <- val_name_data_(data[[i]], serie)
    opts$clickable <- clickable
    opts$legendHoverLink <- legendHoverLink
    opts$center <- center
    opts$radius <- radius
    opts$startAngle <- startAngle
    opts$minAngle <- minAngle
    opts$clockWise <- clockWise
    opts$roseType <- roseType
    opts$selectedOffset <- selectedOffset
    opts$selectedMode <- selectedMode
    opts$z <- z
    opts$zlevel <- zlevel

    p$x$options$xAxis <- NULL
    p$x$options$yAxis <- NULL

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

  p$x$options$legend$data <- p$x$options$legend$data <- get_pie_legend()

  p
}

#' Add radar
#'
#' Add radar chart.
#'
#' @param p an echart object.
#' @param serie value column name to plot.
#' @param name of serie.
#' @param clickable whether plot is clickable.
#' @param symbol marker, see details for valid values.
#' @param symbolSize of symbol.
#' @param symbolRotate angle by which symbol is rotated, i.e.: \code{30}.
#' @param legendHoverLink enables legend hover links.
#' @param polarIndex polar coordinates index.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param ... any other options to pass to the serie.
#'
#' @details
#' Valid values for \code{symbol}:
#' \itemize{
#'   \item{\code{circle}}
#'   \item{\code{rectangle}}
#'   \item{\code{triangle}}
#'   \item{\code{diamond}}
#'   \item{\code{emptyCircle}}
#'   \item{\code{emptyRectangle}}
#'   \item{\code{emptyTriangle}}
#'   \item{\code{emptyDiamond}}
#'   \item{\code{heart}}
#'   \item{\code{droplet}}
#'   \item{\code{pin}}
#'   \item{\code{arrow}}
#'   \item{\code{star}}
#' }
#'
#' @examples
#' radar <- data.frame(axis = rep(LETTERS[1:6], 4), grp = sample(LETTERS[4:9], 24, replace = TRUE),
#'                     value = runif(24, 2, 10))
#'
#' radar %>%
#'   group_by_("grp") %>%
#'   echart_("axis") %>%
#'   eradar_("value", symbolSize = 0) %>%
#'   elegend() %>%
#'   etheme("macarons")
#'
#' radar %>%
#'   group_by_("grp") %>%
#'   echart_("axis") %>%
#'   eradar_("value", symbolSize = htmlwidgets::JS("function(value){return(value)}")) %>%
#'   elegend() %>%
#'   etheme("roma")
#'
#' @seealso \href{http://echarts.baidu.com/echarts2/doc/option-en.html#series-i(radar)}{official radar options docs}
#'
#' @name eradar
#' @rdname eradar
#'
#' @export
eradar_ <- function(p, serie, name = NULL, clickable = TRUE, symbol = NULL, symbolSize = 4, symbolRotate = NULL,
                    legendHoverLink = TRUE, polarIndex = 0, z = 2, zlevel = 0, ...){

  data <- get_dat(serie)

  for(i in 1:length(data)){

    n <- if(is.null(name)) names(data)[i] else name

    # build $serie
    opts <- list(...)
    opts$name <- n
    opts$type <- "radar"
    opts$data <- list(list(value = vector_data_(data[[i]], serie), name = n))
    opts$clickable <- clickable
    opts$symbol <- symbol
    opts$symbolSize <- symbolSize
    opts$symbolRotate <- symbolRotate
    opts$legendHoverLink <- legendHoverLink
    opts$polarIndex <- polarIndex
    opts$z <- opts$z
    opts$zlevel <- zlevel

    p$x$options$series <- append(p$x$options$series, list(opts))

    # set polar $indicator
    p$x$options$polar <- list(
      list(
        indicator = polar_indicator()
      )
    )
  }

  # remove axis
  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p
}


#' Add chord
#'
#' Add chord chart.
#'
#' @param p an echart object.
#' @param name name of serie.
#' @param sort,sortSub data sorting, \code{none}, \code{ascending} or \code{descending}.
#' @param clickable whether plot is clickable.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param symbol marker, see details for valid values.
#' @param symbolSize of symbol.
#' @param clockWise whether links are displayed in clockwise direction.
#' @param minRadius,maxRadius minimum and maximum radius after mapping to symbol size.
#' @param showScale whether the scale will be showed. Only available if ribbonType is true.
#' @param showScaleText whether to show scale text.
#' @param padding distance between each sector.
#' @param ribbonType set to \code{TRUE} to use ribbons.
#' @param ... any other options to pass to serie.
#'
#' @details
#' Valid values for \code{symbol}:
#' \itemize{
#'   \item{\code{circle}}
#'   \item{\code{rectangle}}
#'   \item{\code{triangle}}
#'   \item{\code{diamond}}
#'   \item{\code{emptyCircle}}
#'   \item{\code{emptyRectangle}}
#'   \item{\code{emptyTriangle}}
#'   \item{\code{emptyDiamond}}
#'   \item{\code{heart}}
#'   \item{\code{droplet}}
#'   \item{\code{pin}}
#'   \item{\code{arrow}}
#'   \item{\code{star}}
#' }
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
#'   echart(LETTERS[1:10]) %>%
#'   echord(ribbonType = FALSE)
#'
#' @seealso \href{http://echarts.baidu.com/echarts2/doc/option-en.html#series-i(radar)}{official scatter options docs}
#'
#' @name echord
#' @rdname echord
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
#' @param p an echart object.
#' @param serie values to plot.
#'
#' @examples
#' choropleth <- data.frame(countries = c("France", "Brazil", "China", "Russia", "Canada", "India"),
#'   values = round(runif(6, 10, 25)))
#'
#' choropleth %>%
#'   echart_("countries") %>%
#'   emap_() %>%
#'   emap_choropleth_("values")
#'
#' choropleth %>%
#'   echart_("countries") %>%
#'   emap() %>%
#'   emap_choropleth(values) %>%
#'   ecolorbar(color = list("red", "yellow"), calculable = TRUE)
#'
#' @seealso \code{\link{ecolorbar}}
#'
#' @name emap_choropleth
#' @rdname emap_choropleth
#'
#' @export
emap_choropleth_ <- function(p, serie){

  data <- get_dat(serie)

  p$x$options$dataRange <- default_dataRange_(data[[1]], serie)

  previous <- length(p$x$options$series)
  p$x$options$series[[previous]]$data <- val_name_data_(data[[1]], serie)
  p$x$options$series[[previous]]$hoverable <- TRUE

  p
}

#' Add map coordinates
#'
#' Add coordinates to map.
#'
#' @param p an echart object
#' @param lon,lat coordinates to plot.
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
#' @name emap_coords
#' @rdname emap_coords
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
#' Add lines on map.
#'
#' @param p an echart object.
#' @param edges edges data.frame.
#' @param source,target source and target columns in edges data.frame.
#' @param name name of serie.
#' @param clickable whether lines are clikable.
#' @param symbol symbol, see valid details for valid values.
#' @param symbolSize of symbol.
#' @param symbolRotate angle by which symbol is rotated, i.e.: \code{30}.
#' @param large optimises for 2'000 data points and over.
#' @param smooth whether to smooth lines.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param smoothness line smoothness
#' @param precision for 'average'.
#' @param bundling edge bundling settings, see usage.
#' @param ... any other options to pass to line.
#'
#' @details
#' Valid values for \code{symbol}:
#' \itemize{
#'   \item{\code{circle}}
#'   \item{\code{rectangle}}
#'   \item{\code{triangle}}
#'   \item{\code{diamond}}
#'   \item{\code{emptyCircle}}
#'   \item{\code{emptyRectangle}}
#'   \item{\code{emptyTriangle}}
#'   \item{\code{emptyDiamond}}
#'   \item{\code{heart}}
#'   \item{\code{droplet}}
#'   \item{\code{pin}}
#'   \item{\code{arrow}}
#'   \item{\code{star}}
#' }
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
#' @seealso \code{\link{emap_coords}} \href{http://echarts.baidu.com/echarts2/doc/option-en.html#series-i(map).markLine}{official map line docs}
#'
#' @name emap_lines
#' @rdname emap_lines
#'
#' @export
emap_lines_ <- function(p, edges, source, target, name = NULL, clickable = TRUE, symbol = "arrow",
                       symbolSize = 2, symbolRotate = NULL, large = FALSE, smooth = TRUE, z = 2, zlevel = 0,
                       smoothness = 0.2, precision = 2, bundling = list(enable = FALSE, maxTurningAngle = 45), ...){

  edges <- map_grps_(edges)

  for(i in 1:length(edges)){
    opts <- list(...)
    opts$name <- if(is.null(name)) names(edges)[i] else name
    opts$clickable <- clickable
    opts$symbol <- symbol
    opts$symbolSize <- symbolSize
    opts$symbolRotate <- symbolRotate
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
#' @param p an echart objects.
#' @param serie values to plot.
#' @param clickable whether points are clickable.
#' @param symbol point symbol, see details for valid values.
#' @param symbolSize size of points.
#' @param symbolRotate angle by which symbol is rotated, i.e.: \code{30}.
#' @param large whether to optimise for large datasets: 2K points +.
#' @param itemStyle cutomise points.
#' @param ... any other option to pass to points.
#'
#' @details
#' Valid values for \code{symbol}:
#' \itemize{
#'   \item{\code{circle}}
#'   \item{\code{rectangle}}
#'   \item{\code{triangle}}
#'   \item{\code{diamond}}
#'   \item{\code{emptyCircle}}
#'   \item{\code{emptyRectangle}}
#'   \item{\code{emptyTriangle}}
#'   \item{\code{emptyDiamond}}
#'   \item{\code{heart}}
#'   \item{\code{droplet}}
#'   \item{\code{pin}}
#'   \item{\code{arrow}}
#'   \item{\code{star}}
#' }
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
#' @seealso \href{http://echarts.baidu.com/echarts2/doc/option-en.html#series-i(map).markPoint}{office map points docs}
#'
#' @name emap_points
#' @rdname emap_points
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
#' @param p an echart object.
#' @param lon,lat coordinates.
#' @param z values, heat.
#' @param blurSize blur of points.
#' @param minAlpha minimum transparency.
#' @param valueScale \code{z} multiplier.
#' @param opacity opacity of heatmap.
#' @param gradientColors colors.
#' @param ... any other parameter to pass to heatmap.
#'
#' @examples
#' data <- data.frame(lon = runif(300, 90, 120),
#'   lat = runif(300, 30, 39),
#'   z = runif(300, 75, 100))
#'
#' data %>%
#'   echart_() %>%
#'   emap(mapType = "china") %>%
#'   emap_heat_("lon", "lat", "z")
#'
#' data %>%
#'   echart() %>%
#'   emap(mapType = "china") %>%
#'   emap_heat_("lon", "lat", "z", blurSize = 50, minAlpha = 0.3, opacity = 0.8)
#'
#' @name emap_heat
#' @rdname emap_heat
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
#' @param p an echart object.
#' @param mapType type of map, see examples.
#' @param name name of serie.
#' @param clickable whether elements are clickable.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param selectedMode whether items can be selected.
#' @param hoverable whether elements are hoverable.
#' @param dataRangeHoverLink enables dataRange hover link to the chart.
#' @param mapLocation x and y location of map on canvas, takes \code{top}, \code{bottom}, \code{left}, \code{right}, \code{center}.
#' @param mapValueCalculation takes \code{sum} or \code{average}.
#' @param mapValuePrecision decimal precision.
#' @param showLegendSymbol whether to show symbol on legend.
#' @param roam enables zoom and drag.
#' @param scaleLimit controls drag and zoom limits.
#' @param nameMap custom name mapping.
#' @param textFixed fixed text location for a region.
#' @param ... any other options to pass to map serie.
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
#' us_data <- data.frame(state = c("New York", "Los Angeles", "Dallas"),
#'                       lat = c(40.730610, 34.052235, 33.940369),
#'                       lon = c(-73.935242, -118.243683, -84.692894),
#'                       values = round(runif(3, 1, 2)))
#'
#' us_data %>%
#'   echart_("state") %>%
#'   emap(mapType = "world|United States of America") %>%
#'   emap_coords_("lon", "lat") %>%
#'   emap_points_("values")
#'
#' @seealso \code{\link{emap_coords}}, \code{\link{emap_heat}}, \code{\link{emap_lines}}, \code{emap_choropleth},
#' \code{\link{emap_points}}, \href{http://echarts.baidu.com/echarts2/doc/option-en.html#series-i(map)}{official map docs}
#'
#' @name emap
#' @rdname emap
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
#' Add gauge.
#'
#' @param p an echart object.
#' @param value value to plot.
#' @param indicator indicator appearing in center of gauge.
#' @param name name of serie.
#' @param clickable whether the item is clickable.
#' @param legendHoverLink enables legend hover link.
#' @param center center of gauge in pixels of percent.
#' @param radius radius of gauge in pixels of percent.
#' @param startAngle,endAngle start and end angles of gauge.
#' @param min,max minimum and maximum of gauge.
#' @param splitNumber number of segments.
#' @param tooltip customise tooltip.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param ... any other arguments.
#'
#' @examples
#' echart() %>%
#'   egauge(85, "SPEED")
#'
#' echart() %>%
#'   egauge(25, "SPEED") %>%
#'   etheme("helianthus")
#'
#' echart() %>%
#'   egauge(63, "PERCENT") %>%
#'   etheme("dark")
#'
#' @seealso \href{http://echarts.baidu.com/echarts2/doc/option-en.html#series-i(gauge)}{official gauge docs}
#'
#' @name egauge
#' @rdname egauge
#'
#' @export
egauge_ <- function(p, value, indicator = "", name = NULL, clickable = FALSE, legendHoverLink = TRUE, center = list("50%", "50%"),
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
#' @param p an echart object.
#' @param serie values to plot.
#' @param name name of serie.
#' @param clickable whether segments are clickable.
#' @param legendHoverLink enables legend hover link.
#' @param sort data sorting, takes \code{descending} or \code{ascending}.
#' @param min,max minimum and maximum values of funnel.
#' @param x,y,x2,y2 coordinates of funnel.
#' @param width,height width and height of funnel.
#' @param funnelAlign alignment of funnel takes \code{left}, \code{right} and \code{center}.
#' @param minSize,maxSize minimum and maximum size of funnel.
#' @param gap gap between segments.
#' @param tooltip cutomise tooltip.
#' @param ... any other argument to pass to funnel.
#'
#' @examples
#' funnel <- data.frame(stage = c("View", "Click", "Purchase"), value = c(80, 30, 20))
#'
#' funnel %>%
#'   echart_("stage") %>%
#'   efunnel_("value")
#'
#' @seealso \href{http://echarts.baidu.com/echarts2/doc/option-en.html#series-i(funnel)}{official funnel docs}
#'
#' @name efunnel
#' @rdname efunnel
#'
#' @export
efunnel_ <- function(p, serie, name = NULL, clickable = TRUE, legendHoverLink = TRUE, sort = "descending",
                    min = 0, max = NULL, x = 80, y = 60, x2 = 80, y2 = 60, width = NULL, height = NULL,
                    funnelAlign = "center", minSize = "0%", maxSize = "100%", gap = 0, tooltip, ...){

  data <- get_dat(serie)

  if(is.null(max)) max <- compute_max(data, serie)

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

  p$x$options$legend$data <- get_pie_legend() # legend

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p
}

#' Add venn
#'
#' Add venn diagram
#'
#' @param p an echart object.
#' @param serie a named vector, see details.
#' @param name name of serie.
#' @param clickable whether ciorcles are clickable.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param tooltip cutomise tooltip.
#' @param ... any other argument to pass to funnel.
#'
#' @examples
#' venn <- data.frame(name = c("banana", "pineapple", "overlap"),
#'   values = c(20, 50, 10))
#'
#' venn %>%
#'   echart_("name") %>%
#'   evenn_("values") %>%
#'   etheme("macarons2")
#'
#' @seealso \href{http://echarts.baidu.com/echarts2/doc/option-en.html#series-i(venn)}{official venn docs}
#'
#' @name evenn
#' @rdname evenn
#'
#' @export
evenn_ <- function(p, serie, name = NULL, clickable = TRUE, z = 2, zlevel = 0, tooltip = NULL, ...){

  data <- get_dat(serie)

  name <- if(is.null(name)) names(data)[1] else name
  if(is.null(tooltip)) tooltip <- default_tooltip(trigger = "item")
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

#' Add wordcloud
#'
#' Add wordcloud serie.
#'
#' @param p an echart object.
#' @param freq frequencies.
#' @param color color of terms.
#' @param name name of wordcloud.
#' @param clickable whether terms are clickable.
#' @param center center of cloud.
#' @param size size of cloud.
#' @param textRotation horizontal and vertical text rotation.
#' @param autoSize automatic text size computation.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param tooltip cutomise tooltip.
#' @param ... any other argument to pass to funnel.
#'
#' @examples
#' tf <- data.frame(terms = c("ECharts", "htmlwidgets", "rstats", "htmltools"),
#'   freq = c(20, 17, 15, 7), color = c("red", "orange", "yellow", "grey"))
#'
#' tf %>%
#'   echart_("terms") %>%
#'   ecloud_("freq", "color") %>%
#'   etooltip()
#'
#' @seealso \href{http://echarts.baidu.com/echarts2/doc/option-en.html#series-i(wordCloud)}{official wordcloud docs}
#'
#' @name ecloud
#' @rdname ecloud
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

#' Add heatmap
#'
#' Add heatmap.
#'
#' @param p an echart object.
#' @param y yaxis values.
#' @param values heat.
#' @param name name of serie.
#' @param clickable whether chart is clickable.
#' @param blurSize size of points blur.
#' @param minAlpha minimum transparency.
#' @param valueScale \code{values} multiplier.
#' @param opacity opacity of heatmap.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param gradientColors colors used for gradient as a \code{list} i.e.:\code{list("red", "blue")}
#' @param tooltip cutomise tooltip.
#' @param ... any other options to pass to heatmap.
#'
#' @examples
#' set.seed(19880525)
#' matrix <- data.frame(x = runif(150, 10, 500), y = runif(150, 10, 500), z = runif(150, 10 , 200))
#'
#' matrix %>%
#'   echart_("x") %>%
#'   eheatmap_("y", "z")
#'
#' @seealso \href{http://echarts.baidu.com/echarts2/doc/option-en.html#series-i(heatmap)}{official heatmap docs}
#'
#' @name eheatmap
#' @rdname eheatmap
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
#' Add a dataset.
#'
#' @param p an echart object.
#' @param data data.frame.
#' @param x x variable.
#'
#' @seealso \code{emap}
#'
#' @name edata
#' @rdname edata
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

#' Add Treemap
#'
#' @param p an echart object.
#' @param serie values to plot.
#' @param name name of serie.
#' @param itemStyle style of rectangles.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param center center of map.
#' @param clickable whether rectangles are clickable.
#' @param size size of chart.
#' @param ... any other option to pass to treemap.
#'
#' @examples
#' df <- data.frame(name = LETTERS[1:10], values = round(runif(10, 1, 10)))
#'
#' df %>%
#'   echart_("name") %>%
#'   etreemap_("values") %>%
#'   etooltip(trigger = "item") %>%
#'   etheme("macarons")
#'
#' @name etreemap
#' @rdname etreemap
#'
#' @export
etreemap_ <- function(p, serie, name = NULL, itemStyle = NULL, clickable = FALSE, center = list("50%", "50%"),
                      size = list("80%", "80%"), z = 2, zlevel = 0, ...){

  data <- get_dat(serie)

  if(is.null(itemStyle)) itemStyle <- list(normal = list(label = list(show = TRUE), borderWidth = 1),
                                           emphasis = list(label = list(show = TRUE)))

  for(i in 1:length(data)){
    opts <- list(...)
    opts$name <- if(is.null(name)) names(data)[i] else name
    opts$type <- "treemap"
    opts$itemStyle <- itemStyle
    opts$data <- treemap_data_(data[[i]], serie)

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

  # remove axis
  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p
}

#' Add candlestick
#'
#' Add candlestick bars.
#'
#' @param p an echart object.
#' @param opening,closing,low,high stock prices.
#' @param name name of serie.
#' @param clickable whether serie is clickable.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param ... any other options to pass to candlessticks.
#'
#' @examples
#' # generate data
#' date <- c("2017-01-01", "2017-01-02", "2017-01-03", "2017-01-04", "2017-03-05",
#'           "2017-01-06", "2017-01-07")
#' stock <- data.frame(date = date,
#'                     opening = c(200.60, 200.22, 198.43, 199.05, 203.54, 203.40, 208.34),
#'                     closing = c(200.72, 198.85, 199.05, 203.73, 204.08, 208.11, 211.88),
#'                     low = c(197.82, 198.07, 197.90, 198.10, 202.00, 201.50, 207.60),
#'                     high = c(203.32, 200.67, 200.00, 203.95, 204.90, 208.44, 213.17))
#'
#' stock %>%
#'   echart_("date") %>%
#'   ecandle_("opening", "closing", "low", "high")
#'
#'
#' js <- htmlwidgets::JS("function(params){
#'   var res = 'opening: ' + params.value[0] + '<br>' + 'closing: ' + params.value[3];
#'   return res}")
#'
#' stock %>%
#'   echart(date) %>%
#'   ecandle(opening, closing, low, high, barMaxWidth = 20) %>%
#'   etooltip(trigger = "item", formatter = js) %>%
#'   etheme("macarons")
#'
#' @rdname candlestick
#' @name candlestick
#'
#' @seealso \href{http://echarts.baidu.com/echarts2/doc/option-en.html#title~series-i(k)}{candlestick official docs}
#'
#' @export
ecandle_ <- function(p, opening, closing, low, high, name = NULL, clickable = TRUE, z = 2, zlevel = 0, ...){

  dat <- get("data", envir = data_env)

  for(i in 1:length(dat)){
    opts <- list(...)
    opts$name <- if(is.null(name)) names(dat)[i] else name
    opts$type <- "k"
    opts$data <- candle_data_(dat[[i]], opening, closing, low, high)

    p$x$options$series <- append(p$x$options$series, list(opts))
  }

  p <- p %>%
    eyAxis(type = "value", scale = TRUE)

  p
}
