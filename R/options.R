#' Add global options
#'
#' Add global options.
#'
#' @param p an echart object.
#' @param backgroundColor background color.
#' @param color colors to use in chart.
#' @param renderAsImage allows rendering as image.
#' @param calculable specifies whether the drag-recalculate feature will be enabled.
#' @param symbolList list of default symbols.
#' @param ... any other options.
#'
#' @examples
#' mtcars %>%
#'   echart(mpg) %>%
#'   eline(qsec) %>%
#'   eoptions(backgroundColor = "black")
#'
#' @export
eoptions <- function(p, backgroundColor = NULL, renderAsImage = FALSE, calculable = FALSE, color = NULL, symbolList = NULL, ...){

  opts <- list(...)
  opts$backgroundColor <- backgroundColor
  opts$color <- if(!is.null(color)) color
  opts$renderAsImage <- renderAsImage
  opts$calculable <- calculable
  opts$symbolList <- if(!is.null(symbolList)) symbolList

  p$x$options <- append(p$x$options, opts)

  p

}

#' Add animations
#'
#' Add animations
#'
#' @param p an echart objects.
#' @param animation whether to show the initial animation.
#' @param addDataAnimation specifies whether the dynamic data interface animation will be enabled.
#' @param animationThreshold threshold of animated elements.
#' @param animationDuration duration of animation, in ms.
#' @param animationDurationUpdate duration of the update animation, in ms.
#' @param animationEasing easing effect, see details for valid values.
#' @param ... any other options.
#'
#' @details
#' \itemize{
#'   \item{\code{linear}}
#'   \item{\code{QuadraticIn}}
#'   \item{\code{QuadraticOut}}
#'   \item{\code{QuadraticInOut}}
#'   \item{\code{CubicIn}}
#'   \item{\code{CubicOut}}
#'   \item{\code{CubicInOut}}
#'   \item{\code{QuarticIn}}
#'   \item{\code{QuarticOut}}
#'   \item{\code{QuarticInOut}}
#'   \item{\code{SinusoidalIn}}
#'   \item{\code{SinusoidalOut}}
#'   \item{\code{SinusoidalInOut}}
#'   \item{\code{ExponentialIn}}
#'   \item{\code{ExponentialOut}}
#'   \item{\code{ExponentialInOut}}
#'   \item{\code{CircularIn}}
#'   \item{\code{CircularOut}}
#'   \item{\code{CircularInOut}}
#'   \item{\code{ElasticIn}}
#'   \item{\code{ElasticOut}}
#'   \item{\code{ElasticInOut}}
#'   \item{\code{BackIn}}
#'   \item{\code{BackOut}}
#'   \item{\code{BackInOut}}
#'   \item{\code{BounceIn}}
#'   \item{\code{BounceOut}}
#'   \item{\code{BounceInOut}}
#' }
#'
#' @examples
#' mtcars %>%
#'   echart(mpg) %>%
#'   ebar(qsec) %>%
#'   eanimation(animationEasing = "BounceIn")
#'
#' mtcars %>%
#'   echart(mpg) %>%
#'   escatter(qsec, drat, symbolSize = 20) %>%
#'   eanimation(animationEasing = "CubicInOut")
#'
#' @export
eanimation <- function(p, animation = TRUE, addDataAnimation = TRUE, animationThreshold = 2000,
                       animationDuration = 2000, animationDurationUpdate = 500,
                       animationEasing = "ExponentialOut", ...){

  opts <- list(...)
  opts$animation <- animation
  opts$addDataAnimation <- addDataAnimation
  opts$animationThreshold <- animationThreshold
  opts$animationDuration <- animationDuration
  opts$animationDurationUpdate <- animationDurationUpdate
  opts$animationEasing = animationEasing

  p$x$options <- append(p$x$options, opts)

  p

}

#' Add legend
#'
#' @param p an echart object.
#' @param legend legend.
#' @param show wether to show legend.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param orient orientation, \code{vertical} or \code{horizontal}.
#' @param x x alignment, \code{center}, \code{left} or \code{right}.
#' @param y y alignment, \code{center}, \code{top} or \code{bottom}.
#' @param backgroundColor background color.
#' @param borderColor border color.
#' @param borderWidth border width.
#' @param selectedMode selection mode.
#' @param selected  default selected state.
#' @param textStyle textStyle.
#' @param formatter default formatter.
#' @param itemGap gap between legend items.
#' @param itemWidth,itemHeight width and height of items.
#' @param padding legend padding.
#' @param ... any other option to pass to legend.
#'
#' @examples
#' df <- data.frame(x = LETTERS[1:10], y = runif(10, 0, 10), z = runif(10, 0, 10))
#'
#' df %>%
#'   echart(x) %>%
#'   ebar(y, name = "y - serie") %>%
#'   ebar(z) %>%
#'   elegend()
#'
#' @export
elegend <- function(p, legend, show = TRUE, zlevel = 0, z = 4, orient = "horizontal", x = "center",
                    y = "top", backgroundColor = "rgba(0,0,0,0)", borderColor = "#ccc", borderWidth = 0,
                    padding = 5, itemGap = 10, itemWidth = 20, itemHeight = 14, formatter = NULL,
                    selectedMode = TRUE, selected = NULL, textStyle, ...){

  textStyle <- if(missing(textStyle)) list(fontFamily = "Arial, Verdana, sans-serif", fontSize = 12,
                                           fontStyle = "normal", fontWeight = "normal")

  if(missing(legend) && !length(p$x$options$legend$data)){
    legend <- if(missing(legend)) default_legend(p)
  } else if(missing(legend) && length(p$x$options$legend$data)) {
    legend <- p$x$options$legend$data
  } else if (!missing(legend)) {
    legend <- legend
  }

  opts <- list(...)
  opts$data <- legend
  opts$show <- show
  opts$zlevel <- zlevel
  opts$z <- z
  opts$orient <- orient
  opts$x <- x
  opts$y <- y
  opts$backgroundColor <- backgroundColor
  opts$borderColor <- borderColor
  opts$borderWidth <- borderWidth
  opts$padding <- padding
  opts$itemGap <- itemGap
  opts$itemWidth <- itemWidth
  opts$itemHeight <- itemHeight
  opts$formatter <- formatter
  opts$selectMode <- selectedMode
  opts$selected <- selected
  opts$textStyle <- textStyle

  p$x$options$legend <- append(p$x$options$legend, opts)

  p

}

#' Add theme
#'
#' Add a theme.
#'
#' @param p an echart object.
#' @param theme, see details for valid values.
#'
#' @details
#' valid themes:
#' \itemize{
#'   \item{\code{default}}
#'   \item{\code{mint}}
#'   \item{\code{macarons}}
#'   \item{\code{macarons2}}
#'   \item{\code{green}}
#'   \item{\code{blue}}
#'   \item{\code{dark}}
#'   \item{\code{gray}}
#'   \item{\code{helianthus}}
#'   \item{\code{red}}
#'   \item{\code{roma}}
#'   \item{\code{sakura}}
#'   \item{\code{shine}}
#'   \item{\code{infographic}}
#'   \item{\code{solarlight}}
#' }
#'
#' @examples
#' mtcars %>%
#'   echart(disp) %>%
#'   ebar(qsec) %>%
#'   ebar(mpg) %>%
#'   etheme("roma")
#'
#' @export
etheme <- function(p, theme = "default"){

  themes <- c("default", "mint", "macarons", "macarons2", "green", "blue", "dark", "blue", "dark", "gray", "helianthus",
              "red", "roma", "sakura", "shine", "infographic", "solarlight")

  if(!tolower(theme) %in% themes) stop("invalid theme")

  p$x$theme <- tolower(theme)

  p
}

#' Add tooltip
#'
#' Customise tooltip.
#'
#' @param p an echart object.
#' @param show whether to show the tooltip.
#' @param trigger element that triggers the tooltip, takes \code{item}, \code{axis}.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param showContent whether to show the content of tooltip.
#' @param position specifies position, pass a \code{list}, like \code{list(10, 10)}, fixed position; pass a function,
#' like \code{htmlwidgets::JS("function([x, y]) {return [x + 10, y + 10]}")}
#' @param formatter see \href{official documentation}{http://echarts.baidu.com/echarts2/doc/option-en.html#tooltip.formatter} for more details.
#' @param islandFormatter island content formatter.
#' @param showDelay number of milliseconds the tooltip shows.
#' @param hideDelay number of milliseconds to wait until the tooltip is hidden when mouse out from a point or chart.
#' @param transitionDuration duration in seconds of the animated transition.
#' @param enterable whether to let the mouse go into the tip dom.
#' @param backgroundColor background color.
#' @param borderColor border color.
#' @param borderRadius border radius.
#' @param borderWidth border width.
#' @param padding padding.
#' @param axisPointer axis pointer, triggered by axis.
#' @param textStyle tooltip text size.
#' @param ... any other options to pass to tooltip.
#'
#' @examples
#' mtcars %>%
#'   echart(disp) %>%
#'   eline(mpg) %>%
#'   eline(qsec) %>%
#'   etooltip(trigger = "axis")
#'
#' @export
etooltip <- function(p, show = TRUE, trigger = "axis", zlevel = 1, z = 8, showContent = TRUE,
                     position = NULL, formatter = NULL, islandFormatter = "{a} < br/>{b} : {c}",
                     showDelay = 20, hideDelay = 100, transitionDuration = 0.4, enterable = FALSE,
                     backgroundColor = "rgba(0,0,0,0.7)", borderColor = "#333", borderRadius = 4,
                     borderWidth = 0, padding = 5, axisPointer, textStyle, ...){

  opts <- default_tooltip(show, trigger, zlevel, z, showContent, position, formatter, islandFormatter,
                          showDelay, hideDelay, transitionDuration, enterable, backgroundColor, borderColor,
                          borderRadius, borderWidth, padding, axisPointer, textStyle, ...)

  p$x$options$tooltip <- append(p$x$options$tooltip, opts)

  p

}

#' Add data zoom
#'
#' Add data zoom.
#'
#' @param p an echart object.
#' @param show whether to show the data zoom.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param orient orientation, takes \code{vertical} or \code{horinzontal}.
#' @param backgroundColor background color.
#' @param dataBackgroundColor background color of data zoom.
#' @param fillerColor fill color of selected area.
#' @param handleColor color of data zoom handle.
#' @param handleSize size of handle.
#' @param start,end percent start and end.
#' @param showDetail whether to show detail when dragging.
#' @param realtime set to \code{TRUE} if using real time data.
#' @param zoomLock when set to true, the selected area cannot be zoomed.
#' @param ... any other options to pass to data zoom.
#'
#' @examples
#' mtcars %>%
#'   echart(disp) %>%
#'   eline(mpg) %>%
#'   ezoom()
#'
#' @export
ezoom <- function(p, show = TRUE, zlevel = 0, z = 4, orient = "horizontal", backgroundColor = "rgba(0,0,0,0)",
                  dataBackgroundColor = "#eee", fillerColor = "rgba(144,197,237,0.2)", handleColor = "rgba(70,130,180,0.8)",
                  handleSize = 8, start = 0, end = 100, showDetail = TRUE, realtime = FALSE, zoomLock = FALSE, ...){

  opts <- list(...)
  opts$show <- show
  opts$zlevel <- zlevel
  opts$z <- z
  opts$orient <- orient
  opts$backgroundColor <- backgroundColor
  opts$dataBackgroundColor <- dataBackgroundColor
  opts$fillerColor <- fillerColor
  opts$handleColor <- handleColor
  opts$handleSize <- handleSize
  opts$start <- start
  opts$end <- end
  opts$showDetail <- showDetail
  opts$realtime <- realtime
  opts$zoomLock <- zoomLock

  p$x$options$dataZoom <- append(p$x$options$dataZoom, opts)

  p
}

#' Add title
#'
#' Add chart title and subtitles.
#'
#' @param p an echart object.
#' @param text title.
#' @param subtext subtitle.
#' @param link hyperlink.
#' @param sublink subtext hyperlink.
#' @param target \code{link} opening window: \code{self} or \code{blank}.
#' @param subtarget \code{sublink} opening window: \code{self} or \code{blank}.
#' @param x positon of title, \code{left} or \code{right}.
#' @param y postion of title, \code{top}, \code{bottom} or \code{center}.
#' @param backgroundColor background color.
#' @param borderColor border color.
#' @param borderWidth width of border.
#' @param padding padding.
#' @param itemGap gap between title and subtitle.
#' @param show whether to show the title.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param ... any other options to pass to title
#'
#' @examples
#' mtcars %>%
#'   echart(disp) %>%
#'   eline(mpg) %>%
#'   etitle("MPG vs DISP", "Made with EChart", link = "http://echarts.baidu.com", target = "blank")
#'
#' @export
etitle <- function(p, text, subtext, link, sublink, target = "blank", subtarget = "blank", x = "left", y = "top",
                   backgroundColor = "rgba(0,0,0,0)", borderColor = "#ccc", borderWidth = 0, padding = 5,
                   itemGap = 5, zlevel = 0, z = 6, show = TRUE, ...){

  if(missing(text)) stop("must pass text")

  opts <- list(...)
  opts$show <- show
  opts$text <- text
  opts$subtext <- if(!missing(subtext)) subtext
  opts$link <- if(!missing(link)) link
  opts$sublink <- if(!missing(sublink)) sublink
  opts$target <- target
  opts$subtarget <- subtarget
  opts$x <- x
  opts$y <- y
  opts$backgroundColor <- backgroundColor
  opts$borderColor <- borderColor
  opts$borderWidth <- borderWidth
  opts$padding <- padding
  opts$itemGap <- itemGap
  opts$zlevel <- zlevel
  opts$z <- z

  p$x$options$title <- append(p$x$options$title, opts)

  p
}

#' Customise colorbar
#'
#' Customise the colorbar of your chart.
#'
#' @param p an echart object.
#' @param min,max minimum and maximum.
#' @param which series to serie is to be affected, takes the name of a serie, \code{previous} or \code{all}.
#' @param show whether to show the color bar.
#' @param color colors.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param orient orientation of bar, \code{vertical} or \code{horizontal}.
#' @param x x position; \code{left} or \code{right}.
#' @param y y posotion; \code{top} or \code{bottom}.
#' @param backgroundColor background color.
#' @param borderColor border color.
#' @param borderWidth width of border.
#' @param padding padding.
#' @param itemGap gap between items on bar.
#' @param itemWidth width of the bar.
#' @param itemHeight height of the bar.
#' @param precision decimal precision.
#' @param splitNumber number of segments.
#' @param splitList see \href{http://echarts.baidu.com/echarts2/doc/option-en.html#dataRange.splitList}{official docs} for details.
#' @param range used to set initial range i.e.: \code{list(start = 10, end = 50)}.
#' @param selectedMode selection mode.
#' @param calculable whether values are calculable.
#' @param hoverLink hoverlink with map.
#' @param realtime set to \code{TRUE} if using real time stream.
#' @param ... any other argument to pass to color bar.
#'
#' @details
#' \code{ecolorbar} refers to \href{http://echarts.baidu.com/echarts2/doc/option-en.html#dataRange.hoverLink}{datarange} in docs.
#'
#' @examples
#' df <- data.frame(x = 1:20,
#'                  y = runif(20, 5, 10),
#'                  size = runif(20, 5, 15))
#'
#' df %>%
#'   echart(x) %>%
#'   escatter(y, size, symbolSize = 10, legendHoverLink = TRUE) %>%
#'   ecolorbar(color = list("red", "blue"), min = 5, max = 15, calculable = TRUE)
#'
#' @export
ecolorbar <- function(p, min = NULL, max = NULL, which = "previous", show = TRUE, color = list("#1e90ff", "#f0ffff"),
                       zlevel = 4, z = 0, orient = "vertical", x = "left", y = "bottom",
                       backgroundColor = "rgba(0,0,0,0)", borderColor = "#ccc", borderWidth = 0, padding = 5,
                       itemGap = 10, itemWidth = 20, itemHeight = 14, precision = 0, splitNumber = 5,
                       splitList = NULL, range = NULL, selectedMode = TRUE, calculable = FALSE, hoverLink = TRUE,
                       realtime = FALSE, ...){

  opts <- list(...)
  opts$show <- show
  opts$color <- color
  opts$zlevel <- zlevel
  opts$z <- z
  opts$orient <- orient
  opts$x <- x
  opts$y <- y
  opts$backgroundColor <- backgroundColor
  opts$borderColor <- borderColor
  opts$borderWidth <- borderWidth
  opts$padding <- padding
  opts$itemGap <- itemGap
  opts$itemWidth <- itemWidth
  opts$itemHeight <- itemHeight
  opts$precision <- precision
  opts$splitNumber <- splitNumber
  opts$splitList <- splitList
  opts$range <- range
  opts$selectedMode <- selectedMode
  opts$hoverLink <- hoverLink
  opts$realtime <- realtime

  p$x$options$dataRange <- append(p$x$options$dataRange, opts)

  p$x$options$dataRange$min <- if(!is.null(min)) min else p$x$options$dataRange$min
  p$x$options$dataRange$max <- if(!is.null(max)) max else p$x$options$dataRange$max
  p$x$options$dataRange$calculable <- calculable

  p
}

#' Add Zoom and roam controller
#'
#' Add zoom and roam controller to map.
#'
#' @param p an echart object.
#' @param show set to \code{TRUE} to show the controller
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param x x position; \code{left} or \code{right}.
#' @param y y posotion; \code{top} or \code{bottom}.
#' @param width,height dimensions of controller.
#' @param backgroundColor background color.
#' @param borderColor border color.
#' @param borderWidth width of border.
#' @param padding padding.
#' @param fillerColor filler color.
#' @param handleColor color of handle.
#' @param step moving step of the 4 direction roam in px.
#' @param mapTypeControl ou can specify every single mapType when multiple map in a chart at the same time, such as: \code{list({ china = FALSE, world = TRUE})}.
#' @param ... any other option to pass to controller.
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
#'   emap_points_("values") %>%
#'   emap_roam(mapTypeControl = list(world = TRUE))
#'
#' @export
emap_roam <- function(p, show = TRUE, zlevel = 0, z = 4, x = "left", y = "top", width = 80, height = 120,
                      backgroundColor = "rgba(0,0,0,0)", borderColor = "#ccc", borderWidth = 0, padding = 5,
                      fillerColor = "#fff", handleColor = "#6495ed", step = 15, mapTypeControl = NULL, ...){

  opts <- list(...)
  opts$show <- show
  opts$zlevel <- zlevel
  opts$z <- z
  opts$x <- x
  opts$y <- y
  opts$width <- width
  opts$height <- height
  opts$backgroundColor <- backgroundColor
  opts$borderColor <- borderColor
  opts$borderWidth <- borderWidth
  opts$padding <- padding
  opts$fillerColor <- fillerColor
  opts$handleColor <- handleColor
  opts$step <- step
  opts$mapTypeControl <- if(!is.null(mapTypeControl)) mapTypeControl

  p$x$options$roamController <- opts

  p

}
