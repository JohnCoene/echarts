#' Add global options
#'
#' @export
eoptions <- function(p, backgroundColor = NULL, color, renderAsImage = FALSE, calculable = FALSE, symbolList,
                     calculableColor = "rgba(255,165,0,0.6)", calculableHolderColor = "#ccc", nameConnector = "&",
                     valueConnector = ":", ...){

  color <- if(missing(color)) list("#ff7f50", "#87cefa", "#da70d6", "#32cd32", "#6495ed", "#ff69b4",
                                   "#ba55d3", "#cd5c5c", "#ffa500", "#40e0d0", "#1e90ff", "#ff6347",
                                   "#7b68ee", "#00fa9a", "#ffd700", "#6699FF", "#ff6666", "#3cb371",
                                   "#b8860b", "#30e0e0")

  symbolList <- if(missing(symbolList)) list("circle", "rectangle", "triangle","diamond", "emptyCircle",
                                             "emptyRectangle", "emptyTriangle", "emptyDiamond")

  opts <- list(...)
  opts$backgroundColor <- backgroundColor
  opts$color <- color
  opts$renderAsImage <- renderAsImage
  opts$calculable <- calculable
  opts$symbolList <- symbolList
  opts$calculableColor = calculableColor
  opts$calculableHolderColor = calculableHolderColor
  opts$nameConnector <- nameConnector
  opts$valueConnector <- valueConnector

  p$x$options <- append(p$x$options, opts)

  p

}

#' Add animations
#'
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
#' @export
elegend <- function(p, legend, show = TRUE, zlevel = 0, z = 4, orient = "horizontal", x = "center",
                    y = "top", backgroundColor = "rgba(0,0,0,0)", borderColor = "#ccc", borderWidth = 0,
                    padding = 5, itemGap = 10, itemWidth = 20, itemHeight = 14, formatter = NULL,
                    selectedMode = TRUE, selected = NULL,
                    textStyle, ...){

  textStyle <- if(missing(textStyle)) list(fontFamily = "Arial, Verdana, sans-serif", fontSize = 12,
                                           fontStyle = "normal", fontWeight = "normal")
  legend <- if(missing(legend)) default_legend(p)

  if(!missing(legend)){
    if(length(legend) != length(p$x$options$series)) stop("n legend != n series", call. = FALSE)
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
#' @details
#' valid themes:
#'
#' \code{default}, \code{macarons}, \code{infographic}
#'
#' @export
etheme <- function(p, theme = "default"){

  p$x$theme <- theme

  p
}

#' Add tooltip
#'
#' @export
etooltip <- function(p, show = TRUE, trigger = "axis", zlevel = 1, z = 8, showContent = TRUE,
                     position = NULL, formatter = NULL, islandFormatter = "{a} < br/>{b} : {c}",
                     showDelay = 20, hideDelay = 100, transitionDuration = 4, enterable = FALSE,
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

#' Customise data range
#'
#' @export
edatarange <- function(p, scale, which = "previous", show = TRUE, color = list("#1e90ff", "#f0ffff"),
                       zlevel = 4, z = 0, orient = "vertical", x = "left", y = "bottom",
                       backgroundColor = "rgba(0,0,0,0)", borderColor = "#ccc", borderWidth = 0, padding = 5,
                       itemGap = 10, itemWidth = 20, itemHeight = 14, precision = 0, splitNumber = 5,
                       splitList = NULL, range = NULL, selectedMode = TRUE, calculable = FALSE, hoverLink = TRUE,
                       realtime = FALSE, text = list("High", "Low"), ...){

  if(!missing(scale)){
    data <- get("data", envir = data_env)
    scale <- eval(substitute(scale), data)
  } else {
    scale <- 1:100
  }

  min <- min(scale)
  max <- max(scale)

  opts <- list(...)
  opts$min <- min
  opts$max <- max
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
  opts$calculable <- calculable
  opts$hoverLink <- hoverLink
  opts$realtime <- realtime
  opts$text <- text

  p$x$options$dataRange <- opts

  p
}
