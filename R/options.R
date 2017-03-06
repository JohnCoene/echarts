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

  textStyle <- if(missing(textStyle)) list(fontFamily = "Arial, Verdana, sans-serif", fontSize = 12,
                                           fontStyle = "normal", fontWeight = "normal")

  opts <- list(...)
  opts$show <- show
  opts$trigger <- trigger
  opts$zlevel <- zlevel
  opts$showContent <- showContent
  opts$position <- position
  opts$formatter <- formatter
  opts$islandFormatter <- islandFormatter
  opts$showDelay <- showDelay
  opts$hideDelay <- hideDelay
  opts$transitionDuration <- transitionDuration
  opts$enterable <- enterable
  opts$backgroundColor <- backgroundColor
  opts$borderColor <- borderColor
  opts$borderRadius <- borderRadius
  opts$borderWidth <- borderWidth
  opts$padding <- padding
  opts$axisPointer <- if(!missing(axisPointer)) axisPointer
  opts$textStyle <- if(!missing(textStyle)) textStyle

  p$x$options$tooltip <- append(p$x$options$tooltip, opts)

  p

}

#' Add toolbox
#'
#' @export
etoolbox <- function(p, show = TRUE, zlevel = 0, z = 6, orient = "horizontal", x = "right", y = "top",
                     backgroundColor = "rgba(0,0,0,0)", borderColor = "#ccc", borderWidth = 0, padding = 5,
                     itemGap = 10, itemSize = 16, color, disableColor = "#ddd", effectiveColor = "red",
                     showTitle = TRUE, textStyle, ...){

  textStyle <- if(missing(textStyle)) list(fontFamily = "Arial, Verdana, sans-serif", fontSize = 12,
                                           fontStyle = "normal", fontWeight = "normal")
  color <- if(missing(color)) list("#1e90ff", "#22bb22", "#4b0082", "#d2691e")

  opts <- list(...)
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
  opts$itemSize <- itemSize
  opts$color <- color
  opts$disableColor <- disableColor
  opts$effectiveColor <- effectiveColor
  opts$showTitle <- showTitle
  opts$textStyle <- textStyle

  p$x$options$toolbox <- append(p$x$options$toolbox, opts)

  p

}
