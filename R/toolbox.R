#' Setup toolbox
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

  p$x$options$toolbox <- opts

  p

}

#' Add toolbox feature
#'
#' @export
etoolbox_feature <- function(p, mark, dataZoom, dataView, magicType, restore, saveAsImage){

  opts <- list()
  opts$mark <- if(!missing(mark)) mark
  opts$dataZoom <- if(!missing(dataZoom)) dataZoom
  opts$dataView <- if(!missing(magicType)) magicType
  opts$restore <- if(!missing(restore)) restore
  opts$saveAsImage <- if(!missing(saveAsImage)) sacveAsImage

  p$x$options$toolbox$feature <- append(p$x$options$toolbox$feature, opts)

  p
}

#' Add toolbox feature mark button
#'
#' @export
etoolbox_mark <- function(p, show = TRUE, title = list(mark = "Mark", markUndo = "Undo", markClear = "Clear"),
                          lineStyle = list(color = "#1e90ff", typed = "dashed", width = 2, shadowColor = "rgba(0,0,0,0)",
                                           shadowBlur = 5, shadowOffsetX = 3, shadowOffsetY = 3)){

  opts <- list()
  opts$show <- show
  opts$title <- title
  opts$lineStyle <- lineStyle

  p <- add_toolbox_elem(p, opts, "mark")

  p

}

#' Add toolbox zoom button
#'
#' @export
etoolbox_zoom <- function(p, show = TRUE, title = list(dataZoom = "Area Zoom", dataZoomReset = "Reset")){

  opts <- list()
  opts$show <- show
  opts$title <- title

  p <- add_toolbox_elem(p, opts, "dataZoom")

  p

}

#' Add toolbox view
#'
#' @export
etoolbox_view <- function(p, show = TRUE, title = "View", readOnly = FALSE, lang = list('Data View', 'close', 'refresh'), ...){

  opts <- list(...)
  opts$show <- show
  opts$title <- title
  opts$readOnly <- readOnly
  opts$lang <- lang

  p <- add_toolbox_elem(p, opts, "dataView")

  p

}

#' Add toolbox magic buttons
#'
#' @export
etoolbox_magic <- function(p, show = TRUE, type = list(), title, ...){

  title <- if(missing(title)) list(line = "line",
                                   bar = "bar",
                                   stack = "stack",
                                   tiled = "tiled",
                                   force = "force",
                                   chord = "chord",
                                   pie = "pie",
                                   funnel = "funnel")

  opts <- list(...)
  opts$show <- show
  opts$title <- title
  opts$type <- type

  p <- add_toolbox_elem(p, opts, "magicType")

  p

}

#' Add toolbox restore button
#'
#' @export
etoolbox_restore <- function(p, show = TRUE, title = "Reset"){

  opts <- list()
  opts$show <- show
  opts$title <- title

  p <- add_toolbox_elem(p, opts, "restore")

  p

}

#' Add toolbox save as image button
#'
#' @export
etoolbox_save <- function(p, show = TRUE, title = "Save as image", type = "png", name = "echarts", lang = "Save"){

  opts <- list()
  opts$show <- show
  opts$title <- title

  p <- add_toolbox_elem(p, opts, "saveAsImage")

  p

}

#' Add all elements of toolbox
#'
#' Adds toolbok mark, restor, save, and view.
#'
#' @export
etoolbox_full <- function(p, ...){

  p <- p %>%
    etoolbox(...) %>%
    etoolbox_mark() %>%
    etoolbox_restore() %>%
    etoolbox_save() %>%
    etoolbox_zoom() %>%
    etoolbox_view()

  p
}
