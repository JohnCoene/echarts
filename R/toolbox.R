#' Setup toolbox
#'
#' Setup toolbox
#'
#' @param p an echart object.
#' @param show whether to show the toolbox.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param orient toolbox orientation, \code{horizontal} or \code{vertical}.
#' @param x horizontal alignment, \code{left}, \code{right}.
#' @param y vertical alignment, \code{top}, \code{center}, \code{bottom}.
#' @param backgroundColor background color.
#' @param borderColor border color.
#' @param borderWidth border width.
#' @param padding padding.
#' @param itemGap space between toolbox buttons.
#' @param itemSize size of buttons.
#' @param color color of buttons.
#' @param disableColor color of disabled item.
#' @param effectiveColor color of active button.
#' @param showTitle set to \code{TRUE} to show text.
#' @param textStyle style of text.
#' @param ... any other options.
#'
#' @examples
#' mtcars %>%
#'   echart(qsec) %>%
#'   ebar(mpg) %>%
#'   etoolbox() %>%
#'   etoolbox_magic(type = list("line", "bar"))
#'
#' @export
etoolbox <- function(p, show = TRUE, zlevel = 0, z = 6, orient = "horizontal", x = "right", y = "top",
                     backgroundColor = "rgba(0,0,0,0)", borderColor = "#ccc", borderWidth = 0, padding = 5,
                     itemGap = 10, itemSize = 16, color = NULL, disableColor = "#ddd", effectiveColor = "red",
                     showTitle = TRUE, textStyle = NULL, ...){

  if(is.null(textStyle)) textStyle <- list(fontFamily = "Arial, Verdana, sans-serif", fontSize = 12,
                                           fontStyle = "normal", fontWeight = "normal")

  color <- if(is.null(color)) list("#1e90ff", "#22bb22", "#4b0082", "#d2691e") else color

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
#' Add toolbox feature.
#'
#' @param p an echart object.
#' @param mark markLine icons see \code{\link{etoolbox_mark}}.
#' @param dataZoom dataZoom icons \code{\link{etoolbox_zoom}}.
#' @param dataView dataView icons \code{\link{etoolbox_view}}.
#' @param magicType magicType icons \code{\link{etoolbox_magic}}.
#' @param restore restore icon \code{\link{etoolbox_restore}}.
#' @param saveAsImage saveAsImage icon \code{\link{etoolbox_save}}.
#'
#' @examples
#' mtcars %>%
#'   echart(qsec) %>%
#'   ebar(mpg) %>%
#'   etoolbox() %>%
#'   etoolbox_magic(type = list("line", "bar")) %>%
#'   etoolbox_feature(restore = list(show = TRUE))
#'
#' @export
etoolbox_feature <- function(p, mark, dataZoom, dataView, magicType, restore, saveAsImage){

  opts <- list()
  opts$mark <- if(!missing(mark)) mark
  opts$dataZoom <- if(!missing(dataZoom)) dataZoom
  opts$dataView <- if(!missing(magicType)) magicType
  opts$restore <- if(!missing(restore)) restore
  opts$saveAsImage <- if(!missing(saveAsImage)) saveAsImage

  p$x$options$toolbox$feature <- append(p$x$options$toolbox$feature, opts)

  p
}

#' Add toolbox feature mark button
#'
#' Enable marking chart.
#'
#' @param p an echart object.
#' @param show whether to show mark.
#' @param title mark button title.
#' @param lineStyle style of marked line.
#'
#' @examples
#' mtcars %>%
#'   echart(qsec) %>%
#'   ebar(mpg) %>%
#'   etoolbox() %>%
#'   etoolbox_mark()
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
#' Add zoom feature.
#'
#' @param p an echart object.
#' @param show whether to show zoom.
#' @param title button title.
#'
#' @examples
#' mtcars %>%
#'   echart(qsec) %>%
#'   ebar(mpg) %>%
#'   etoolbox() %>%
#'   etoolbox_zoom()
#'
#' @export
etoolbox_zoom <- function(p, show = TRUE, title = list(dataZoom = "Area Zoom", dataZoomReset = "Reset")){

  opts <- list()
  opts$show <- show
  opts$title <- title

  p <- add_toolbox_elem(p, opts, "dataZoom")

  p

}

#' Add toolbox data view
#'
#' Enables viewing data table.
#'
#' @param p an echart object.
#' @param show whether to show data view.
#' @param title button title.
#' @param readOnly set as read-only.
#' @param lang default text.
#' @param ... any other parameters to pass to data view.
#'
#' @examples
#' mtcars %>%
#'   echart(qsec) %>%
#'   ebar(mpg) %>%
#'   etoolbox() %>%
#'   etoolbox_view()
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
#' Enable changing chart type.
#'
#' @param p an echart object.
#' @param show wehtehr to show magic buttons.
#' @param type chart types.
#' @param title titles of charts.
#' @param ... any other options to pass to magic feature.
#'
#' @examples
#' mtcars %>%
#'   echart(disp) %>%
#'   ebar(mpg, stack = "grp") %>% # stack
#'   ebar(qsec, stack = "grp") %>% # stack
#'   ebar(wt) %>% # not stacked
#'   etooltip() %>%
#'   elegend() %>%
#'   etoolbox() %>%
#'   etoolbox_magic(type = list("bar", "line", "stack", "tiled"))
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
#' Add toolbox restore button.
#'
#' @param p an echart object.
#' @param show whether to show button.
#' @param title title of button.
#'
#' @examples
#' mtcars %>%
#'   echart(disp) %>%
#'   ebar(mpg, stack = "grp") %>% # stack
#'   ebar(qsec, stack = "grp") %>% # stack
#'   ebar(wt) %>% # not stacked
#'   etoolbox_restore()
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
#' Add save as image button.
#'
#' @param p an echart object.
#' @param show whether to show the button.
#' @param title title of button.
#' @param type image type
#' @param name of file.
#' @param lang text.
#'
#' @examples
#' mtcars %>%
#'   echart(disp) %>%
#'   ebar(mpg, stack = "grp") %>% # stack
#'   ebar(qsec, stack = "grp") %>% # stack
#'   etoolbox() %>%
#'   etoolbox_save()
#'
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
#' @param p an echart object.
#' @param ... any other option to pass to \code{\link{etoolbox}}.
#'
#' @details Adds mark, restore, save, view and zoom buttons
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
