#' emap line effect
#'
#' Effect for emap lines
#'
#' @export
emap_line_effect <- function(show = TRUE, loop = TRUE, period = 30, scaleSize = 1, color = "#fff",
                             shadowBlur = 10, shadowColor = NULL, ...){

  lineStyle <- if(missing(itemStyle))

  opts <- list(...)
  opts$show <- show
  opts$loop <- loop
  opts$period <- period
  opts$scaleSize <- scaleSize
  opts$color <- color
  opts$shadowColor <- if(!is.null(shadowColor)) shadowColor

  return(opts)
}

#' mark points
#'
#' @export
emark_point <- function(p, which = "previous", data = list(), clickable = TRUE, symbol = "pin", symbolSize = 10,
                        symbolRotate = NULL, large = FALSE, effect, itemStyle, ...){

  opts <- list(...)
  opts$data <- data
  opts$clickable <- clickable
  opts$symbol = symbol
  opts$symbolSize <- symbolSize
  opts$symbolRotate <- symbolRotate
  opts$large <- large
  opts$effect <- if(!missing(effect)) effect
  opts$itemStyle <- if(!missing(itemStyle)) itemStyle

  p <- mark(p, which, opts, "markPoint")

  p

}

#' mark line
#'
#' @export
emark_line <- function(p, which = "previous", data = list(), clickable = TRUE, symbol = list("circle", "arrow"),
                       symbolSize = list(2, 4), symbolRotate = NULL, large = FALSE, smooth = FALSE, precision = 2,
                       bundling = list(enable = FALSE, maxTurningAngle = 45), z = 2, zlevel = 0, effect, itemStyle, ...){

  opts <- list(...)
  opts$data <- data
  opts$clickable <- clickable
  opts$symbol = symbol
  opts$symbolSize <- symbolSize
  opts$symbolRotate <- symbolRotate
  opts$large <- large
  opts$smooth <- smooth
  opts$precision <- precision
  opts$bundling <- bundling
  opts$effect <- if(!missing(effect)) effect
  opts$itemStyle <- if(!missing(itemStyle)) itemStyle

  p <- mark(p, which, opts, "markLine")

  p
}
