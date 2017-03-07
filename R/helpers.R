#' emap line effect
#'
#' Effect for emap lines
#'
#' @export
emap_line_effect <- function(show = TRUE, loop = TRUE, period = 30, scaleSize = 1, color = "#fff",
                             shadowBlur = 10, shadowColor = NULL){
  opts <- list(
    show = show,
    loop = loop,
    period = period,
    scaleSize = scaleSize,
    color = color,
    shadowBlur = shadowBlur
  )
  opts$shadowColor = if(!is.null(shadowColor)) shadowColor

  return(opts)
}

#' emap line effect
#'
#' Effect for emap lines
#'
#' @export
emap_point_effect <- function(show = TRUE, loop = TRUE, period = 30, scaleSize = 1, color = "#fff",
                              shadowBlur = 10, shadowColor = NULL){
  opts <- list(
    show = show,
    loop = loop,
    period = period,
    scaleSize = scaleSize,
    color = color,
    shadowBlur = shadowBlur
  )
  opts$shadowColor = if(!is.null(shadowColor)) shadowColor

  return(opts)
}
