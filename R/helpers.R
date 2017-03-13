#' emap line effect
#'
#' Effect for emap lines
#'
#' @param show set to \code{TRUE} to show effect.
#' @param loop set to \code{TRUE} to loop animation.
#' @param period period loop.
#' @param scaleSize scale.
#' @param color color.
#' @param shadowBlur blur.
#' @param shadowColor color of shadow.
#' @param ... any other option to pass to effect.
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
#'   echart(city) %>%
#'   emap() %>%
#'   emap_coords(lon, lat) %>%
#'   emap_lines(edges, source, target, effect = emap_line_effect())
#'
#' @export
emap_line_effect <- function(show = TRUE, loop = TRUE, period = 30, scaleSize = 1, color = "#fff",
                             shadowBlur = 10, shadowColor = NULL, ...){

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
#' @param p an echart object.
#' @param which serie to mark lines of, takes \code{previous}, \code{all} or name of specific serie.
#' @param data data of mark points, points to mark.
#' @param clickable whether marked points are clickable.
#' @param symbol symbol, see details for valid values.
#' @param symbolSize size of symbol.
#' @param symbolRotate symbol rotation angle, i.e.:\code{30}.
#' @param large set to \code{TRUE} to optimise for large datasets.
#' @param ... any other options to pass to mark points.
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
#' df <- data.frame(x = 1:150,
#'                  y = round(rnorm(150, mean = 5, sd = 1), 2),
#'                  z = round(rnorm(150, mean = 7, sd = 1), 2))
#'
#' df %>%
#'   echart(x) %>%
#'   escatter(y) %>%
#'   escatter(z) %>%
#'   emark_point(which = "all", data = list(list(type = "min"), list(type = "max")))
#'
#' @export
emark_point <- function(p, which = "previous", data = list(), clickable = TRUE, symbol = "pin", symbolSize = 10,
                        symbolRotate = NULL, large = FALSE, ...){

  opts <- list(...)
  opts$data <- data
  opts$clickable <- clickable
  opts$symbol = symbol
  opts$symbolSize <- symbolSize
  opts$symbolRotate <- symbolRotate
  opts$large <- large

  p <- mark(p, which, opts, "markPoint")

  p

}

#' mark line
#'
#' @param p an echart object.
#' @param which serie to mark lines of, takes \code{previous}, \code{all} or name of specific serie.
#' @param data data of mark points, points to mark.
#' @param clickable whether marked points are clickable.
#' @param symbol symbol, see details for valid values.
#' @param symbolSize size of symbol.
#' @param symbolRotate symbol rotation angle, i.e.:\code{30}.
#' @param large set to \code{TRUE} to optimise for large datasets.
#' @param smooth whether to smooth line.
#' @param precision decimal precision.
#' @param bundling line bundling.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param ... any other options to pass to mark points.
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
#' df <- data.frame(x = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
#'                  radio = c(320, 332, 301, 334, 390, 330, 320),
#'                  social = c(120, 132, 101, 134, 90, 230, 210))
#'
#' df %>%
#'   echart(x) %>%
#'   ebar(radio, stack = "grp") %>%
#'   ebar(social, stack = "grp") %>%
#'   emark_line(data = list(list(type = "max")), clickable = FALSE) %>%
#'   etooltip(axisPointer = list(type = "shadow")) %>%
#'   etoolbox_full() %>%
#'   etoolbox_magic(type = list("tiled", "stack", "line", "bar"))
#'
#' @export
emark_line <- function(p, which = "previous", data = list(), clickable = TRUE, symbol = list("circle", "arrow"),
                       symbolSize = list(2, 4), symbolRotate = NULL, large = FALSE, smooth = FALSE, precision = 2,
                       bundling = list(enable = FALSE, maxTurningAngle = 45), z = 2, zlevel = 0, ...){

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

  p <- mark(p, which, opts, "markLine")

  p
}
