#' Add timeline
#'
#' @examples
#' tl <- data.frame(x = 1:100,
#'   values = runif(100, 1, 5),
#'   time.stamp = rep(c("day1", "day2", "day3", "day4"), 25))
#'
#' tl %>%
#'   echart_("x") %>%
#'   eline_("values") %>%
#'   etimeline_()
#'
#' @export
etimeline_ <- function(p, timeline, show = TRUE, zlevel = 0, z = 4, type = "time", notMerge = FALSE, realtime = TRUE,
                       x = 80., y = NULL, x2 = 80, y2 = 0, height = 50, backgroundColor = "rgba(0,0,0,0)",
                       borderWidth = 0, borderColor = "#ccc", padding = 5, controlPosition = "left",
                       autoPlay = FALSE, loop = TRUE, playInterval = 2000, symbol = "emptyDiamond",
                       symbolSize = 4, currentIndex = 0, ...){

  if(missing(timeline)) stop("must pass timeline column")

  opts <- list(...)
  opts$show <- show
  opts$zlevel <- zlevel
  opts$data <- timeline
  opts$z <- z
  opts$type <- type
  opts$notMerge <- notMerge
  opts$realtime <- realtime
  opts$x <- x
  opts$y <- y
  opts$x2 <- x2
  opts$y2 <- y2
  opts$height <- height
  opts$backgroundColor <- backgroundColor
  opts$borderWidth <- borderWidth
  opts$borderColor <- borderColor
  opts$padding <- padding
  opts$controlPosition <- controlPosition
  opts$autoPlay <- autoPlay
  opts$loop <- loop
  opts$playInterval <- playInterval
  opts$symbol <- symbol
  opts$symbolSize <- symbolSize
  opts$currentIndex <- currentIndex

  p$x$options$series

  p$x$timeline <- opts

  p

}
