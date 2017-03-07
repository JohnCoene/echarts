#' Add edges
#'
#' Add edges for \code{\link{eforce}}.
#'
#' @examples
#' let <- LETTERS[1:20]
#'
#' edges <- data.frame(source = sample(let, 20), target = sample(let, 20),
#'   weight = runif(20, 5, 20))
#'
#' nodes <- data.frame(name = let, value = runif(20, 5, 25), group = rep(LETTERS[1:4], 5))
#'
#' echart() %>%
#'   eforce(itemStyle = list(normal = list(label = list(show = TRUE)))) %>% # show labels
#'   enodes(nodes, name, value = value, category = group) %>%
#'   elinks(edges, source, target)
#'
#' @export
elinks <- function(p, links, source, target, weight = 1){

  previous <- length(p$x$options$series)

  p$x$options$series[[previous]]$links = build_links(links, source, target, weight)

  p
}

#' Add nodes
#'
#' Add nodes for \code{\link{eforce}}.
#'
#' @examples
#' let <- LETTERS[1:20]
#'
#' edges <- data.frame(source = sample(let, 20), target = sample(let, 20),
#'   weight = runif(20, 5, 20))
#'
#' nodes <- data.frame(name = let, value = runif(20, 5, 25), group = rep(LETTERS[1:4], 5))
#'
#' echart() %>%
#'   eforce(itemStyle = list(normal = list(label = list(show = TRUE)))) %>% # show labels
#'   enodes(nodes, name, value = value, category = group) %>%
#'   elinks(edges, source, target)
#'
#' @export
enodes <- function(p, nodes, name, label, value, category, symbolSize, ignore = FALSE, symbol = "circle",
                   fixX = FALSE, fixY = FALSE){


  if(missing(name) || missing(nodes)) stop("must pass nodes and name column")

  name <- eval(substitute(name), nodes)
  ignore <- if(length(ignore) > 1) eval(substitute(ignore), nodes)
  symbol <- if(length(symbol) > 1) eval(substitute(symbol), nodes)
  fixX <- if(length(fixX) > 1) eval(substitute(fixX), nodes)
  fixY <- if(length(fixY) > 1) eval(substitute(fixY), nodes)

  vertices <- data.frame(row.names = 1:length(name))
  vertices$name <- name
  vertices$value <- if(!missing(value)) eval(substitute(value), nodes)
  vertices$symbolSize <- if(!missing(symbolSize)) eval(substitute(symbolSize), nodes)
  vertices$label <- if(!missing(label)) eval(substitute(label), nodes)
  vertices$category <- if(!missing(category)) eval(substitute(category), nodes)
  vertices$ignore <- ignore
  vertices$symbol <- symbol
  vertices$fixX <- fixX
  vertices$fixY <- fixY

  row.names(vertices) <- NULL
  vertices <- apply(vertices, 1, as.list)

  previous <- length(p$x$options$series)

  p$x$options$series[[previous]]$nodes = vertices

  p

}

#' Build force network
#'
#' Plot force directed graph.
#'
#' @export
eforce <- function(p, large = FALSE, center = list("50%", "50%"), roam = FALSE, size = "100%", ribbonType = FALSE,
                   minRadius = 10, maxRadius = 20, linkSymbol = "none", linkSymbolSize = list(10, 15), scaling = 1,
                   gravity = 1, draggable = TRUE, useWorker = TRUE, steps = 1, z = 2, zlevel = 0, ...){

  opts <- list(...)
  opts$type <- "force"
  opts$large <- large
  opts$center <- center
  opts$roam <- roam
  opts$size <- size
  opts$minRadius <- minRadius
  opts$maxRadius <- maxRadius
  opts$linkSymbol <- linkSymbol
  opts$linkSymbolSize <- linkSymbolSize
  opts$scaling <- scaling
  opts$gravity <- gravity
  opts$draggable <- draggable
  opts$useWorker <- useWorker
  opts$steps <- steps
  opts$z <- z
  opts$zlevel <- zlevel

  p$x$options$xAxis <- NULL
  p$x$options$yAxis <- NULL

  p$x$options$series <- append(p$x$options$series, list(opts))

  p

}
