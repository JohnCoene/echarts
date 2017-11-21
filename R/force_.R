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
#'   eforce_(itemStyle = list(normal = list(label = list(show = TRUE)))) %>% # show labels
#'   enodes_(nodes, "name", value = "value", category = "group") %>%
#'   elinks_(edges, "source", "target")
#'
#' @rdname elinks
#'
#' @export
elinks_ <- function(p, links, source, target, weight = 1){

  previous <- length(p$x$options$series)

  p$x$options$series[[previous]]$links = build_links_(links, source, target, weight)

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
#'   eforce_(itemStyle = list(normal = list(label = list(show = TRUE)))) %>% # show labels
#'   enodes_(nodes, "name", value = "value", category = "group") %>%
#'   elinks_(edges, "source", "target")
#'
#' @rdname enodes
#'
#' @export
enodes_ <- function(p, nodes, name, label = NULL, value = NULL, category = NULL, symbolSize = NULL, depth = NULL, ignore = FALSE,
                    symbol = "circle", fixX = FALSE, fixY = FALSE){


  if(missing(name) || missing(nodes)) stop("must pass nodes and name column")

  name <- as.character(nodes[, name])
  ignore <- if(length(ignore) > 1) nodes[, ignore]
  symbol <- if(length(symbol) > 1) nodes[, symbol]
  fixX <- if(length(fixX) > 1) nodes[, fixX]
  fixY <- if(length(fixY) > 1) nodes[, fixY]

  vertices <- data.frame(row.names = 1:length(name))
  vertices$name <- name
  vertices$value <- if(!is.null(value)) nodes[, value]
  vertices$symbolSize <- if(!is.null(symbolSize)) nodes[, symbolSize]
  vertices$label <- if(!is.null(label)) nodes[, label]
  vertices$category <- if(!is.null(category)) cat2num(nodes[, category])
  vertices$depth <- if(!is.null(depth)) nodes[, depth]
  vertices$ignore <- ignore
  vertices$symbol <- symbol
  vertices$fixX <- fixX
  vertices$fixY <- fixY

  row.names(vertices) <- NULL
  vertices <- apply(vertices, 1, as.list)

  previous <- length(p$x$options$series)


  p$x$options$series[[previous]]$categories <- if(!is.null(category)) node_cat(nodes[, category])
  p$x$options$series[[previous]]$nodes <- vertices

  # adapt legend
  if(!is.null(category)){
    p$x$options$legend$data <- force_legend(nodes[, category])
    p$x$options$legend$x <- "left"
    p$x$options$legend$orient <- "vertical"
  }

  p

}

#' Build force network
#'
#' Plot force directed graph.
#'
#' @rdname eforce
#'
#' @export
eforce_ <- function(p, name = NULL, large = FALSE, center = list("50%", "50%"), roam = FALSE, size = "100%",
                    minRadius = 10, maxRadius = 20, linkSymbol = "none", linkSymbolSize = list(10, 15), scaling = 1,
                    gravity = 1, draggable = TRUE, useWorker = TRUE, steps = 1, z = 2, zlevel = 0, ...){

  opts <- list(...)
  opts$name <- if(!is.null(name)) name
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
