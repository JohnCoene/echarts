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
#' echart() %>%
#'   eforce(itemStyle = list(normal = list(label = list(show = TRUE)))) %>% # show labels
#'   enodes(nodes, name, value = value, category = group) %>%
#'   elinks(edges, source, target, weight)
#'
#' @export
elinks <- function(p, links, source, target, weight = 1){

  source <- deparse(substitute(source))
  target <- deparse(substitute(target))

  weight <- if(class(weight)[1] == "integer" || class(weight)[1] == "numeric") weight else deparse(substitute(weight))

  p %>%
    elinks_(links, source, target, weight)
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

  name <- deparse(substitute(name))
  label <- if(!missing(label)) deparse(substitute(label)) else NULL
  value <- if(!missing(value)) deparse(substitute(value)) else NULL
  category <- if(!missing(category)) deparse(substitute(category)) else NULL
  symbolSize <- if(!missing(symbolSize)) deparse(substitute(symbolSize)) else NULL

  p %>%
    enodes_(nodes, name, label, value, category, symbolSize, ignore, symbol, fixX, fixY)

}

#' Build force network
#'
#' Plot force directed graph.
#'
#' @export
eforce <- function(p, name = NULL, large = FALSE, center = list("50%", "50%"), roam = FALSE, size = "100%", ribbonType = FALSE,
                   minRadius = 10, maxRadius = 20, linkSymbol = "none", linkSymbolSize = list(10, 15), scaling = 1,
                   gravity = 1, draggable = TRUE, useWorker = TRUE, steps = 1, z = 2, zlevel = 0, ...){

  p %>%
    eforce_(name, large, center, roam, size, ribbonType, minRadius, maxRadius, linkSymbol, linkSymbolSize, scaling,
           gravity, draggable, useWorker, steps, z, zlevel, ...)

}
