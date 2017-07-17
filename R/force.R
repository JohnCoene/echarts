#' Add edges
#'
#' Add edges for \code{\link{eforce}}.
#'
#' @param p an echart object.
#' @param links edges data.frame.
#' @param source source column.
#' @param target target column.
#' @param weight edge weight.
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
#'   elinks(edges, source, target, weight = 1)
#'
#' @name elinks
#' @rdname elinks
#'
#' @seealso \code{\link{enodes}} \code{\link{eforce}}
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
#' @param p an echart object.
#' @param nodes nodes data.frame.
#' @param name name column.
#' @param label nodes label column.
#' @param value nodes value (size).
#' @param category nodes group column.
#' @param symbolSize nodes symbol size column.
#' @param depth depth of nodes.
#' @param ignore whether to ignore nodes.
#' @param symbol nodes symbol, see details for valid values.
#' @param fixX,fixY whether to fix x and y axis position.
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
#' @name nodes
#' @rdname enodes
#'
#' @seealso \code{\link{enodes}} \code{\link{eforce}}
#'
#' @export
enodes <- function(p, nodes, name, label, value, category, symbolSize, depth, ignore = FALSE, symbol = "circle",
                   fixX = FALSE, fixY = FALSE){

  name <- deparse(substitute(name))
  label <- if(!missing(label)) deparse(substitute(label)) else NULL
  value <- if(!missing(value)) deparse(substitute(value)) else NULL
  category <- if(!missing(category)) deparse(substitute(category)) else NULL
  symbolSize <- if(!missing(symbolSize)) deparse(substitute(symbolSize)) else NULL
  depth <- if(!missing(depth)) deparse(substitute(depth)) else NULL

  p %>%
    enodes_(nodes, name, label, value, category, symbolSize, depth, ignore, symbol, fixX, fixY)

}

#' Build force network
#'
#' @param p an echart objects.
#' @param name name of network.
#' @param large set to \code{TRUE} to optimise for large graphs.
#' @param center center of network.
#' @param roam set to \code{TRUE} to enable zoom and drag.
#' @param size size of layout.
#' @param ribbonType whether to use ribbons.
#' @param minRadius,maxRadius minimum and maximum radius of nodes.
#' @param linkSymbol can be set to \code{arrow}.
#' @param linkSymbolSize size of \code{symbol}.
#' @param scaling scaling factor.
#' @param gravity centripetal force coefficient.
#' @param draggable set to \code{TRUE} to allow dragging nodes.
#' @param useWorker specifies whether to put layout calculation into web worker when the browser supports web worker.
#' @param steps the number of iterations of each frame layout calculation.
#' @param z,zlevel first and second grade cascading control, the higher z the closer to the top.
#' @param ... any other options to pass to serie.
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
#' @name eforce
#' @rdname eforce
#'
#' @seealso \code{\link{enodes}} \code{\link{eforce}}
#'
#' @export
eforce <- function(p, name = NULL, large = FALSE, center = list("50%", "50%"), roam = FALSE, size = "100%", ribbonType = FALSE,
                   minRadius = 10, maxRadius = 20, linkSymbol = "none", linkSymbolSize = list(10, 15), scaling = 1,
                   gravity = 1, draggable = TRUE, useWorker = TRUE, steps = 1, z = 2, zlevel = 0, ...){

  p %>%
    eforce_(name, large, center, roam, size, ribbonType, minRadius, maxRadius, linkSymbol, linkSymbolSize, scaling,
           gravity, draggable, useWorker, steps, z, zlevel, ...)

}
