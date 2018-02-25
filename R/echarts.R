#' Initiate an echart
#'
#' Initiate an echart graph.
#'
#' @param data data.frame containing data to plot.
#' @param x variable column.
#' @param width,height dimensions of chart.
#' @param elementId id of div containing chart.
#'
#' @examples
#' mtcars %>%
#'   echart(mpg) %>%
#'   eline(drat)
#'
#' @import htmlwidgets
#'
#' @name echart
#' @rdname echart
#'
#' @importFrom methods is
#'
#' @export
echart <- function(data, x, width = NULL, height = NULL, elementId = NULL) {

  # x
  if(!missing(x)){
    xvar <- tryCatch(eval(substitute(x), data), error = function(e) e)
    if(is(xvar, "error")){
      xvar <- check_xvar(data, x)
      x.name <- NULL
    } else {
      x.name <- deparse(substitute(x))
      data <- sort_data(data, x.name)
      xvar <- check_xvar(data, xvar)
    }
  } else {
    xvar <- NULL
    x.name <- NULL
  }

  if(!missing(data)){
    data <- map_grps_(data)
    assign("data", data, envir = data_env)
  }

  # assign for future use
  assign("x", xvar, envir = data_env)
  assign("x.name", x.name, envir = data_env)

  # forward options using x
  x = list(
    theme = "default",
    options = list(
      xAxis = list(
        list(
          type = "category",
          data = xvar
        )
      ),
      yAxis = list(),
      series = list()
    )
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'echarts',
    x,
    width = width,
    height = height,
    sizingPolicy = htmlwidgets::sizingPolicy(knitr.defaultWidth = "100%",
                                             knitr.defaultHeight = 400,
                                             viewer.fill = TRUE,
                                             padding = 0),
    package = 'echarts',
    elementId = elementId
  )
}

#' @rdname echart
#'
#' @export
echart_ <- function(data, x, width = NULL, height = NULL, elementId = NULL) {

  # x
  if(!missing(x)){
    xvar <- tryCatch(unlist(unname(data[, x])), error = function(e) e)
    if(is(xvar, "error")){
      xvar <- check_xvar(data, x)
      x.name <- NULL
    } else {
      xvar <- check_xvar(data, xvar)
      x.name <- x
    }
  } else {
    xvar <- NULL
    x.name <- NULL
  }

  if(!missing(data)){

    data <- map_grps_(data)

    assign("data", data, envir = data_env)
  }

  # assign for future use
  assign("x", xvar, envir = data_env)
  assign("x.name", x.name, envir = data_env)

  # forward options using x
  x = list(
    theme = "default",
    options = list(
      xAxis = list(
        list(
          type = "category",
          data = unique(xvar)
        )
      ),
      yAxis = list(),
      series = list()
    )
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'echarts',
    x,
    width = width,
    height = height,
    sizingPolicy = htmlwidgets::sizingPolicy(defaultWidth = "100%",
                                             knitr.defaultWidth = "100%",
                                             knitr.defaultHeight = 400,
                                             viewer.fill = TRUE,
                                             padding = 0),
    package = 'echarts',
    elementId = elementId
  )
}

#' Shiny bindings for echarts
#'
#' Output and render functions for using echarts within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a echarts
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param id target chart id.
#' @param session shiny session
#'
#' @name echarts-shiny
#'
#' @export
echartsOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'echarts', width, height, package = 'echarts')
}

#' @rdname echarts-shiny
#' @export
renderEcharts <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, echartsOutput, env, quoted = TRUE)
}

#' @rdname echarts-shiny
#' @export
echartsProxy <- function(id, session = shiny::getDefaultReactiveDomain()){

  proxy <- list(id = id, session = session)
  class(proxy) <- "echartsProxy"

  return(proxy)
}
