#' echarts Shiny proxies
#'
#' Shiny proxies for echarts.
#'
#' @param proxy as returned by \code{\link{echartsProxy}}
#'
#' @examples
#' \dontrun{
#' library(shiny)
#'
#' ui <- fluidPage(
#'   titlePanel("echarts Proxies"),
#'   fluidRow(
#'     column(
#'       3,
#'       actionButton("getOptions", "get options")
#'     ),
#'     column(
#'       3,
#'       selectInput(
#'         "theme",
#'         "change theme",
#'         choices =
#'           c(
#'             "default",
#'             "red",
#'             "mint",
#'             "macarons",
#'             "macarons2",
#'             "green",
#'             "blue",
#'             "dark",
#'             "gray",
#'             "helianthus",
#'             "wef",
#'             "roma",
#'             "sakura",
#'             "shine",
#'             "infographic",
#'             "solarlight"
#'          )
#'       )
#'     ),
#'     column(
#'       3,
#'       actionButton("addData", "add data")
#'     ),
#'     column(
#'       3,
#'       numericInput(
#'         "markPoint",
#'         "Mark point",
#'         10
#'       )
#'     )
#'   ),
#'   fluidRow(
#'     echartsOutput("chart")
#'   )
#' )
#'
#' server <- function(input, output){
#'   df <- data.frame(x = 1:50, y = runif(50, 5, 10), z = runif(50, 7, 12), w = runif(50, 10, 13))
#'
#'   output$chart <- renderEcharts(
#'     df %>%
#'       echart(x) %>%
#'       earea(y, name = "Metric", stack = "grp1") %>%
#'       eline(w, name = "Variable", stack = "grp2") %>%
#'       eline(z, name = "Var", stack = "grp3") %>%
#'       etitle("echarts", "and its Shiny proxies")
#'   )
#'
#'   reactive_1 <- eventReactive(input$addData, {
#'     runif(1, 5, 13)
#'   })
#'
#'   reactive_2 <- eventReactive(input$addData, {
#'     runif(1, 5, 13)
#'   })
#'
#'   reactive_3 <- eventReactive(input$addData, {
#'     runif(1, 5, 13)
#'   })
#'
#'   observeEvent(input$getOptions, {
#'     echartsProxy("chart") %>%
#'       egetoptions_p()
#'   })
#'
#'   observeEvent(input$theme, {
#'     echartsProxy("chart") %>%
#'       etheme_p(input$theme)
#'   })
#'
#'   observeEvent(input$addData, {
#'     echartsProxy("chart") %>%
#'       edata_p(index = 0, reactive_1()) %>%
#'       edata_p(index = 1, reactive_2()) %>%
#'       edata_p(index = 2, reactive_3())
#'   })
#'
#'   observeEvent(input$markPoint, {
#'     data = list(
#'       name = "point",
#'       value = input$markPoint,
#'       x = 20,
#'       y = 10
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @rdname proxies
#' @export
eget_options_p <- function(proxy){

  data <- list(id = proxy$id)

  proxy$session$sendCustomMessage("egetoptions_p", data)

  return(proxy)
}

#' @rdname proxies
#' @export
edata_p <- function(proxy, index, data, head = FALSE, grow = FALSE){

  data <- list(id = proxy$id, data = list(list(index, data, head, grow)))

  proxy$session$sendCustomMessage("eadd_line_p", data)

  return(proxy)
}

#' @rdname proxies
#' @export
etheme_p <- function(proxy, theme){
  if(tolower(theme) == "grey") theme <- "gray"

  themes <- c("default", "mint", "macarons", "macarons2", "green", "blue", "dark", "blue", "dark", "gray", "helianthus",
              "red", "roma", "sakura", "shine", "infographic", "solarlight", "wef")

  if(!tolower(theme) %in% themes) stop("invalid theme", call. = FALSE)

  data <- list(id = proxy$id, theme = theme)

  proxy$session$sendCustomMessage("etheme_p", data)

  return(proxy)
}

#' @rdname proxies
#' @export
emark_point_p <- function(proxy, index, data = list(), clickable = TRUE, symbol = "pin", symbol.size = 10,
                          symbol.rotate = NULL, large = FALSE, effect = NULL){

  opts <- list()
  opts$clickable <- clickable
  opts$symbol = symbol
  opts$symbolSize <- symbol.size
  opts$symbolRotate <- symbol.rotate
  opts$large <- large
  opts$effect <- effect
  opts$data <- data

  data <- list(id = proxy$id, data = opts)

  proxy$session$sendCustomMessage("emark_point_p", data)

  return(proxy)
}
