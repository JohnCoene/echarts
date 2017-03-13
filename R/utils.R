na2ec <- function(data){
  data[is.na(data)] <- "-"
  data
}

get_dat <- function(serie){
  data <- get("data", envir = data_env)
  if(length(data) == 1) names(data) <- serie
  data
}

vector_data_ <- function(data, serie){
  data[, serie]
}

scatter_data_ <- function(data, serie, size = NULL, symbolSize){

  # get for eval
  x <- get("x", envir = data_env)

  serie <- data[, serie]

  # build matrix
  if(!is.null(size)){
    size <- data[, size]
    size <- normalise_size(size, symbolSize)
    values <- suppressWarnings(cbind(x, serie, size))
  } else {
    values <- suppressWarnings(cbind(x, serie))
  }

  colnames(values) <- NULL # remove names

  values <- apply(values, 1, as.list)

  return(values)
}

val_name_data_ <- function(data, serie){

  # get for eval
  x <- get("x", envir = data_env)

  serie <- data[, serie]

  data <- cbind.data.frame(x, serie)
  names(data) <- c("name", "value")

  data <- apply(data, 1, as.list)

  return(data)
}

polar_indicator <- function(){
  x <- get("x", envir = data_env)
  x <- unique(x)
  x <- data.frame(text = x)
  x <- apply(x, 1, as.list)
  return(x)
}

chord_data <- function(){
  x <- get("x", envir = data_env)
  x <- data.frame(name = x)
  x <- apply(x, 1, as.list)
  return(x)
}

chord_matrix <- function(){

  matrix <- get("data", envir = data_env)
  matrix <- matrix[[1]]

  x.len <- length(get("x", envir = data_env))

  if(ncol(matrix) != nrow(matrix)) stop("uneven columns and rows", call. = FALSE)
  if(x.len != ncol(matrix)) stop("length of x != matrix dimensions", call. = FALSE)

  colnames(matrix) <- NULL # remove names

  matrix <- apply(matrix, 2, as.numeric) # to numeric!
  matrix <- apply(matrix, 1, as.list)

  return(matrix)
}

default_dataRange_ <- function(data, serie){
  serie <- data[, serie]

  calc <- class2calc(serie)

  dataRange <- list(
    min = min(serie),
    max = max(serie),
    calculable = calc
  )

  return(dataRange)

}

class2calc <- function(x){

  if(class(x)[1] == "integer" || class(x)[1] == "numeric"){
    TRUE
  } else {
    FALSE
  }
}

build_coord_ <- function(data, lon, lat){

  x <- get("x.name", envir = data_env)
  x <- data[, x]

  # build serie for EC
  serie <- data[, c(lon, lat)]
  rownames(serie) <- NULL
  colnames(serie) <- NULL

  # to list
  serie <- apply(serie, 1, as.list)

  if(!is(x, "error")) names(serie) <- x

  return(serie)

}

map_lines_ <- function(data, source, target){

  # source & target
  source <- data[, source]
  target <- data[, target]

  # list of lists
  edges <- list()
  for(i in 1:length(source)){
    edges[[i]] <- list(list(name = source[i]), list(name = target[i]))
  }

  return(edges)
}

cloud_data_ <- function(data, freq, color){

  x <- get("x", envir = data_env) # get words

  # build data
  freq <- data[, freq]

  df <- cbind.data.frame(as.character(x), freq)
  names(df) <- c("name", "value")

  df <- apply(df, 1, as.list)

  if(!is.null(color)){
    color <- data[, color]
    for(i in 1:length(color)){
      df[[i]]$itemStyle <- list(normal = list(color = color[i]))
    }
  }

  return(df)
}

heat_data_ <- function(data, y, z){

  x <- get("x", envir = data_env) # get words

  # source
  y <- data[, y]
  z <- data[, z]

  df <- cbind(x, y, z)
  colnames(df) <- NULL # remove names

  df <- apply(df, 1, as.list)

  return(df)
}

heat_map_data_ <- function(lon, lat, z){

  # build data
  data <- get("data", envir = data_env)
  data <- data[[1]]

  # source
  df <- data[, c(lon, lat, z)]
  colnames(df) <- NULL # remove names

  df <- apply(df, 1, as.list)

  return(df)
}

default_legend <- function(p){
  series <- p$x$options$series

  name <- list()
  for(i in 1:length(series)){
    name[[i]] <- series[[i]]$name
  }

  return(name)
}

default_tooltip <- function(show = TRUE, trigger = "axis", zlevel = 1, z = 8, showContent = TRUE,
                     position = NULL, formatter = NULL, islandFormatter = "{a} < br/>{b} : {c}",
                     showDelay = 5, hideDelay = 100, transitionDuration = 4, enterable = FALSE,
                     backgroundColor = "rgba(0,0,0,0.7)", borderColor = "#333", borderRadius = 4,
                     borderWidth = 0, padding = 5, axisPointer, textStyle, ...){

  textStyle <- if(missing(textStyle)) list(fontFamily = "Arial, Verdana, sans-serif", fontSize = 12,
                                           fontStyle = "normal", fontWeight = "normal")

  opts <- list(...)
  opts$show <- show
  opts$trigger <- trigger
  opts$zlevel <- zlevel
  opts$showContent <- showContent
  opts$position <- position
  opts$formatter <- formatter
  opts$islandFormatter <- islandFormatter
  opts$showDelay <- showDelay
  opts$hideDelay <- hideDelay
  opts$transitionDuration <- transitionDuration
  opts$enterable <- enterable
  opts$backgroundColor <- backgroundColor
  opts$borderColor <- borderColor
  opts$borderRadius <- borderRadius
  opts$borderWidth <- borderWidth
  opts$padding <- padding
  opts$axisPointer <- if(!missing(axisPointer)) axisPointer
  opts$textStyle <- if(!missing(textStyle)) textStyle

  return(opts)

}

default_gradient <- function(){
  list("blue", "cyan", "lime", "yellow", "red")
}

build_nodes <- function(nodes, name, label = NULL, value = NULL, category = NULL, symbolSize = NULL,
                        ignore = FALSE, symbol = "circle", fixX = FALSE, fixY = FALSE){

  name <- eval(substitute(name, parent.frame()), nodes)
  ignore <- if(length(ignore) > 1) eval(substitute(ignore, parent.frame()), nodes)
  symbol <- if(length(symbol) > 1) eval(substitute(symbol, parent.frame()), nodes)
  fixX <- if(length(fixX) > 1) eval(substitute(fixX, parent.frame()), nodes)
  fixY <- if(length(fixY) > 1) eval(substitute(fixY, parent.frame()), nodes)

  vertices <- data.frame(row.names = 1:length(name))
  vertices$name <- name
  vertices$value <- if(!is.null(value)) eval(substitute(value, parent.frame()), nodes)
  vertices$symbolSize <- if(!is.null(symbolSize)) eval(substitute(symbolSize, parent.frame()), nodes)
  vertices$label <- if(!is.null(label)) eval(substitute(label, parent.frame()), nodes)
  vertices$category <- if(!is.null(category)) eval(substitute(category, parent.frame()), nodes)
  vertices$ignore <- ignore
  vertices$symbol <- symbol
  vertices$fixX <- fixX
  vertices$fixY <- fixY

  row.names(vertices) <- NULL
  vertices <- apply(vertices, 1, as.list)

  return(vertices)

}

build_links_ <- function(edges, source, target, weight = 1){

  source <- edges[, source]
  target <- edges[, target]
  if(class(weight)[1] == "character") edges[, weight]

  links <- cbind.data.frame(source, target)
  links$weight <- weight

  links <- apply(links, 1, as.list)

  return(links)

}

mark <- function(p, which, opts, type = "markPoint"){

  if(which == "previous"){

    previous <- length(p$x$options$series)
    p$x$options$series[[previous]][[type]] <- opts

  } else if(tolower(which) == "all"){

    for(i in 1:length(p$x$options$series)){
      p$x$options$series[[i]][[type]] <- opts
    }

  } else {

    # get all series names
    post <- get_series_name(p, which)

    p$x$options$series[[pos]][[type]] <- opts

  }

  p
}

axis_category <- function(show = TRUE, zlevel = 0, z = 0, boundaryGap = FALSE, ...){

  opts <- list(...)
  opts$type <- "category"
  opts$show <- show
  opts$zlevel <- zlevel
  opts$z <- z
  opts$boundaryGap <- boundaryGap

  return(opts)
}

axis_value <- function(show = TRUE, zlevel = 0, z = 0, position = "left", name = NULL,
                       nameLocation = "end", nameTextStyle = list(), boundaryGap = list(0, 0),
                       min = NULL, max = NULL, scale = FALSE, splitNumber = NULL, ...){

  opts <- list(...)
  opts$type <- "value"
  opts$show <- show
  opts$zlevel <- zlevel
  opts$z <- z
  opts$position <- position
  opts$name <- name
  opts$nameLocation <- nameLocation
  opts$nameTextStyle <- nameTextStyle
  opts$boundaryGap <- boundaryGap
  opts$min <- min
  opts$max <- max
  opts$scale <- scale
  opts$splitNumber <- splitNumber

  return(opts)
}

axis_time <- function(show = TRUE, zlevel = 0, z = 0, position = "bottom", name = NULL,
                      nameLocation = "end", nameTextStyle = list(), boundaryGap = list(0, 0),
                      min = NULL, max = NULL, scale = FALSE, splitNumber = NULL, ...){

  opts <- list(...)
  opts$type <- "time"
  opts$show <- show
  opts$zlevel <- zlevel
  opts$z <- z
  opts$position <- position
  opts$name <- name
  opts$nameLocation <- nameLocation
  opts$nameTextStyle <- nameTextStyle
  opts$boundaryGap <- boundaryGap
  opts$min <- min
  opts$max <- max
  opts$scale <- scale
  opts$splitNumber <- splitNumber

  return(opts)
}

axis_log <- function(show = TRUE, zlevel = 0, z = 0, position = "left", logLabelBase = NULL,
                     logPositive = NULL, ...){

  opts <- list(...)
  opts$type <- "log"
  opts$show <- show
  opts$zlevel <- zlevel
  opts$z <- z
  opts$position <- position
  opts$name <- logLabelBase
  opts$nameLocation <- logLabelBase

  return(opts)
}

get_axis_type <- function(x){

  cl <- class(x)[1]

  if(cl == "character" || cl == "factor" || cl == "date"){
    return("category")
  } else if(cl == "POSIXct" || cl == "POSIXlt"){
    return("category")
  } else {
    return("category")
  }

}

add_axis <- function(p, opts, append = FALSE, axis){

  if(append == FALSE){
    p$x$options[[axis]] <- list(opts)
  } else {
    index <- length(p$x$options[[axis]]) + 1
    p$x$options[[axis]][[index]] <- opts
  }

  p
}

add_toolbox_elem <- function(p, opts, elem){

  tb <- p$x$options$toolbox

  if(!length(tb)){
    p <- p %>%
      etoolbox()
  }

  p$x$options$toolbox$feature[[elem]] <- opts

  p

}

# return name index
get_series_name <- function(p, which){

  # get all series names
  n <- lapply(1:length(p$x$options$series), function(x){
    p$x$options$series[[x]]$name
  })

  position <- grep(paste0("^", which, "$"), n) # get which position

  if(!length(position)) stop(paste("cannot find serie named:", which), call. = FALSE)

  return(position)
}

get_min_ <- function(serie){
  min(serie)
}

get_max_ <- function(serie){
  max(serie)
}

# set calculable according to class
is_calculable_ <- function(x){
  cl <- class(x)[1]
  if(cl == "integer" || cl == "numeric"){
    TRUE
  } else {
    FALSE
  }
}

get_pie_legend <- function(){
  x <- get("x", envir = data_env)
  return(as.character(x))
}

get_map_index_ <- function(p, series_name){
  all_names <- mapply(function(x){ x[["name"]]}, p$x$options$series)
  index <- match(series_name, all_names)

  if(!length(index)) index <- length(p$x$options$series)
  return(index)
}

val_name_data_map_ <- function(data, serie){

  # get for eval
  x <- get("x.name", envir = data_env)

  data <- data[, c(x, serie)]
  names(data) <- c("name", "value")
  rownames(serie) <- NULL

  data <- apply(data, 1, as.list)
  names(data) <- NULL # remove persistent rownames

  return(data)
}

clean_data_map <- function(data){
  x.name <- get("x.name", envir = data_env)

  if(!is.null(x.name)){
    # clean FUN
    clean <- function(x){
      x[x[, x.name] != "",]
    }

    data <- Map(clean, data) # clean
  }

  # remove now-empty data.frame
  data <- data[lapply(data, nrow) > 0]

  return(data)
}

check_xvar <- function(data, x){

  if(dplyr::is.grouped_df(data)){
    x <- unique(x)
  }

  return(x)
}

node_cat <- function(categories){

  categories <- unique(categories)
  cat <- data.frame(name = categories)
  cat <- apply(cat, 1, as.list)

  return(cat)
}

cat2num <- function(x){
  x <- as.numeric(as.factor(x)) # to numeric
  x <- x - 1 # javascript counts from 0
  return(x)
}

force_legend <- function(categories){
  categories <- unique(categories)
  return(categories)
}

scatter_size <- function(size){
  htmlwidgets::JS("function(value){return value[2];}")
}

normalise_size <- function(size, symbolSize){
  size <- (x - min(x)) / (max(x) - min(x))
  size <- size * symbolSize
  return(size)
}

compute_max <- function(data, serie){

  data <- do.call("rbind.data.frame", lapply(data, as.data.frame))
  x <- data[, serie]

  return(max(x))

}
