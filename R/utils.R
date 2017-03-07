
vector_data <- function(serie){
  data <- get("data", envir = data_env) # get data for eval

  eval(substitute(serie, parent.frame()), data) # eval
}

scatter_data <- function(serie, size){

  # get for eval
  x <- get("x", envir = data_env)
  data <- get("data", envir = data_env)

  serie <- eval(substitute(serie, parent.frame()), data)

  # build matrix
  if(!missing(size)){
    size <- eval(substitute(size, parent.frame()), data)
    values <- cbind(x, serie, size)
  } else{
    values <- cbind(x, serie)
  }

  colnames(values) <- NULL # remove names

  values <- apply(values, 1, as.list)

  return(values)
}

val_name_data <- function(serie){

  # get for eval
  x <- get("x", envir = data_env)
  data <- get("data", envir = data_env)

  serie <- eval(substitute(serie, parent.frame()), data)

  data <- cbind.data.frame(x, serie)
  names(data) <- c("name", "value")

  data <- apply(data, 1, as.list)

  return(data)
}

polar_indicator <- function(){
  x <- get("x", envir = data_env)
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

  if(ncol(matrix) != nrow(matrix)) stop("must pass adjacency matrix", call. = FALSE)

  colnames(matrix) <- NULL # remove names

  matrix <- apply(matrix, 1, as.list)

  return(matrix)
}

default_dataRange <- function(serie){

  data <- get("data", envir = data_env)
  serie <- eval(substitute(serie, parent.frame()), data)

  calc <- class2calc(serie)

  dataRange <- list(
    min = min(serie),
    max = max(serie),
    text = list("High", "Low"),
    realtime = FALSE,
    calculable = calc,
    color = list('orangered','yellow','lightskyblue')
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


build_coord <- function(lon, lat){

  x <- tryCatch(get("x", envir = data_env), error = function(e) e)
  data <- get("data", envir = data_env)
  lon <- eval(substitute(lon, parent.frame()), data)
  lat <- eval(substitute(lat, parent.frame()), data)

  serie <- cbind(lon, lat)
  colnames(serie) <- NULL
  serie <- apply(serie, 1, as.list)

  if(!is(x, "error")) names(serie) <- x

  return(serie)

}

map_lines <- function(edges, source, target){

  # source
  source <- eval(substitute(source, parent.frame()), edges)
  target <- eval(substitute(target, parent.frame()), edges)

  # list of lists
  edges <- list()
  for(i in 1:length(source)){
    edges[[i]] <- list(list(name = source[i]), list(name = target[i]))
  }

  return(edges)
}


cloud_data <- function(freq, color){

  x <- get("x", envir = data_env) # get words

  # build data
  data <- get("data", envir = data_env)
  freq <- eval(substitute(freq, parent.frame()), data)

  df <- cbind.data.frame(as.character(x), freq)
  names(df) <- c("name", "value")

  df <- apply(df, 1, as.list)

  if(!missing(color)){
    color <- eval(substitute(color, parent.frame()), data)
    for(i in 1:length(color)){
      df[[i]]$itemStyle <- list(normal = list(color = color[i]))
    }
  }

  return(df)
}

heat_data <- function(y, z){

  x <- get("x", envir = data_env) # get words

  # build data
  data <- get("data", envir = data_env)

  # source
  y <- eval(substitute(y, parent.frame()), data)
  z <- eval(substitute(z, parent.frame()), data)

  df <- cbind(x, y, z)
  colnames(df) <- NULL # remove names

  df <- apply(df, 1, as.list)

  return(df)
}

heat_map_data <- function(lon, lat, z){

  # build data
  data <- get("data", envir = data_env)

  # source
  lon <- eval(substitute(lon, parent.frame()), data)
  lat <- eval(substitute(lat, parent.frame()), data)
  z <- eval(substitute(z, parent.frame()), data)

  df <- cbind(lon, lat, z)
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
                     showDelay = 20, hideDelay = 100, transitionDuration = 4, enterable = FALSE,
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

default_mark_point <- function(data = list(), clickable = TRUE, symbol = "pin", symbolSize = 10, symbolRate = NULL,
                               large = FALSE, effect, itemStyle, ...){

  opts <- list(...)
  opts$clickable <- clickable
  opts$symbol <- symbol
  opts$symbolSize <- symbolSize
  opts$symbolRate <- symbolRate
  opts$large <- large
  opts$effect <- if(!missing(effect)) effect
  opts$itemStyle <- if(!missing(itemStyle)) itemStyle
  opts$data <- data

  return(opts)

}

default_mark_line <- function(data = list(), clickable = TRUE, symbol = list("circle", "arrow"), symbolSize = list(2, 4),
                              symbolRate = NULL, large = FALSE, smooth = FALSE, smoothness = 0.2, precision = 2,
                              bundling, effect, itemStyle, ...){

  opts <- list(...)
  opts$data <- data
  opts$clickable <- clickable
  opts$symbol <- symbol
  opts$symbolSize <- symbolSize
  opts$symbolRate <- symbolRate
  opts$large <- large
  opts$smooth <- smooth
  opts$smoothness <- smoothness
  opts$precision <- precision
  opts$bundling <- if(!missing(bundling)) bundling
  opts$effect <- if(!missing(effect)) effect
  opts$itemStyle <- if(!missing(itemStyle)) itemStyle

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

build_links <- function(edges, source, target, weight = 1){

  source <- eval(substitute(source, parent.frame()), edges)
  target <- eval(substitute(target, parent.frame()), edges)

  links <- cbind.data.frame(source, target)
  links$weight <- weight

  links <- apply(links, 1, as.list)

  return(links)

}
