
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


build_coord <- function(long, lat){

  x <- get("x", envir = data_env)
  data <- get("data", envir = data_env)
  lon <- eval(substitute(lon, parent.frame()), data)
  lat <- eval(substitute(lat, parent.frame()), data)

  serie <- cbind(lon, lat)
  colnames(serie) <- NULL
  serie <- apply(serie, 1, as.list)
  names(serie) <- x

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
