# group2series
map_grps_ <- function(data){

  if(dplyr::is.grouped_df(data)){

    # deparse groups to get grp column
    g.col <- dplyr::groups(data)
    g.col <- unlist(lapply(g.col, deparse))

    data <- dplyr::ungroup(data)
    data <- apply(data, 2, na2ec)
    data <- as.data.frame(data)
    row.names(data) <- NULL

    grps <- unique(data[,g.col]) # get unique grps

    # fun to filter grps
    filter_grp <- function(grps){
      data[data[, g.col] == grps,]
    }

    data <- Map(filter_grp, grps)
    names(data) <- grps

  } else {
    data <- na2ec(data)
    row.names(data) <- NULL
    data <- list(as.data.frame(data))
  }

  return(data)

}
