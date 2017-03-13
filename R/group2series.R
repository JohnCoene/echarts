# group2series
map_grps_ <- function(data){

  if(dplyr::is.grouped_df(data)){

    # deparse groups to get grp column
    g.col <- dplyr::groups(data)
    g.col <- unlist(lapply(g.col, deparse))

    data <- dplyr::ungroup(data)
    data <- na2ec(data)
    data <- as.data.frame(data)

    grps <- unique(data[,g.col]) # get unique grps

    # fun to filter grps
    filter_grp <- function(grps){
      data[data[, g.col] == grps,]
    }

    data <- Map(filter_grp, grps)
    names(data) <- grps

  } else {
    data <- na2ec(data)
    data <- list(data)
  }

  return(data)

}
