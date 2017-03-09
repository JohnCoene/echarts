#' group2series
#'
group2wide_ <- function(p, serie){

  data <- get("data", envir = data_env)

  if(dplyr::is.grouped_df(data)){

    # deparse groups to get grp column
    g.col <- dplyr::groups(data)
    g.col <- unlist(lapply(g.col, deparse))

    grps <- unique(data[,g.col]) # get unique grps

    # fun to filter grps
    filter_grp <- function(grps){
      data[data[, g.col] == grps,]
    }

    data <- Map(filter_grp, grps)

  }

}
