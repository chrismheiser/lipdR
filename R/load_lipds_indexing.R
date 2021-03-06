###############################################
## Load LiPDs - Indexing
## Misc functions that aid in converting LiPD
## data into a preferred R analysis structure
###############################################


#' Change index-by-number to index-by-variableName
#' @export
#' @keywords internal
#' @param d LiPD file
#' @return d Modified LiPD file
indexByNameLoad <- function(d){
  paleo <- c("paleoData", "paleoMeasurementTable", "paleoModel")
  chron <- c("chronData", "chronMeasurementTable", "chronModel")
  d <- indexSectionLoad(d, paleo)
  d <- indexSectionLoad(d, chron)
  d <- indexGeo(d)
  return(d)
}

#' Change index-by-number for one section
#' @export
#' @keywords internal
#' @param d LiPD metadata
#' @param keys Section keys
#' @return d Modified LiPD metadata
indexSectionLoad <- function(d, keys){
  key1 <- keys[[1]]
  key2 <- keys[[2]]
  key3 <- keys[[3]]

  # d$paleoData
  pc <- hasData(d[["metadata"]], key1)

  # section
  tryCatch({
    if (!is.null(pc)){
      for (i in 1:length(pc)){
        
        # measurement
        for (j in 1:length(pc[[i]][[key2]])){
          
          # check in measurement table
          if (!is.null(hasData(pc[[i]][[key2]], j))){
            new.table <- moveColsUp(pc[[i]][[key2]][[j]])
            d[["metadata"]][[key1]][[i]][[key2]][[j]] <- new.table
          }
        } ## measurement
        
        # loop in models
        for (j in 1:length(pc[[i]][[key3]])){
          
          # summary
          if (!is.null(hasData(pc[[i]][[key3]][[j]], "summaryTable"))){
            new.table <- moveColsUp(pc[[i]][[key3]][[j]][["summaryTable"]])
            d[["metadata"]][[key1]][[i]][[key3]][[j]][["summaryTable"]] <- new.table
          } # end summary
          
          # ensemble
          if (!is.null(hasData(pc[[i]][[key3]][[j]], "ensembleTable"))){
            new.table <- moveColsUp(pc[[i]][[key3]][[j]][["ensembleTable"]])
            d[["metadata"]][[key1]][[i]][[key3]][[j]][["ensembleTable"]] <- new.table
          } # end ensemble
          
          # distribution
          if(!is.null(hasData(pc[[i]][[key3]][[j]], "distributionTable"))){
            for (k in 1:length(pc[[i]][[key3]][[j]][["distributionTable"]])){
              new.table <- moveColsUp(pc[[i]][[key3]][[j]][["distributionTable"]][[k]])
              d[["metadata"]][[key1]][[i]][[key3]][[j]][["distributionTable"]][[k]] <- new.table
            }
          } ## end distribution
          
        } ## end models
      }
    }
  }, error=function(cond){
    print(paste0("error load_lipds_indexing: indexSection: ", key1, ", ", cond));
  })
  return(d)
}

#' Get rid of "columns" layer so that the columns data is directly beneath its corresponding table
#' @export
#' @keywords internal
#' @param table Table to be reorganized
#' @return table Modified table
moveColsUp <- function(table){
  #look for columns
  if(is.null(table[["columns"]])){
    #already been removed - just needs to be named
   stop("there should be a columns variable in here")
  }else{
    # create a list
    new.cols <- list()
    col.len <- length(table[["columns"]])

    # loop for each column
    for (i in 1:col.len){
      # get the variable name
      try(vn <- table[["columns"]][[i]][["variableName"]])
      if (is.null(vn)){
        table[[i]] <- table[["columns"]][[i]]
      } else {
        table[[vn]] <- table[["columns"]][[i]]
      }
    }
    # remove the columns item from table
    table[["columns"]] <- NULL
  }
  return(table)
}



