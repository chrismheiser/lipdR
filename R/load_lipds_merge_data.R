###############################################
## Load LiPDs - Merge
## Merge metadata and csv into one LiPD object
###############################################


merge.main <- function(D, lpds){

  paleo <- c("paleoData", "paleoMeasurementTable", "paleoModel")
  chron <- c("chronData", "chronMeasurementTable", "chronModel")

  for (lipd in 1:length(lpds)){
    name <- lpds[[lipd]]
    # merge paleo
    D[[name]] <- merge.data.lipd(D[[name]], paleo)
    # merge chron
    D[[name]] <- merge.data.lipd(D[[name]], chron)
  }

  return(D)
}

#' Merge csv numeric data into the metadata columns
#' @export
#' @param d LiPD metadata
#' @param keys Paleo or Chron keys
#' @return d Modified LiPD metadata
merge.data.lipd <- function(d, keys){

    # paleoData or chronData
    key1 <- keys[[1]]
    # measurement table
    key2 <- keys[[2]]
    # model table
    key3 <- keys[[3]]

    # d$chronData
    pc <- d[["metadata"]][[key1]]

    # section
    # d$chronData[[i]]
    for (i in 1:length(pc)){

      # measurement tables
      # d$chronData[[i]]chronMeasurementTable
      for (j in 1:length(pc[[i]][[key2]])){

        # d$chronData[[i]]chronMeasurementTable[[j]]
        table <- pc[[i]][[key2]][[j]]

        filename <- tryCatch(
          {path2 <- table[["filename"]]},
          error=function(cond){
            return(NULL)
          })
        if (!is.null(filename)){
          csv.cols <- d[["csv"]][[filename]]
          meta.cols <- table[["columns"]]
          d[["metadata"]][[key1]][[i]][[key2]][[j]][["columns"]] <- merge.csv(csv.cols, meta.cols)
        }
      } # end measurement tables

      # model tables
      if(any(names(pc[[i]])==key3)){

        # loop in models
        chronModel=vector(mode="list",length=length(pc[[i]][[key3]]))

        # d$chronData[[i]]$chronModel
        for (j in 1:length(pc[[i]][[key3]])){

          # d$chronData[[i]]$chronModel[[j]]

          # summary table
          # d$chronData[[i]]$chronModel[[j]]$summaryTable[[1]] - only one per model
          table <- pc[[i]][[key3]][[j]][["summaryTable"]]
          filename <- table[["filename"]]
          if (!is.null(filename)){
            csv.cols <- d[["csv"]][[filename]]
            meta.cols <- table[["columns"]]
            columns <- merge.csv(csv.cols, meta.cols)
            toCopy = which(names(table)!="columns")
            for(tt in toCopy){
              chronModel[[j]]$summaryTable[[names(table)[tt]]] <- table[[names(table)[tt]]]
            }
            chronModel[[j]]$summaryTable$columns <-columns

          }

          # ensemble table
          # d$chronData[[i]]$chronModel[[j]]$ensembleTable[[1]] - only one per model
          table <- pc[[i]][[key3]][[j]][["ensembleTable"]]

          # get filename
          filename <- table[["filename"]]

          if (!is.null(filename)){
            csv.cols <- d[["csv"]][[filename]]
            meta.cols <- table[["columns"]]
            columns <- merge.csv(csv.cols, meta.cols)
            toCopy = which(names(table)!="columns")
            for(tt in toCopy){
              chronModel[[j]]$ensembleTable[[names(table)[tt]]]=table[[names(table)[tt]]]
            }
            chronModel[[j]]$ensembleTable$columns = columns
          }

          # distribution tables
          distributionTable = vector(mode = "list",length = length(pc[[i]][[key3]][[j]][["distributionTable"]][[1]])[1])
          if(length(distributionTable)>=1){

            # d$chronData[[i]]$chronModel[[j]]$distributionTable
            for (k in 1:length(pc[[i]][[key3]][[j]][["distributionTable"]][[1]])[1]){

              # d$chronData[[i]]$chronModel[[j]]$distributionTable[[k]]
              table <- pc[[i]][[key3]][[j]][["distributionTable"]][[k]]

              filename <- table[["filename"]]
              if (!is.null(filename)){
                csv.cols <- d[["csv"]][[filename]]
                meta.cols <- table[["columns"]]
                columns  <- merge.csv(csv.cols, meta.cols)
                toCopy <- which(names(table)!="columns")
                for(tt in toCopy){
                  distributionTable[[k]][[names(table)[tt]]]=table[[names(table)[tt]]]
                }
                distributionTable[[k]]$columns = columns

              }
            }
            chronModel[[j]]$distributionTable=distributionTable
          }

          #add in anything that we didn't recreate
          icm=names(pc[[i]][[key3]][[j]])
          cmnames=names(chronModel[[j]])
          toAdd = which(!(icm %in% cmnames))
          for(ta in 1:length(toAdd)){
            chronModel[[j]][[icm[toAdd[ta]]]]=pc[[i]][[key3]][[j]][[icm[toAdd[ta]]]]
          }
        } ##end chronModel loop
        d[["metadata"]][[key1]][[i]]$chronModel = chronModel

      }#end chronModel

    } ## end chrondata

  return(d)
}


#' Merge CSV data into the metadata
#' @exports
#' @param csv.cols CSV data for this file
#' @param meta.cols Target metadata columns
#' @return meta.cols Modified metadata columns
merge.csv <- function(csv.cols, meta.cols){

  col.ct <- length(meta.cols)
  # go through the columns
  for (i in 1:col.ct){

    # special case for ensemble tables - a "column" that holds many columns
    if (is.list(meta.cols[[i]][["number"]]) | length(meta.cols[[i]][["number"]]) > 1){
      tmp <- list()
      nums <- meta.cols[[i]][["number"]]
      for (j in 1:length(nums)){
        tmp[[j]] <- csv.cols[[nums[[j]]]]
      }
      meta.cols[[i]][["values"]] <- tmp
    }
    else {
      # assign in the values
      meta.cols[[i]][["values"]] <- csv.cols[,meta.cols[[i]][["number"]]]
    }

  }
  return(meta.cols)
}

