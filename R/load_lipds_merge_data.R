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
          meta.cols <- table[["columns"]][[1]]
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
            meta.cols <- table[["columns"]][[1]]
            columns <- merge.csv(csv.cols, meta.cols)
            toCopy = which(names(table)!="columns")
            for(tt in toCopy){
              chronModel[[j]]$summaryTable[[names(table)[tt]]]=table[[names(table)[tt]]]
            }
            chronModel[[j]]$summaryTable$columns = columns

          }

          # ensemble table
          # d$chronData[[i]]$chronModel[[j]]$ensembleTable[[1]] - only one per model
          table <- pc[[i]][[key3]][[j]][["ensembleTable"]]

          # get filename
          filename <- table[["filename"]]

          if (!is.null(filename)){
            csv.cols <- d[["csv"]][[filename]]
            meta.cols <- table[["columns"]][[1]]
            columns <- merge.csv(csv.cols, meta.cols)
            toCopy = which(names(table)!="columns")
            for(tt in toCopy){
              chronModel[[j]]$ensembleTable[[names(table)[tt]]]=table[[names(table)[tt]]]
            }
            chronModel[[j]]$ensembleTable$columns = columns
          }

          # distribution tables
          distributionTable = vector(mode = "list",length = dim(pc[[i]][[key3]][[j]][["distributionTable"]][[1]])[1])
          if(length(distributionTable)>=1){

            # d$chronData[[i]]$chronModel[[j]]$distributionTable
            for (k in 1:dim(pc[[i]][[key3]][[j]][["distributionTable"]][[1]])[1]){

              # d$chronData[[i]]$chronModel[[j]]$distributionTable[[k]]
              table <- pc[[i]][[key3]][[j]][["distributionTable"]][[k]]

              filename <- table[["filename"]]
              if (!is.null(filename)){
                csv.cols <- d[["csv"]][[filename]]
                meta.cols <- table[["columns"]][[1]]
                columns  <- merge.csv(csv.cols, meta.cols)
                toCopy = which(names(table)!="columns")
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
#' @export
#' @param csv.cols CSV data for this file
#' @param meta.cols Target metadata columns
#' @return meta.cols Modified metadata columns
merge.csv <- function(csv.cols, meta.cols){
  meta.list <- list()

  # go through the columns
  for (i in 1:dim(meta.cols)[1]){

    # grab the data
    csv.col <- csv.cols[[i]]

    #make it a list
    meta.list[[i]]=as.list(meta.cols[i,])

    # assign in the values
    meta.list[[i]]$values = csv.col
  }
  return(meta.list)
}

#' Special merge function for ensemble table entries
#' @export
#' @param csv.cols CSV data for this file
#' @param meta.cols Target metadata columns
#' @return meta.cols Modified metadata columns
merge.csv.ens <- function(csv.cols, meta.cols){
  meta.list <- list()

  for (i in 1:dim(meta.cols)[1]){
    #go through the columns
    #grab the data
    meta.list[[i]]=as.list(meta.cols[i,])
    meta.list[[i]]$number = meta.list[[i]]$number[[1]]
    csv.col <- csv.cols[,meta.list[[i]]$number]

    # assign in the values
    meta.list[[i]]$values = csv.col
  }
  return(meta.list)
}


