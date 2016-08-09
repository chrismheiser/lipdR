###############################################
## Load LiPDs - Versions
## Converts the incoming LiPD data into the
## current LiPD version structure
###############################################

#' Convert LiPD version whenever necessary
#' @export
#' @param D LiPD Library
#' @return D modified LiPD Library
convert.version <- function(D){
  # Loop once for every LiPD object
  for (i in 1:length(D)){
    # Check which version this LiPD file is
    lipd <- D[[i]]
    version <- get.version(lipd)

    # check and convert any data frames into lists
    lipd <- convert.dfs2lst(lipd)

    # Replace the LiPD data with the new converted structure
    D[[i]] <- lipd
  }
  return(D)
}

# Get the version number from metadata
get.version <- function(d){
  version <- as.numeric(d[["metadata"]][["LiPDVersion"]])
  if (length(version)==0){
    version <- 1.0
  }
  else if (version != 1.1 & version != 1.2){
    print("LiPD Version is invalid")
  }
  return(version)
}

#' Check / convert and fixed data frames into scalable lists
#' @export
#' @param d LiPD metadata
#' @return d Modified LiPD metadata
convert.dfs2lst <- function(d){

  paleos <- c("paleoData", "paleoMeasurementTable", "paleoModel")
  chrons <- c("chronData", "chronMeasurementTable", "chronModel")

  # convert single entries to lists. matching structure to 1.2
  d <- convert.s2m(d, paleos)
  d <- convert.s2m(d, chrons)

  return(d)
}

#' Convert from a single fixed table, to a multiple scalable table
#' (LiPD Verison 1.1 to 1.2 change)
#' @export
#' @param d LiPD metadata
#' @return d Modified LiPD metadata
convert.s2m <- function(d, keys){

  key1 <- keys[[1]]
  key2 <- keys[[2]]
  key3 <- keys[[3]]

  # PALEODATA
  # data frame?
  if (is.data.frame(d[["metadata"]][[key1]])){
    tmp <- d[["metadata"]][[key1]]
    d[["metadata"]][[key1]] <- list()
    d[["metadata"]][[key1]][[1]] <- as.list(tmp)
  }
  # multiples?
  path1 <- tryCatch(
    {path1 <- d[["metadata"]][[key1]][[1]]},
    error=function(cond){ return(NULL)} )
  # convert to multiples
  if (is.null(path1)){
    tmp <- d[["metadata"]][[key1]]
    d[["metadata"]][[key1]] <- list()
    d[["metadata"]][[key1]][[1]] <- tmp
  } # END PALEODATA

  # loop
  for (i in 1:length(d[["metadata"]][[key1]])){

    # PALEODATA[[i]]
    # data frame?
    if (is.data.frame(d[["metadata"]][[key1]][[i]])){
      d[["metadata"]][[key1]][[i]] <- as.list(d[["metadata"]][[key1]][[i]])
    }

    # MEAS + MODEL
    # table exists ?
    # d$paleoData[[i]]$paleoMeasurementTable
    path.meas <- tryCatch(
      {path.meas <- d[["metadata"]][[key1]][[i]][[key2]]},
      error=function(cond){return(NULL)})
    # table exists ?
    # d$paleoData[[i]]$paleoModel
    path.model <- tryCatch(
      {path.model <- d[["metadata"]][[key1]][[i]][[key3]]},
      error=function(cond){return(NULL)})
    # tables do not exist.
    # make a meas table
    if (is.null(path.meas) & is.null(path.model)){
      tmp <- d[["metadata"]][[key1]][[i]]
      d[["metadata"]][[key1]][[i]] <- list()
      d[["metadata"]][[key1]][[i]][[key2]] <- list()
      d[["metadata"]][[key1]][[i]][[key2]][[1]] <- tmp
    }  # end meas and model

    # DIRECT
    # multiples ?
    # d$paleoData[[i]]$paleoMeasurementTable$columns
    path.direct <- tryCatch(
      {
        if (!is.null(d[["metadata"]][[key1]][[i]][[key2]][["columns"]])){
          path.direct = TRUE
        } else {
            path.direct = NULL
        }
      }, error = function(cond){return(NULL)}
    )
    # convert to multiples
    # d$paleoData[[i]]$paleoMeasurementTable
    if (!is.null(path.direct)){
      tmp <- d[["metadata"]][[key1]][[i]][[key2]]
      d[["metadata"]][[key1]][[i]][[key2]] <- list()
      d[["metadata"]][[key1]][[i]][[key2]][[1]] <- tmp
    } # end direct data

    # MEASUREMENT
    # paleoData[[i]]paleoMeasurementTable
    # data frame ?
    if (is.data.frame( d[["metadata"]][[key1]][[i]][[key2]])){
      d[["metadata"]][[key1]][[i]][[key2]] <- as.list(d[["metadata"]][[key1]][[i]][[key2]])
    }
    # multiples ?
    path2 <- tryCatch(
      {path2 <- d[["metadata"]][[key1]][[1]][[key2]][[1]]},
      error=function(cond){return(NULL)})
    # convert to multiples
    # d$paleoData[[i]]$paleoMeasurementTable[[j]]
    if (is.null(path2)){
      tmp <- d[["metadata"]][[key1]][[1]][[key2]]
      d[["metadata"]][[key1]][[1]][[key2]] <- list()
      d[["metadata"]][[key1]][[1]][[key2]][[1]] <- tmp
    } # END MEASUREMENT

    # loop
    for (j in 1:length(d[["metadata"]][[key1]][[i]][[key2]])){

      # MEASUREMENT[[j]]
      # paleoData[[i]]paleoMeasurementTable[[j]]
      # data frame?
      if (is.data.frame(d[["metadata"]][[key1]][[i]][[key2]][[j]])){
        d[["metadata"]][[key1]][[i]][[key2]][[j]] <- as.list(d[["metadata"]][[key1]][[i]][[key2]][[j]])
      } # END MEASUREMENT[[j]]

    }

    # continue if Model table present
    if (length(d[["metadata"]][[key1]][[i]][[key3]]) > 0){
      # MODEL
      # paleoData[[i]]paleoModel
      # data frame ?
      if (is.data.frame( d[["metadata"]][[key1]][[i]][[key3]])){
        d[["metadata"]][[key1]][[i]][[key3]] <- as.list(d[["metadata"]][[key1]][[i]][[key3]])
      }
      # multiples ?
      path <- tryCatch(
        {path <- d[["metadata"]][[key1]][[1]][[key3]][[1]]},
        error=function(cond){return(NULL)})
      # convert to multiples
      if (is.null(path)){
        tmp <- d[["metadata"]][[key1]][[1]][[key3]]
        d[["metadata"]][[key1]][[1]][[key3]] <- list()
        d[["metadata"]][[key1]][[1]][[key3]][[1]] <- tmp
      } # END MODEL

      # loop
      for (j in 1:length(d[["metadata"]][[key1]][[i]][[key3]])){

        # MODEL[[j]]
        # paleoModel[[j]]
        # data frame ?
        if (is.data.frame( d[["metadata"]][[key1]][[i]][[key3]][[j]])){
          d[["metadata"]][[key1]][[i]][[key3]][[j]] <- as.list(d[["metadata"]][[key1]][[i]][[key3]][[j]])
        }

        # SUMMARY
        # paleoModel[[j]]$summaryTable
        # data frame ?
        if (is.data.frame( d[["metadata"]][[key1]][[i]][[key3]][[j]][["summaryTable"]])){
          d[["metadata"]][[key1]][[i]][[key3]][[j]][["summaryTable"]] <- as.list(d[["metadata"]][[key1]][[i]][[key3]][[j]][["summaryTable"]])
        }

        # ENSEMBLE
        # paleoModel[[j]]$ensembleTable
        # data frame ?
        if (is.data.frame( d[["metadata"]][[key1]][[i]][[key3]][[j]][["ensembleTable"]])){
          d[["metadata"]][[key1]][[i]][[key3]][[j]][["ensembleTable"]] <- as.list(d[["metadata"]][[key1]][[i]][[key3]][[j]][["sensembleTable"]])
        }

        # DISTRIBUTION
        # paleoModel[[j]]$distributionTable
        if (is.data.frame( d[["metadata"]][[key1]][[i]][[key3]][[j]][["distributionTable"]])){
          d[["metadata"]][[key1]][[i]][[key3]][[j]][["distributionTable"]] <- as.list(d[["metadata"]][[key1]][[i]][[key3]][[j]][["distributionTable"]])
        }
        # multiples ?
        path <- tryCatch(
          {path <- d[["metadata"]][[key1]][[1]][[key3]][[j]][["distributionTable"]][[1]]},
          error=function(cond){return(NULL)})
        # convert to multiples
        if (is.null(path)){
          tmp <- d[["metadata"]][[key1]][[i]][[key3]][[j]][["distributionTable"]]
          d[["metadata"]][[key1]][[i]][[key3]][[j]][["distributionTable"]] <- list()
          d[["metadata"]][[key1]][[i]][[key3]][[j]][["distributionTable"]][[1]] <- tmp
        } # end paleo model

        # loop
        for (k in 1:length(d[["metadata"]][[key1]][[i]][[key3]][[j]][["distributionTable"]])){

          # DISTRIBUTION[[k]]
          if (is.data.frame( d[["metadata"]][[key1]][[i]][[key3]][[j]][["distributionTable"]][[k]])){
            d[["metadata"]][[key1]][[i]][[key3]][[j]][["distributionTable"]][[k]] <- as.list(d[["metadata"]][[key1]][[i]][[key3]][[j]][["distributionTable"]][[k]])
          } # END DISTRIBUTION[[k]]

        } # end dist loop

      } # end models

    } # end if


  } # end section

  # change the LiPDVersion value to 1.2
  d[["metadata"]][["LiPDVersion"]] <- 1.2
  return(d)
}
