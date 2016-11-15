###############################################
## Load LiPD files
## The main part of loading the the data to
## memory
###############################################

#' Import the data from each csv and jsonld file for given LiPD
#' @export
#' @keywords internal
#' @param lpd_noext List of lipd files without extention
#' @param tmp Char path to the temp folder in memory
#' @return out.list List of data for each lipd file
loadLipdFile <- function(lpd_noext, tmp){
  d <- list()

  # Move into the tmp folder
  setwd(tmp)

  print(sprintf("loading: %s", lpd_noext))
  tryCatch({
    setwd(lpd_noext)

    # real bagit. move into data folder
    if (dir.exists("data")){ setwd("data") }

    # fake bagit. no data folder. all files in root dir.
    d <- getData()
    
    # Move back up to the tmp directory
    setwd(tmp)
  },error=function(cond){
    print(paste0("Couldn't find the unarchived LiPD data. Make sure your LiPD filename matches the data set name: ", lpd_noext))
  })

  return(d)
}

#' Retrieve and import csv and jsonld files in the current directory.
#' @export
#' @keywords internal
#' @return data.list List of data for one LiPD file
getData <- function(){
  data.list <- list()
  # list of csv files
  c <- listFiles("csv")
  # csv data placeholder
  c.data=vector(mode="list",length=length(c))
  # import each csv file
  for (ci in 1:length(c)){
    df=read.csv(c[ci], header=FALSE, blank.lines.skip = FALSE,na.strings = c("nan", "NaN", "NAN", "NA"))
    c.data[[c[ci]]]=df
  }

  # jsonld file - one per lpd
  j <- listFiles("jsonld")
  # import jsonld file
  if (length(j)>1){
    print("error load_lipds_file: getData: more than 1 jsonld file found")
    for(i in 1:length(j)){
      print(j[[i]])
    }
    data.list[["metadata"]] <- list()
  } else {
    # use jsonlite to parse json from file
    j.data <- jsonlite::fromJSON(j, simplifyDataFrame = FALSE)
    # remove empty items from the json
    j.data <- removeEmptyRec(j.data)
    # combine data for return.
    data.list[["metadata"]] <- j.data
  }

  data.list[["csv"]] <- c.data
  # data.list[["csv"]] <- clean.csv(data.list[["csv"]])

  return(data.list)
}
