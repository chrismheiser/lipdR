#' Zip a directory, and move up a level
#' @export
#' @param dir Directory to be zipped
#' @param tmp Directory that holds resulting zip file
#' @return none
zipper <- function(dir, tmp){
  # zip the top lipd directory. zip file is create one level up
  setwd(dir)
  include.files <- list.files(getwd(), recursive = TRUE)
  suppressAll(zip(dir, include.files))
  setwd(tmp)
}
