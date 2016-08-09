#' Main function. Loop through all LiPD records, saving, and then cleaning up
#' @export
#' @param D LiPD Library
#' @return none
save.lipds <- function(D){

  # starting directory. this is where files will be saved to
  initial.dir <- getwd()

  # loop by record names
  lpds <- names(D)

  for (i in 1:length(lpds)){
    # reference to single lipd record
    d <- D[[lpds[[i]]]]
    # call one lipd by name, and pass the name too
    print(sprintf("saving: %s", lpds[[i]]))
    save.lipd.file(d, lpds[[i]])
  }

  # return back to the initial directory
  setwd(initial.dir)
}
