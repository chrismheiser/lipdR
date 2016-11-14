###############################################
## Load Lipds - wrapper
## Combines all the file loading functions
## into one process
###############################################

#' Main LiPD loading function. Combines all processes into one.
#' @export
#' @return D LiPD Library
loadLipds <- function(){
  # setModules()
  options(warn = -1)

  # Ask user where files are stored
  path.and.file <- getSrcOrDst()

  # Do initial set up
  working.dir <- path.and.file[["dir"]]
  assign("working.dir", working.dir, envir = .GlobalEnv)
  setwd(working.dir)
  tmp <- createTmpDir()

  # Get names of lipd files present
  lpds_ext <- getListLpdExt(path.and.file)
  lpds <- stripExtension(lpds_ext)

  tryCatch({
    # Unzip the lipd files to the temp workspace
    unzipper(lpds_ext, tmp)

    # Start importing data from the unpacked temp workspace
    D <- loadLipdFiles(tmp, lpds)

    # Convert metadata structure to newest LiPD version
    D <- convertVersion(D)

    # Now you have all the data loaded in memory, place data from csv into columns
    D <- addCsvToMetadata(D, lpds)

    # Change columns and tables to index-by-name
    D <- indexByName(D, lpds)

    # We no longer need the csv and metadata separate parts. Link straight to the data.
    D <- removeLayers(D, lpds)

    # If there's only one lpds object in D, move it to the front
    if(length(D)==1){
      D <- D[[1]]
    }
  }, error = function(cond){
    print(paste0("error load_lipds_main: ", cond))
  })

  # Move back to the inital directory (Prior to temp folder)
  setwd(working.dir)

  return(D)
}
