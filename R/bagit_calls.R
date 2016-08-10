###############################################
## Bagit Calls
##
## An R layer that uses rPython to makes calls
## to the python bagit module. Cross-language
## functionality.
###############################################



#' Since we don't have a way of getting the bagit module in R,
#' all we can do is use the default bag function by calling the
#' full python file on a directory. This will create a bag.
#' @export
#' @param path The path to the directory that needs to be bagged
#' @return none
bagit <- function(data.dir, initial.dir){
  # check for bagit.py in case they're working in the package folder
  bagit.script <- file.path(initial.dir, "R", "bagit.py")
  if(!file.exists(bagit.script)){
    print("Select your bagit.py file")
    bagit.script=file.choose()
  }
  Sys.chmod(bagit.script, "777")
  # do a system call for bagit on the tmp folder
  ret <- system(paste0(bagit.script, " ", data.dir), ignore.stdout = TRUE, ignore.stderr = TRUE)
  # do soft bagit if system call status returns 1 (error)
  return(ret)
}


# Check that bag is valid
validate.bag <- function(bag){

}

# Create bag from LiPD directory
create.bag <- function(){

}

# Open bag given LiPD directory
open.bag <- function(){

}

# Create, open, and save bag
close.bag <- function(){

}

# Check bag.info for DOI resolved flag true/false
doi.resolved.flag <- function(){

}

