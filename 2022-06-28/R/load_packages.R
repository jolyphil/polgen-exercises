load_one_package <- function(one_package){
  if (one_package %in% rownames(installed.packages()) == FALSE){
    install.packages(one_package,dep=TRUE)
  }
  
  if(!require(one_package,character.only = TRUE)) {
    stop("Package not found")
  }
  
  require(one_package,character.only = TRUE)
  
}

load_packages <- function(p){
  
  sapply(p, load_one_package)
  
}