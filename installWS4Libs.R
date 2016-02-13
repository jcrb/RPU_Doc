### this script checks if libraries for Workshop 4 are available, 
### if not it installs, then loads them all
### cengel - Feb 2015

libs <- c("sp", "rgdal", "classInt", "RColorBrewer", "ggplot2", "hexbin", "ggmap", "XML", "dplyr")

for (x in libs){
  if(x %in% rownames(installed.packages()) == FALSE) {
    print(paste0("installing ", x, "..."))
    install.packages(x)
  }
  else{
    print (paste0(x, " is already installed "))
  }
  library(x, character.only = TRUE)
}
