load_aux <- function(){
  path <- '/volumes/shared/nda2_baseline/data/auxiliaries/'
  
    
  file_base_name <- sub("_V[0-9]+\\.csv$", "", basename(path))
    
  # List all files in the specified directory that match the base file name with a version
  files <- list.files(path = paste0(path), 
                      pattern = paste0(file_base_name, "_V[0-9]+\\.csv$"), full.names = TRUE)
    
  # Extract version numbers from file names
  versions <- sapply(files, function(x) {
    # Simplify the pattern to just extract the digits after '_V'
    version_str <- gsub(".*_V([0-9]+)\\.csv$", "\\1", x)
    as.numeric(version_str)
    })
  
  # Get the file with the highest version number
  latest_file <- files[which.max(versions)]
    
  # we load the auxiliary information just to check the data quickly:
  auxiliaries <- read.csv(paste0(latest_file, sep = ''))%>%
    dplyr::select(-any_of('X'))
  return(list(aux = auxiliaries))
}