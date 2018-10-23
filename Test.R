get_sites <- function(x, y){
  path <- "./results/%s/%s/data/nexus7"  
  full_path <- sprintf(path, x, y)
  files <- list.files(full_path, include.dirs=TRUE)
  ret <- c()
  for(f in files){
    p <- paste(full_path, f, sep="/")
    if(dir.exists(p)){
      ret <- c(ret, paste(p, "comandroidchrome/android/energy_consumed_Joule.txt", sep="/"))
    }
  }
  return(ret)
}

read_data <- function(x,y){
  files <- get_sites(x,y)
  vals <- unlist(lapply(files, read.delim), 
                recursive=TRUE,
                use.names=FALSE)
  return(vals)
}

print(read_data('initial', 'fast_sw'))
