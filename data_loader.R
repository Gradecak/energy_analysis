get_sites <- function(load, network, sw){
  path <- "/Users/marijangradecak/Documents/DroppingElectrons/results/%s/%s_%s/data/nexus7"  
  full_path <- sprintf(path, load, network, sw)
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

read_data <- function(load, network, sw, group=FALSE, rm_outliers=FALSE){
  files <- get_sites(load,network, sw)
  vals <- unlist(lapply(files, read.delim), 
                 recursive=TRUE,
                 use.names=FALSE)
  if(group){
    name <- paste(network,sw, sep="_")
    temp <- data.frame(value=vals, group=name)
  }
  else{
    temp <- data.frame(value=vals, network=network, sw=sw)  
  }
  if(rm_outliers){
      outliers = boxplot(temp, plot=FALSE)$out
      temp <- temp[!temp$value %in% outliers, ]      
  }
  return(temp)
}