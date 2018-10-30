library(stringr)
path <- "/Users/marijangradecak/Documents/DroppingElectrons/results/%s/%s_%s/data/nexus7"  

get_sites <- function(load, network, sw){
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


read_sites_data <- function(load, network, sw, num_sites=.Machine$integer.max){
    files <- get_sites(load,network, sw)
    vals <- lapply(files, function(x) 
          list(value=unlist(read.delim(x), recursive = TRUE, use.names = FALSE), file=x)
      )
    
    data <- c()
    i <- 0
    for(v in vals){
      if(i < num_sites){
        tempdf <- data.frame(values = v[1])
        file <- strsplit(v[2]$file, "/")[[1]][11]
        tempdf$file <- file
        data <- rbind(data, tempdf)
        i <- i + 1
      }
    }
    
    data$network = network
    data$sw = sw
    return(data)
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

list_site_folders <- function(load, network, sw){
  full_path <- sprintf(path, load, network, sw)
  sites <- list.files(full_path, include.dirs=TRUE)
  return(paste(full_path, sites, sep="/"))
}