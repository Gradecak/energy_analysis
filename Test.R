library(fitdistrplus)
library(ggpubr)
library(dplyr)


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
  name <- paste(x,y, sep="_")
  temp <- data.frame(value=vals, group=name)
  return(temp)
}

plotTreatment <- function(load, treatments, rm_outlier=FALSE){
  data <- read_data(load, treatments[1])
  for(t in treatments[1:length(treatments)]){
    data <- rbind(data, read_data(load, t))   
  }

  #Remove the large outlier skewing boxplot
  if(rm_outlier){
    OutVals = boxplot(data, plot=FALSE)$out
    m <- max(sapply(OutVals, max))
    data <- data[!data$value == m, ]    
  }
  
  group_by(data, group) %>%
    summarise(
      count = n(),
      mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE)
    )
  
  ggboxplot(data, x = "group", y = "value", 
            color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#00FF00"),
            ylab = "Joules", xlab = "Treatment")
}

plotTreatment('subseq', c('fast_sw', 'fast_nosw'))
plotTreatment('subseq', c('slow_sw', 'slow_nosw'))
plotTreatment('initial', c('fast_sw', 'fast_nosw'))
plotTreatment('initial', c('slow_sw', 'slow_nosw'), rm_outlier=TRUE)






