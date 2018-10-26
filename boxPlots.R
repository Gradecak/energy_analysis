library(fitdistrplus)
library(ggpubr)
library(dplyr)

source("/Users/marijangradecak/Documents/DroppingElectrons/data_loader.R")

plotTreatment <- function(load, treatments, rm_outlier=FALSE){
  data <- c()
  for(t in treatments){
    print(t)
    x <- strsplit(t, "_")[[1]]
    data <- rbind(data, read_data(load, x[1], x[2], group=TRUE, rm_outliers=rm_outlier))   
  }
  
  ggboxplot(data, x = "group", y = "value", 
            color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#00FF00"),
            ylab = "Joules", xlab = "Treatment")
}

#box plots
plotTreatment('subseq', c('fast_sw', 'fast_nosw'))
plotTreatment('subseq', c('fast_sw', 'fast_nosw'))
plotTreatment('subseq', c('slow_sw', 'slow_nosw'))
plotTreatment('initial', c('fast_sw', 'fast_nosw'))
plotTreatment('initial', c('slow_sw', 'slow_nosw'), rm_outlier=TRUE)
plotTreatment('initial', c('slow_sw', 'fast_sw'), rm_outlier=TRUE)
plotTreatment('initial', c('slow_nosw', 'fast_nosw'), rm_outlier=TRUE)
plotTreatment('initial', c('slow_sw', 'fast_sw'), rm_outlier = TRUE)
plotTreatment('initial', c('slow_nosw', 'fast_nosw'), rm_outlier = TRUE)
plotTreatment('subseq', c('slow_sw', 'fast_sw'))
plotTreatment('subseq', c('slow_nosw', 'fast_nosw'))
