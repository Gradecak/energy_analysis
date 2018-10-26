library(fitdistrplus)
library(ggpubr)
library(dplyr)


get_sites <- function(x, y){
  path <- "/Users/marijangradecak/Documents/DroppingElectrons/results/%s/%s/data/nexus7"  
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

#normal distribution testing <INITIAL>
i_f_sw <- read_data('initial', 'fast_sw')
ggqqplot(i_f_sw$value)
shapiro.test(i_f_sw$value)
i_f_nosw <- read_data('initial', 'fast_nosw')
ggqqplot(i_f_sw$value)
shapiro.test(i_f_nosw$value)
i_s_sw <- read_data('initial', 'slow_sw')
ggqqplot(i_s_sw$value)
shapiro.test(i_s_sw$value)
i_s_nosw <- read_data('initial', 'slow_nosw')
ggqqplot(i_s_nosw$value)
shapiro.test(i_s_nosw$value)
#normal distribution testing <SUBSEQUENT>
s_f_sw <- read_data('subseq', 'fast_sw')
ggqqplot(s_f_sw$value)
shapiro.test(s_f_sw$value)
s_f_nosw <- read_data('subseq', 'fast_nosw')
ggqqplot(s_f_sw$value)
shapiro.test(s_f_nosw$value)
s_s_sw <- read_data('subseq', 'slow_sw')
ggqqplot(s_s_sw$value)
shapiro.test(s_s_sw$value)
s_s_nosw <- read_data('subseq', 'slow_nosw')
ggqqplot(s_s_nosw$value)
shapiro.test(s_s_nosw$value)

all_data <- read_data('initial', 'fast_sw')
all_data <- rbind(all_data, read_data('initial', 'fast_nosw'))
all_data <- rbind(all_data, read_data('initial', 'slow_sw'))
all_data <- rbind(all_data, read_data('initial', 'slow_nosw'))

#box plots
plotTreatment('subseq', c('fast_sw', 'fast_nosw'))
plotTreatment('subseq', c('slow_sw', 'slow_nosw'))
plotTreatment('initial', c('fast_sw', 'fast_nosw'))
plotTreatment('initial', c('slow_sw', 'slow_nosw'), rm_outlier=TRUE)


initial <- read_data('initial', 'fast_sw')
initial <- rbind(initial, read_data('initial', 'fast_nosw'))
initial <- read_data('initial', 'slow_sw')
initial <- rbind(initial, read_data('initial', 'slow_nosw'))

kruskal.test(value ~  group, data = initial)
pairwise.wilcox.test(initial$value, initial$group,
                     p.adjust.method = "BH")



