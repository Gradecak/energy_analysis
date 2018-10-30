library(fitdistrplus)
library(ggpubr)
library(dplyr)
library(plyr)
library(psych)

source("/Users/marijangradecak/Documents/DroppingElectrons/data_loader.R")

# initial load box plot
rm_outliers <- TRUE
initial <- read_data('initial', 'fast', 'sw', rm_outliers = rm_outliers)
initial <- rbind(initial, read_data('initial', 'fast','nosw', rm_outliers = rm_outliers))
initial <- rbind(initial, read_data('initial', 'slow','sw', rm_outliers = rm_outliers))
initial <- rbind(initial, read_data('initial', 'slow','nosw', rm_outliers = rm_outliers))
initial$load="initial"

subseq <- read_data('subseq', 'fast', 'sw', rm_outliers = rm_outliers)
subseq <- rbind(subseq, read_data('subseq', 'fast','nosw', rm_outliers = rm_outliers))
subseq <- rbind(subseq, read_data('subseq', 'slow','sw', rm_outliers = rm_outliers))
subseq <- rbind(subseq, read_data('subseq', 'slow','nosw', rm_outliers = rm_outliers))
subseq$load="subseq"

all <- rbind(initial, subseq)
ggboxplot(all, x = "load", y = "value", 
          ylab = "Joules", xlab = "Energy Consumed")

i_swon <- subset(initial, sw="sw")
i_swoff <- subset(initial, sw="nosw")

#summary of initial_sw vs initial_nosw
ddply(initial, ~ sw, summarise, 
      min=round(min(value), digits=2), 
      max=round(max(value), digits = 2), 
      sd=round(sd(value), digits=2),
      mean=round(mean(value), digits = 2),
      median=round(median(value), digits = 2),
      variance=round(var(value), digits =2),
      cv=round(sd(value)/var(value),digits=2)
)

#summary of subseq_sw vs subseq_nosw
ddply(subseq, ~ sw, summarise, 
      min=round(min(value), digits=2), 
      max=round(max(value), digits = 2), 
      sd=round(sd(value), digits=2),
      mean=round(mean(value), digits = 2),
      median=round(median(value), digits = 2),
      variance=round(var(value), digits =2),
      cv=round(sd(value)/var(value),digits=2)
)

#summary of data
ddply(all, ~load, summarise, 
      min=round(min(value), digits=2), 
      max=round(max(value), digits = 2), 
      sd=round(sd(value), digits=2),
      mean=round(mean(value), digits = 2),
      median=round(median(value), digits = 2),
      variance=round(var(value), digits =2),
      cv=round(sd(value)/var(value),digits=2)
      )

