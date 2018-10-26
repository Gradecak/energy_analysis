library(fitdistrplus)
library(ggpubr)
library(dplyr)
library(car)

source("/Users/marijangradecak/Documents/DroppingElectrons/data_loader.R")

initial <- read_data('initial', 'fast', 'sw')
initial <- rbind(initial, read_data('initial', 'fast','nosw'))
initial <- rbind(initial, read_data('initial', 'slow','sw'))
initial <- rbind(initial, read_data('initial', 'slow','nosw'))
subseq <- rbind(subseq, read_data('subseq', 'fast','nosw'))
subseq <- rbind(subseq, read_data('subseq', 'fast','sw'))
subseq <- rbind(subseq, read_data('subseq', 'slow','sw'))
subseq <- rbind(subseq, read_data('subseq', 'slow','nosw'))

kruskal.test(value ~ network , data = initial)
