library(fitdistrplus)
library(ggpubr)
library(dplyr)
library(car)
library(FSA)
library(lmtest)

source("/Users/marijangradecak/Documents/DroppingElectrons/data_loader.R")
rm_outliers <- TRUE
initial <- read_data('initial', 'fast', 'sw', rm_outliers = rm_outliers)
initial <- rbind(initial, read_data('initial', 'fast','nosw', rm_outliers = rm_outliers))
initial <- rbind(initial, read_data('initial', 'slow','sw', rm_outliers = rm_outliers))
initial <- rbind(initial, read_data('initial', 'slow','nosw', rm_outliers = rm_outliers))
subseq <- read_data('subseq', 'fast','nosw', rm_outliers = rm_outliers)
subseq <- rbind(subseq, read_data('subseq', 'fast','sw', rm_outliers = rm_outliers))
subseq <- rbind(subseq, read_data('subseq', 'slow','sw', rm_outliers = rm_outliers))
subseq <- rbind(subseq, read_data('subseq', 'slow','nosw', rm_outliers = rm_outliers))

kruskal.test(value ~ network , data = initial)
kruskal.test(value ~ sw , data = initial)

initial$group <- paste(initial$sw, initial$network, sep = "_")
initial$group <- as.factor(initial$group)
subseq$group <- paste(subseq$sw, subseq$network, sep = "_")
subseq$group <- as.factor(subseq$group)

i_sw_fast <- read_data('initial', 'fast', 'sw', rm_outliers = TRUE)
i_nosw_fast <- read_data('initial', 'fast', 'nosw', rm_outliers = TRUE)

wilcox.test(i_sw_fast$value, i_nosw_fast$value)

kruskal.test(value ~ group , data = initial)
kruskal.test(value ~ group, data = subseq)

dunnTest(value ~ group, data = initial, method="bh")
dunnTest(value ~ group, data = subseq, method="bh")
