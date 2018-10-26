library(fitdistrplus)
library(ggpubr)
library(dplyr)

source("/Users/marijangradecak/Documents/DroppingElectrons/data_loader.R")


initial_fast_sw <- read_data('initial', 'fast', 'sw', rm_outliers = TRUE)
ggqqplot(initial_fast_sw$value)
shapiro.test(initial_fast_sw$value)

initial_fast_nosw <- read_data('initial', 'fast', 'nosw', rm_outliers = TRUE)
ggqqplot(initial_fast_nosw$value)
shapiro.test(initial_fast_nosw$value)

initial_slow_sw <- read_data('initial', 'slow', 'sw', rm_outliers = TRUE)
ggqqplot(initial_slow_sw$value)
shapiro.test(initial_slow_sw$value)

initial_slow_nosw <- read_data('initial', 'slow', 'nosw', rm_outliers = TRUE)
ggqqplot(initial_slow_sw$value)
shapiro.test(initial_slow_sw$value)


subseq_fast_sw <- read_data('subseq', 'fast', 'sw', rm_outliers = TRUE)
ggqqplot(subseq_fast_sw$value)
shapiro.test(subseq_fast_sw$value)

subseq_fast_nosw <- read_data('subseq', 'fast', 'nosw', rm_outliers = TRUE)
ggqqplot(subseq_fast_nosw$value)
shapiro.test(subseq_fast_nosw$value)

subseq_slow_sw <- read_data('subseq', 'slow', 'sw', rm_outliers = TRUE)
ggqqplot(subseq_slow_sw$value)
shapiro.test(subseq_slow_sw$value)

subseq_slow_nosw <- read_data('subseq', 'slow', 'nosw', rm_outliers = TRUE)
ggqqplot(subseq_slow_sw$value)
shapiro.test(subseq_slow_sw$value)

# distribution of initial data
initial <- rbind(initial_fast_nosw, initial_fast_sw)
initial <- rbind(initial, initial_slow_sw)
initial <- rbind(initial, initial_slow_nosw)
initial_plot <- ggqqplot(initial$value)
shapiro.test(initial$value)

#distribution of subseq data
subseq <- rbind(subseq_fast_nosw, subseq_fast_sw)
subseq <- rbind(subseq, subseq_slow_nosw)
subseq <- rbind(subseq, subseq_slow_sw)
subseq_plot <- ggqqplot(subseq$value)
subseqshapiro.test(subseq$value)

ggarrange(initial_plot, subseq_plot, ncol=2, labels = c("Initial Distribution", "Subseq Distribution"))

