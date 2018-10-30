library(fitdistrplus)
library(ggpubr)
library(ggplot2)
library(cowplot)
library(dplyr)

source("/Users/marijangradecak/Documents/DroppingElectrons/data_loader.R")

plotTreatment <- function(load, treatments, rm_outlier=FALSE){
  data <- c()
  for(t in treatments){
    print(t)
    x <- strsplit(t, "_")[[1]]
    data <- rbind(data, read_data(load, x[1], x[2], group=TRUE, rm_outliers=rm_outlier))   
  }
  
  # color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#00FF00"),  
  return(ggboxplot(data, x = "group", y = "value", 
            ylab = "Joules", xlab = "Treatment"))
}

fix_site_name <- function(filename){
  f <- gsub("^(httpswww|https)", "", filename)
  x <- gsub("com$", "", f)
  print(x)
  return(x)
}

#box plots
# initial fast sw on vs off

rm_outlier <- TRUE
plot_grid(
  plotTreatment('initial', c('fast_sw', 'fast_nosw'), rm_outlier = rm_outlier),
  plotTreatment('initial', c('slow_sw', 'slow_nosw'), rm_outlier = rm_outlier),
  plotTreatment('initial', c('slow_sw', 'fast_sw'), rm_outlier = rm_outlier),
  plotTreatment('initial', c('slow_nosw', 'fast_nosw'), rm_outlier = rm_outlier),
  nrow=2,
  ncol=2
)

plot_grid(
  plotTreatment('subseq', c('fast_sw', 'fast_nosw'), rm_outlier = rm_outlier),
  plotTreatment('subseq', c('slow_sw', 'slow_nosw'), rm_outlier = rm_outlier), 
  plotTreatment('subseq', c('slow_sw', 'fast_sw'), rm_outlier = rm_outlier),
  plotTreatment('subseq', c('slow_nosw', 'fast_nosw'), rm_outlier = rm_outlier),
  nrow=2,
  ncol=2
)

rm_outliers <- TRUE
initial <- read_data('initial', 'fast', 'sw', rm_outliers = rm_outliers)
initial <- rbind(initial, read_data('initial', 'fast','nosw', rm_outliers = rm_outliers))
initial <- rbind(initial, read_data('initial', 'slow','sw', rm_outliers = rm_outliers))
initial <- rbind(initial, read_data('initial', 'slow','nosw', rm_outliers = rm_outliers))
subseq <- read_data('subseq', 'fast','sw', rm_outliers = rm_outliers)
subseq <- rbind(subseq, read_data('subseq', 'slow','sw', rm_outliers = rm_outliers))
subseq <- rbind(subseq, read_data('subseq', 'fast','nosw', rm_outliers = rm_outliers))
subseq <- rbind(subseq, read_data('subseq', 'slow','nosw', rm_outliers = rm_outliers))


# intiail sw vs nosw
ggboxplot(initial, x = "sw", y = "value", 
          ylab = "Joules", xlab = "Treatment")

# subseq sw vs nosw
ggboxplot(subseq, x = "sw", y = "value", 
          ylab = "Joules", xlab = "Treatment")

#plotting individual sites
ss_f <- read_sites_data('subseq', 'fast', 'sw', num_sites = 7)
ss_f <- rbind(ss_f, read_sites_data('subseq', 'fast', 'nosw', num_sites = 7))
ss_f$site <- fix_site_name(ss_f$file)
#get list of sites that we use for initial load
sites <- unique(ss_f$file)
print(sites)

# remove outliers
outliers = boxplot(ss_f$value, plot=FALSE)$out
ss_f <- ss_f[!ss_f$value %in% outliers, ]

bp <- ggboxplot(ss_f, x="sw", y="value") +
  background_grid(major = 'y', minor = "none") + # add thin horizontal lines 
  panel_border() + # and a border around each panel
  labs(x="Service Worker status", y="Energy (J)")
bp + facet_grid(. ~ site) +
  theme(strip.text.x = element_text(size = 7))

i_f <- read_sites_data('initial', 'fast', 'sw')
i_f <- rbind(i_f, read_sites_data('initial', 'fast', 'nosw'))
i_f <- i_f[i_f$file %in% sites, ]
i_f$site <- fix_site_name(i_f$file)

outliers = boxplot(i_f$value, plot=FALSE)$out
i_f <- i_f[!i_f$value %in% outliers, ]
bp <- ggboxplot(i_f, x="sw", y="value") +
  background_grid(major = 'y', minor = "none") + # add thin horizontal lines 
  panel_border() + # and a border around each pane
  labs(x="Service Worker status", y="Energy (J)")
bp + facet_grid(. ~ site) +
  theme(strip.text.x = element_text(size = 7))
