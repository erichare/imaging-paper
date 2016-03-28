degraded_stats <- read.csv("data-degraded-25-25/bullet-stats-degraded.csv")
regular_stats <- read.csv("data-25-25/bullet-stats.csv")
original_stats <- read.csv("data-25-25/bullet-stats-old.csv")

library(ggplot2)

qplot(original_stats$CMS, regular_stats$cms.per.y)
