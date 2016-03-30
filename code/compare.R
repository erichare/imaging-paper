degraded_stats <- read.csv("data-degraded-25-25/bullet-stats-degraded.csv")
regular_stats <- read.csv("data-25-25/bullet-stats.csv")
original_stats <- read.csv("data-25-25/bullet-stats-old.csv")

library(ggplot2)
library(dplyr)
library(tidyr)

merged_stats <- regular_stats %>%
    select(b2, b1, cms.per.y, matches.per.y, ccf, match, num.matches.new = num.matches) %>%
    inner_join(
        original_stats %>%
            select(b2, b1, CMS, num.matches, ccf)
    , by = c("b1" = "b1", "b2" = "b2"))

qplot(CMS, cms.per.y, data = merged_stats, facets = ~match) +
    theme_bw()

qplot(num.matches, matches.per.y, data = merged_stats, facets = ~match) +
    theme_bw()

qplot(num.matches, num.matches.new, data = merged_stats, facets = ~match) +
    theme_bw()

qplot(ccf.y, ccf.x, data = merged_stats, facets = ~match) +
    theme_bw()
