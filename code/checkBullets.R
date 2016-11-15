library(bulletr)
library(dplyr)
library(ggplot2)
library(zoo)

datadir <- "images/Hamby (2009) Barrel/bullets"
all_data <- file.path(datadir, dir(datadir))
knowns <- all_data[grep("[Bb]r[0-9].*", all_data)]
unknowns <- all_data[grep("Ukn*|br[A-Z].*", all_data)]

if (!file.exists("csvs/crosscuts.csv")) {
crosscuts <- sapply(c(knowns, unknowns), function(x) {
  cat(x, " ")
  crosscut <- bulletCheckCrossCut(x, xlimits = c(100, 400), distance = 25)
  cat(crosscut)
  cat("\n")
  crosscut
})

dframe <- data.frame(
  path = c(knowns, unknowns),
  cc = crosscuts
)

write.csv(dframe, file="csvs/crosscuts.csv", row.names=FALSE)
}
