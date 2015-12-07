library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(zoo)

knowndatadir <- "app/images/Hamby252_3DX3P1of2/"
knowns <- file.path(knowndatadir, dir(knowndatadir, pattern="x3p"))

unknowndatadir <- "app/images/Hamby252_3DX3P2of2/"
unknowns <- file.path(unknowndatadir, dir(unknowndatadir))

span <- 25
dataStr <- sprintf("data-%d", span)

###############
# can we identify the barrels the unknown bullets came from?


# match unknown land using crosscuts
ccs <- read.csv(file.path(dataStr, "crosscuts.csv"))

for (j in 1:90) {
  
  reslist <- lapply(knowns, function(x) {
    cat(x)
    cat("\n")

    cc1 <- ccs$cc[which(ccs$path == x)]
    cc2 <- ccs$cc[which(ccs$path == unknowns[j])]

    bulletGetMaxCMSXXX(x, unknowns[j], cc1, cc2, span=span)
  })
  save(reslist, file=file.path(dataStr, sprintf("unkn%d.RData", j)))
}


for (j in 1:90) {
  
  reslist <- lapply(knowns, function(x) {
    cat(x)
    cat("\n")
    #    browser()
    cc1 <- ccs$cc[which(ccs$path == x)]
    cc2 <- ccs$cc[which(ccs$path == unknowns[j])]
    
    bulletGetMaxCMSXXX(x, unknowns[j], cc1, cc2, span=25)
  })
  save(reslist, file=sprintf("data-25/unkn%d.RData", j))
}


  