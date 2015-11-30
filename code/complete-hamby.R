library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)
library(gridExtra)

knowndatadir <- "app/images/Hamby252_3DX3P1of2/"
knowns <- file.path(knowndatadir, dir(knowndatadir, pattern="x3p"))

unknowndatadir <- "app/images/Hamby252_3DX3P2of2/"
unknowns <- file.path(unknowndatadir, dir(unknowndatadir))


###############
# can we identify the barrels the unknown bullets came from?


# match unknown land using crosscuts
ccs <- read.csv("crosscuts.csv")
for (j in 1:90) {
  
  reslist <- lapply(knowns, function(x) {
    cat(x)
    cat("\n")
    
    bulletGetMaxCMS(x, unknowns[j], 
                    crosscut = ccs$cc[which(ccs$path==x)], 
                    crosscut2 = ccs$cc[which(ccs$path==unknowns[j])], check=TRUE)
  })
  save(reslist, file=sprintf("data/unkn%d.RData", j))
}


# match first unknown land
for (j in 1:90) {

reslist <- lapply(knowns, function(x) {
  cat(x)
  cat("\n")
  bulletGetMaxCMS(x, unknowns[j], check=TRUE)
})
save(reslist, file=sprintf("data/unkn%d.RData", j))
}

for (j in 1:90) {
  reslist <- lapply(knowns, function(x) {
    cat(x)
    cat("\n")
    bulletGetMaxCMS(x, unknowns[j])
  })
  save(reslist, file=sprintf("data/unkn%d.RData", j))
}


for (j in 2:90) {
  reslist <- lapply(knowns, function(x) {
    cat(x)
    cat("\n")
    bulletGetMaxCMS(x, unknowns[j], crosscut = 112.5)
  })
  save(reslist, file=sprintf("data/unkn%d.RData", j))
}


cmsdist <- sapply(reslist, function(x) x$maxCMS)
qplot(cmsdist, geom="bar") + theme_bw() + xlab("Number of CMS")
res <- reslist[[which.max(cmsdist)]]  


  ggplot() +
    theme_bw() + 
    geom_rect(aes(xmin=miny, xmax=maxy), ymin=-6, ymax=5, fill="grey90", data=res$lines) +
    geom_line(aes(x = y, y = l30, colour = bullet),  data = res$bullets) +
    geom_hline(yintercept = res$threshold) +
    geom_hline(yintercept = - res$threshold) +
    scale_colour_brewer(palette="Set1") +
    theme(legend.position = c(1,1), legend.justification=c(1,1)) + 
    ylim(c(-6,6)) +
    geom_text(aes(x = meany), y= -5.5, label= "x", data = subset(res$lines, !match)) +
    geom_text(aes(x = meany), y= -5.5, label= "o", data = subset(res$lines, match)) 
