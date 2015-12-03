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
ccs <- read.csv("data/crosscuts.csv")
load("data/bullets.RData")
bPaths <- sapply(bullets, function(x) x$path)


for (j in 1:90) {
  
  reslist <- lapply(knowns, function(x) {
    cat(x)
    cat("\n")
#    browser()
    b1 <- bullets[[which(bPaths == x)]]
    b2 <- bullets[[which(bPaths == unknowns[j])]]
    
    bulletGetMaxCMSXXX(b1, b2)
  })
  save(reslist, file=sprintf("data/unkn%d.RData", j))
}



# match unknown land using crosscuts
ccs <- read.csv("data/crosscuts.csv")
for (j in 1:90) {
  
  reslist <- lapply(knowns, function(x) {
    cat(x)
    cat("\n")
    browser()
    bulletGetMaxCMS(x, unknowns[j], 
                    crosscut = ccs$cc[which(ccs$path==x)], 
                    crosscut2 = ccs$cc[which(ccs$path==unknowns[j])], check=TRUE)
  })
  save(reslist, file=sprintf("data/unkn%d.RData", j))
}

# match some known lands
br4 <- knowns[grep("Br4", knowns)]
reslist <- list()
for (j in 1) {
  res <- lapply(br4[7:12], function(x) {
    cat(x)
    cat("\n")
    
    bulletGetMaxCMS(x, br4[j], 
                    crosscut = ccs$cc[which(ccs$path==x)], 
                    crosscut2 = ccs$cc[which(ccs$path==unknowns[j])], check=TRUE)
  })
  reslist <- c(reslist, res)
}
save(reslist, file=sprintf("data/br4-%d.RData", j))

br <- knowns[grep("Br8", knowns)]
reslist <- list()
for (j in 2) {
  res <- lapply(br[7:12], function(x) {
    cat(x)
    cat("\n")
    
    bulletGetMaxCMS(x, br[j], 
                    crosscut = ccs$cc[which(ccs$path==x)], 
                    crosscut2 = ccs$cc[which(ccs$path==unknowns[j])], check=TRUE)
  })
  reslist <- c(reslist, res)
}
save(reslist, file=sprintf("data/br8-%d.RData", j))

br <- knowns[grep("Br9", knowns)]
reslist <- list()
for (j in 1) {
  res <- lapply(br[7:12], function(x) {
    cat(x)
    cat("\n")
    
    bulletGetMaxCMS(x, br[j], 
                    crosscut = ccs$cc[which(ccs$path==x)], 
                    crosscut2 = ccs$cc[which(ccs$path==unknowns[j])], check=TRUE)
  })
  reslist <- c(reslist, res)
}
cmsdist <- sapply(reslist, function(x) x$maxCMS)
qplot(cmsdist, geom="bar") + theme_bw() + xlab("Number of CMS")

save(reslist, file=sprintf("data/br9.RData", j))


br <- knowns[grep("Br10", knowns)]
reslist <- list()
for (j in 2) {
  res <- lapply(br[7:12], function(x) {
    cat(x)
    cat("\n")
    
    bulletGetMaxCMS(x, br[j], 
                    crosscut = ccs$cc[which(ccs$path==x)], 
                    crosscut2 = ccs$cc[which(ccs$path==unknowns[j])], check=TRUE)
  })
  reslist <- c(reslist, res)
}
cmsdist <- sapply(reslist, function(x) x$maxCMS)
qplot(cmsdist, geom="bar") + theme_bw() + xlab("Number of CMS")
save(reslist, file=sprintf("data/br10.RData", j))


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

  
  for ( i in 1:6) {
    res <- reslist[[i]]  
    
    ch <- scan()
    print(ggplot() +
            theme_bw() + 
            geom_rect(aes(xmin=miny, xmax=maxy), ymin=-6, ymax=5, fill="grey90", data=res$lines) +
            geom_line(aes(x = y, y = l30, colour = bullet),  data = res$bullets) +
            geom_hline(yintercept = res$threshold) +
            geom_hline(yintercept = - res$threshold) +
            scale_colour_brewer(palette="Set1") +
            theme(legend.position = c(1,1), legend.justification=c(1,1)) + 
            ylim(c(-6,6)) +
            geom_text(aes(x = meany), y= -5.5, label= "x", data = subset(res$lines, !match)) +
            geom_text(aes(x = meany), y= -5.5, label= "o", data = subset(res$lines, match)))
    
  }
  
  