library(x3pr)
library(x3prplus)
library(ggplot2)

crosscuts_44 <- read.csv("csvs/crosscuts-25-set44.csv")
apply(crosscuts_44, 1, function(row) {
    cat(row[1])
    mybullet <- get_crosscut(row[1], x = as.numeric(row[2]), transpose = TRUE)
    x <- get_grooves(mybullet, smoothfactor = 35, groove_cutoff = 500)
    cat(file, "\n")
    print(x$plot)
    #my.loess <- fit_loess(mybullet, x)
    #print(my.loess$resid)
    value <- readline("Press Enter to Continue, or q to Quit")
    if (value == "q") break;
})

crosscuts <- read.csv("csvs/crosscuts-25.csv")
apply(crosscuts, 1, function(row) {
    cat(row[1])
    mybullet <- get_crosscut(row[1], x = as.numeric(row[2]))
    x <- get_grooves(mybullet, smoothfactor = 15)
    print(x$plot)
    #my.loess <- fit_loess(mybullet, x)
    #print(my.loess$resid)
    value <- readline("Press Enter to Continue, or q to Quit")
    if (value == "q") break;    
})
