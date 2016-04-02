library(x3pr)
library(x3prplus)
library(ggplot2)

for (file in dir("~/Downloads/Hamby Set 44/known/")) {
    mybullet <- get_crosscut(file.path("~/Downloads/Hamby Set 44/known/", file), x = 100, transpose = TRUE)
    x <- get_grooves(mybullet)
    cat(file, "\n")
    print(x$plot)
    #my.loess <- fit_loess(mybullet, x)
    #print(my.loess$resid)
    value <- readline("Press Enter to Continue, or q to Quit")
    if (value == "q") break;
}
