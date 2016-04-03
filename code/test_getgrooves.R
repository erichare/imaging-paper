library(x3pr)
library(x3prplus)
library(ggplot2)

path1 <- "~/Downloads/Hamby Set 44/known/"
for (file in dir(path1)) {
    mybullet <- get_crosscut(file.path(path1, file), x = 100, transpose = TRUE)
    x <- get_grooves(mybullet, groove_cutoff = 500)
    cat(file, "\n")
    print(x$plot)
    #my.loess <- fit_loess(mybullet, x)
    #print(my.loess$resid)
    value <- readline("Press Enter to Continue, or q to Quit")
    if (value == "q") break;
}

path2 <- "~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2/"
for (file in dir(path2)) {
    mybullet <- get_crosscut(file.path(path2, file), x = 100)
    x <- get_grooves(mybullet, smoothfactor = 15)
    cat(file, "\n")
    print(x$plot)
    #my.loess <- fit_loess(mybullet, x)
    #print(my.loess$resid)
    value <- readline("Press Enter to Continue, or q to Quit")
    if (value == "q") break;
}
