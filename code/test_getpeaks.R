library(x3pr)
library(x3prplus)
library(ggplot2)

plot_3d_land("images/Cary Persistence/bullets/CWBLT0001-1.x3p")

for (file in dir("images/Cary Persistence/bullets")) {
    mybullet <- get_crosscut(file.path("images/Cary Persistence/bullets", file), x = 125)
    x <- get_grooves(mybullet)
    cat(file, "\n")
    my.loess <- fit_loess(mybullet, x)
    result <- get_peaks(my.loess$data, smoothfactor = 5, window = FALSE, striae = FALSE)
    print(result$plot)
    value <- readline("Press Enter to Continue, or q to Quit")
    if (value == "q") break;
}
