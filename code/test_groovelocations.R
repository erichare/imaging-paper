library(x3pr)
library(x3prplus)
library(ggplot2)
library(dplyr)

ccs <- read.csv("csvs/crosscuts-25-set44.csv")
grooves <- read.csv("csvs/grooves-set44.csv")

apply(ccs, 1, function(row) {
    cat(row[1])
    
    mybullet <- get_crosscut(row[1], x = as.numeric(row[2]), transpose = TRUE)
    relevant.groove <- filter(grooves, bullet == row[1], x == mybullet$x[1])
    
    print(qplot(y, value, data = mybullet, geom = "line") +
        theme_bw() +
        geom_vline(xintercept = relevant.groove$groove_left, color = "blue") +
        geom_vline(xintercept = relevant.groove$groove_right, color = "blue"))

    value <- readline("Press Enter to Continue, g to enter groove, or q to Quit")
    if (value == "q") break;
    
    if (value == "g") {
        newvalue <- ""
        while (newvalue != "g") {
            left <- as.numeric(readline("Enter the coordinate of the left groove "))
            right <- as.numeric(readline("Enter the coordinate of the right groove "))
            
            print(qplot(y, value, data = mybullet, geom = "line") +
                theme_bw() +
                geom_vline(xintercept = left, color = "blue") +
                geom_vline(xintercept = right, color = "blue"))
            
            newvalue <- readline("Press g to confirm, or any other key to try again")
        }
        
        grooves$groove_left[grooves$bullet == row[1] & grooves$x == mybullet$x[1]] <<- left
        grooves$groove_right[grooves$bullet == row[1] & grooves$x == mybullet$x[1]] <<- right
        grooves$manual[grooves$bullet == row[1] & grooves$x == mybullet$x[1]] <<- TRUE
    }
})

write.csv(grooves, file = "csvs/grooves-set44.csv", row.names = FALSE)
