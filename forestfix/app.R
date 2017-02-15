library(shiny)
library(RMySQL)
library(dplyr)
library(ggplot2)

load("ccf_set252.RData")

dbname <- "bullets"
user <- "buser"
password <- readLines("buser_pass.txt")
host <- "10.25.122.176"
port <- 3306

my_db <- src_mysql(dbname, host, port, user, password)

my_signatures <- tbl(my_db, "signatures")
my_profiles <- tbl(my_db, "profiles")

ui <- fluidPage(
   
   titlePanel("Forest Fixer"),
   
   sidebarLayout(
      sidebarPanel(
          numericInput("profile1", "Profile 1", value = 11261),
          numericInput("profile2", "Profile 2", value = 195067)
      ),
      
      mainPanel(
          plotOutput("profplot"),
          dataTableOutput("ccf")
      )
   )
)

server <- function(input, output) {
    
    prof1 <- reactive({
        my_signatures %>%
            filter(profile_id == input$profile1, run_id == 1) %>%
            collect()
    })
    
    prof2 <- reactive({
        my_signatures %>%
            filter(profile_id == input$profile2, run_id == 1) %>%
            collect()
    })
    
    mylag <- reactive({
        result <- CCFs_set252 %>%
            filter(profile1_id == input$profile1, profile2_id == input$profile2)
        return(result$lag * 1000)
    })
    
    aligned <- reactive({
        prof1_zeroed <- prof1() %>%
            mutate(y = y - min(y),
                   profile_id = factor(profile_id))
        prof2_zeroed <- prof2() %>%
            mutate(y = y - min(y)) %>%
            mutate(y = y + mylag(),
                   profile_id = factor(profile_id))

        return(rbind(prof1_zeroed, prof2_zeroed))
    })
    
    output$profplot <- renderPlot({
        ggplot(data = aligned(), aes(x = y, y = l30, colour = profile_id)) +
            geom_line() +
            theme_bw()
    })
    
    output$ccf <- renderDataTable({
        return(CCFs_set252)
    })
    
}

shinyApp(ui = ui, server = server)
