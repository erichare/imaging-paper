library(shiny)
library(RMySQL)
library(dplyr)
library(tidyr)
library(ggplot2)
library(bulletr)
library(plotly)

dbname <- "bullets"
user <- "buser"
password <- "TihrcdTJn#7pWv"
host <- "127.0.0.1"

con <- dbConnect(MySQL(), user = user, password = password,
                 dbname = dbname, host = host)

bullet_choices <- dbGetQuery(con, "SELECT id,name FROM metadata_bullet")
bullet_choices_vec <- bullet_choices$id
names(bullet_choices_vec) <- bullet_choices$name

ui <- fluidPage(
   
   titlePanel("Bullet Database"),
   
   sidebarLayout(
      sidebarPanel(
          selectInput("land", "Bullet Land", choices = bullet_choices_vec),
          numericInput("crosscut", "Crosscut", min = 0, max = 500, value = 0)
      ),
      
      mainPanel(
          tableOutput("metadata"),
          tableOutput("profile_metadata"),
          plotOutput("profile"),
          plotOutput("signature")
          #plotlyOutput("viewland")
      )
   )
)

server <- function(input, output, session) {
    bullet_metadata <- reactive({
        result <- dbGetQuery(con, paste0("SELECT * FROM metadata_bullet WHERE id = ", input$land, ""))
        updateNumericInput(session, "crosscut", max = result$num_profiles[1])
        
        return(result)
    })
    
    observe({
        result <- dbGetQuery(con, paste0("SELECT * FROM metadata_bullet_derived WHERE id = ", input$land, ""))
        updateNumericInput(session, "crosscut", value = result$crosscut[1])
    })
    
    bullet_metadata_derived <- reactive({
        result <- dbGetQuery(con, paste0("SELECT * FROM metadata_bullet_derived WHERE id = ", input$land, ""))
        #updateNumericInput(session, "crosscut", value = result$crosscut[1])
        
        return(result)
    })
    
    output$metadata <- renderTable({
        return(bullet_metadata())
    })
    
    mybullet <- reactive({
        mymet <- as.list(bullet_metadata())[-(1:2)]
        
        bullet_data <- dbGetQuery(con, paste0("SELECT * FROM data WHERE bullet_id = ", input$land, ""))
        attr(bullet_data, "info") <- mymet
        
        unfort <- unfortify_x3p(bullet_data)
        
        return(unfort)
    })
    
    myprofile <- reactive({
        bullet_prof <- dbGetQuery(con, paste0("SELECT * FROM data WHERE bullet_id = ", input$land, " AND x = ", input$crosscut))

        return(bullet_prof %>% select(x, y, value))
    })
    
    profile_metadata_derived <- reactive({
        result <- dbGetQuery(con, paste0("SELECT * FROM metadata_profile_derived WHERE bullet_id = ", input$land, " AND x = ", input$crosscut))
        #updateNumericInput(session, "crosscut", value = result$crosscut[1])
        
        return(result)
    })
    
    output$profile_metadata <- renderTable({
        return(profile_metadata_derived())
    })
    
    output$profile <- renderPlot({
        ggplot(data = myprofile(), aes(x = y, y = value)) +
            geom_line() +
            theme_bw() +
            geom_vline(xintercept = profile_metadata_derived()$groove_left_pred[1], colour = "blue") +
            geom_vline(xintercept = profile_metadata_derived()$groove_right_pred[1], colour = "blue")
    })
    
    mysignature <- reactive({
        bullet_sig <- dbGetQuery(con, paste0("SELECT * FROM signatures WHERE bullet_id = ", input$land))
        
        return(bullet_sig)
    })
    
    output$signature <- renderPlot({
        ggplot(data = mysignature(), aes(x = y, y = l30)) +
            geom_line() +
            theme_bw() +
            ggtitle(paste("Signature at x =", bullet_metadata_derived()$crosscut[1]))
    })
    
    output$viewland <- renderPlotly({
        plot_3d_land(bullet = mybullet())
    })
}

shinyApp(ui = ui, server = server)
