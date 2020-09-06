# Load packages
library(tidyverse)
library(shiny)
library(DBI)
library(RSQLite)
library(ggrepel)

# USER INTERFACE ----
ui <- fluidPage(

    titlePanel("sharpRisk"),
    tabsetPanel(type = "tabs",
        tabPanel("Heatmap",
                 plotOutput("heatmap")
        ),
        tabPanel("Risk List",
            DT::dataTableOutput("risksDT")
        ), # end tabPanel "Risk List"
        
        tabPanel("Add Risks",
                 textInput("risk_number", "Risk Number"),
                 textInput("risk_name", "Risk Name"),
                 selectInput(inputId = "risk_impact",
                             label = "Risk Impact",
                             choices = c(1, 2, 3, 4), 
                             selected = 4, 
                             ),
                 selectInput(inputId = "risk_probability",
                             label = "Risk Probability",
                             choices = c(1, 2, 3, 4), 
                             selected = 4, 
                 ),
                 textInput(inputId = "risk_description",
                           label = "Risk description"),
                 actionButton("submit", "Submit"),
                 tags$hr(),                
                 actionButton("delete_risk", "Delete Risk")
                 ),
        tabPanel("Dashboard")
    ) # end tabsetPanel
)

# BACKEND ----

server <- function(input, output) {

# connect to database ----
    riskdb_path = "database/risks.db"
    riskdb_conn = dbConnect(SQLite(), dbname = riskdb_path)

# load risks ----
    load_risks <- function() {
        riskdb_conn <- dbConnect(SQLite(), riskdb_path)
        query <- sprintf("SELECT * FROM risks")
        risks <- dbGetQuery(riskdb_conn, query)
        dbDisconnect(riskdb_conn)
        risks
    }
    
# aggregate risk form data

    aggregate_risk_data <- reactive({
        fields <- c("risk_name", 
                    "risk_number", 
                    "risk_impact", 
                    "risk_probability",
                    "risk_description")
        aggregated_risks <- sapply(fields, function(x) input[[x]])
        aggregated_risks
    })
    
# save risk sql instruction ----
    save_risk <- function(x) {
        riskdb_conn <- dbConnect(SQLite(), riskdb_path)
        # Construct the update query by looping over the data fields
        query <- sprintf(
            "INSERT INTO risks (%s) VALUES ('%s')",
            paste(names(x), collapse = ", "),
            paste(x, collapse = "', '"))
        dbGetQuery(riskdb_conn, query)
        dbDisconnect(riskdb_conn)
    }
    
# save risk data on click ----
    observeEvent(input$submit, {
        save_risk(aggregate_risk_data())
    })

# delete risk on click ---- 
    observeEvent(input$delete_risk, {
         riskdb_conn <- dbConnect(SQLite(), riskdb_path)
        delete_risk_query <- sprintf(
            "DELETE FROM risks WHERE risk_number = %s",
            input$risk_number)
        dbGetQuery(riskdb_conn, delete_risk_query)
        dbDisconnect(riskdb_conn)
    })
    

# risk table ----
# (update if either submit or delete is clicked)
    output$risksDT <- DT::renderDataTable({
        input$submit | input$delete_risk
        load_risks()
    })   

# risk color function ---
    risk_color <- function(risk_impact, risk_probability) {
        r_color <- case_when(
            risk_impact == 1 & risk_probability == 1 ~ "yellow",
            risk_impact == 1 & risk_probability == 2 ~ "yellow",
            risk_impact == 1 & risk_probability == 3 ~ "orange",
            risk_impact == 1 & risk_probability == 4 ~ "orange",
            risk_impact == 2 & risk_probability == 1 ~ "yellow",
            risk_impact == 2 & risk_probability == 2 ~ "orange",
            risk_impact == 2 & risk_probability == 3 ~ "orange",
            risk_impact == 2 & risk_probability == 4 ~ "red",
            risk_impact == 3 & risk_probability == 1 ~ "yellow",
            risk_impact == 3 & risk_probability == 2 ~ "orange",
            risk_impact == 3 & risk_probability == 3 ~ "red",
            risk_impact == 3 & risk_probability == 4 ~ "red",
            risk_impact == 4 & risk_probability == 1 ~ "orange",
            risk_impact == 4 & risk_probability == 2 ~ "orange",
            risk_impact == 4 & risk_probability == 3 ~ "red",
            risk_impact == 4 & risk_probability == 4 ~ "red",
            TRUE ~ "")
        r_color
    }
    
# heatmap plot ----
    output$heatmap <- renderPlot({
        input$submit | input$delete_risk
        load_risks() %>%
        # calculating the color for each risk and adding it into a new column
        mutate(r_color = risk_color(risk_impact, risk_probability)) %>%
        ggplot(aes(x = risk_impact, y = risk_probability, color = r_color)) +
            geom_point() +
            geom_text_repel(aes(label = risk_name,
                                hjust = "left",
                                nudge_x = 1)) +
            # forcing the color aesthetics into the color scale
            scale_color_identity() +
            scale_x_continuous(breaks = c(0, 1, 2, 3, 4),
                               limits = c(-.5, 4.5)) +
            scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                               limit = c(-0.5, 4.5)) +
            theme_minimal() +
            labs(x = "Risk Impact",
                 y = "Risk Probability")
    })

} # end of server function

# run app ----
shinyApp(ui = ui, server = server)
