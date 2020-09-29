# Load packages ----
library(tidyverse)
library(lubridate)
library(glue)
library(shiny)
library(DBI)
library(RSQLite)
library(ggrepel)
library(tidytext)
library(stopwords)
library(htmltools)

# Server function ----
server <- function(input, output) {
# Database connection ----
    riskdb_path <- "database/risks.db"
    # riskdb_path <- "../app/database/venus_expert.db"
    riskdb_conn <- dbConnect(SQLite(), dbname = riskdb_path)
# Declare global variables ----
    risk_fields <- c("risk_number",
                     "risk_name",
                     "risk_description",
                     "risk_impact",
                     "risk_probability"
    )
    action_fields <- c("action_number",
                       "risk_number",
                       "action_description", 
                       "action_responsible")
# Load risks and actions
    load_risks <- function() {
        riskdb_conn <- dbConnect(SQLite(), riskdb_path)
        query <- "SELECT * FROM risks"
        risks <- dbGetQuery(riskdb_conn, query)
        dbDisconnect(riskdb_conn)
        risks
    }
    load_actions <- function() {
        riskdb_conn <- dbConnect(SQLite(), riskdb_path)
        query <- "SELECT * FROM actions"
        actions <- dbGetQuery(riskdb_conn, query) %>%
            mutate(action_deadline = ymd(as.integer(action_deadline)))
        dbDisconnect(riskdb_conn)
        actions
    }
    load_actions_by_risk <- function() {
        riskdb_conn <- dbConnect(SQLite(), riskdb_path)
        query <- glue(
            "SELECT * FROM actions WHERE risk_number = {input$risk_number}")
        actions <- dbGetQuery(riskdb_conn, query) %>%
            mutate(action_deadline = ymd(as.integer(action_deadline)))
        dbDisconnect(riskdb_conn)
        actions
    }
# Heatmap ----
    heatmap_tiles <- tribble(
        ~impact, ~probability, ~tile_color,
        1, 1, "yellow",
        1, 2, "yellow",
        1, 3, "yellow",
        1, 4, "orange",
        2, 1, "yellow",
        2, 2, "orange",
        2, 3, "orange",
        2, 4, "orange",
        3, 1, "orange",
        3, 2, "orange",
        3, 3, "red",
        3, 4, "red",
        4, 1, "orange",
        4, 2, "red",
        4, 3, "red",
        4, 4, "red"
    )
    # defining the risk text color function (black, red and white)
    risk_color_bw <- function(risk_impact, risk_probability) {
        r_color <- case_when(
            risk_impact == 1 & risk_probability == 1 ~ "black",
            risk_impact == 1 & risk_probability == 2 ~ "black",
            risk_impact == 1 & risk_probability == 3 ~ "black",
            risk_impact == 1 & risk_probability == 4 ~ "black",
            risk_impact == 2 & risk_probability == 1 ~ "black",
            risk_impact == 2 & risk_probability == 2 ~ "black",
            risk_impact == 2 & risk_probability == 3 ~ "black",
            risk_impact == 2 & risk_probability == 4 ~ "black",
            risk_impact == 3 & risk_probability == 1 ~ "black",
            risk_impact == 3 & risk_probability == 2 ~ "black",
            risk_impact == 3 & risk_probability == 3 ~ "white",
            risk_impact == 3 & risk_probability == 4 ~ "white",
            risk_impact == 4 & risk_probability == 1 ~ "black",
            risk_impact == 4 & risk_probability == 2 ~ "white",
            risk_impact == 4 & risk_probability == 3 ~ "white",
            risk_impact == 4 & risk_probability == 4 ~ "white",
            TRUE ~ "")
        r_color
    }
# rendering the plot (updated if either update_risk or delete is clicked)
    output$heatmap <- renderPlot({
        input$update_risk | input$delete_risk
        load_risks() %>%
            mutate(r_color = risk_color_bw(
                risk_impact, risk_probability)) %>%
            ggplot(aes(x = risk_impact, y = risk_probability)) +
            geom_tile(data = heatmap_tiles, 
                      aes(x = impact, y = probability, 
                          fill = tile_color,
                          color = "black")) +
            # geom_point() +
            geom_text_repel(aes(label = glue("{risk_number} - {risk_name}"),
                                color = r_color,
                                hjust = "left",
                                nudge_x = 1),
                            segment.color = NA) +
            scale_x_continuous(breaks = c(0, 1, 2, 3, 4),
                               limits = c(0.5, 4.5)) +
            scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                               limit = c(0.5, 4.5)) +
            scale_fill_identity() +
            scale_color_identity() +
            theme_minimal() +
            labs(x = "Risk Impact",
                 y = "Risk Probability") +
            theme(legend.position = "none",
                  panel.grid = element_blank())
        
    })
# Text Analysis -------------------------------
    output$top10words <- renderPlot({
        input$update_risk | input$delete_risk
        risks <- load_risks()
        risks_tidy <- risks$risk_description %>% 
            # I had to add as.tibble as the loading provided only a chr vector
            as.tibble() %>% 
            mutate(line = row_number()) %>%
            # and now amazing: converting everything into words!!!
            unnest_tokens(input = value, output = word) %>%
            filter(!word %in% get_stopwords(language = "en")$word) 
        risks_tidy %>%
            count(word, sort = TRUE) %>%
            head(10) %>%
            mutate(word = fct_inorder(word)) %>%
            mutate(word = fct_rev(word)) %>%
            ggplot(aes(y = n, x = word)) +
            geom_col(fill = "darkred", alpha = 0.7) +
            coord_flip() +
            theme_minimal() +
            labs(title = "Top ten words on risk descriptions",
                 subtitle = "",
                 y = "Number of occurrences",
                 x = "")
    })
# Functions for database query ----
    get_risk_data <- function(number, risk_fields) {
        riskdb_conn <- dbConnect(SQLite(), riskdb_path)
        risk_fields_str <- paste(risk_fields, collapse = ", ")
        query <- glue(
            "SELECT {risk_fields_str} FROM risks WHERE risk_number = {number}"
        )
        risk_data <- dbGetQuery(riskdb_conn, query)
        dbDisconnect(riskdb_conn)
        risk_data
    }
    get_action_data <- function(number, action_fields) {
        riskdb_conn <- dbConnect(SQLite(), riskdb_path)
        action_fields_str <- paste(action_fields, collapse = ", ")
        query <- glue(
            "SELECT {action_fields_str} FROM actions WHERE action_number = {number}"
        )
        action_data <- dbGetQuery(riskdb_conn, query)
        dbDisconnect(riskdb_conn)
        action_data
    }
# Functions for fields output in risk management form----
    get_risk_name <- reactive({
            get_risk_data(input$risk_number, risk_fields) %>% pull(risk_name)
    })
    output$risk_name <- renderUI({
        textInput(inputId = "selected_risk_name",
                  label = "Risk Name",
                  width = 600,
                  value = get_risk_name())
    })
    get_risk_description <- reactive({
            get_risk_data(input$risk_number, risk_fields) %>% pull(risk_description)
    })
    output$risk_description <- renderUI({
        textAreaInput(
            width = 600,
            height = 200,
            inputId = "selected_risk_description",
            label = "Risk Description",
            value = get_risk_description())
    })
    get_action_description <- reactive({
        get_action_data(input$selected_action_number, action_fields) %>% pull(action_description)
    })
    output$action_description <- renderUI({
        textAreaInput(
            width = 600,
            height = 200,
            inputId = "selected_action_description",
            label = "Action Description",
            value = get_action_description())
    })
    get_risk_impact <- reactive({
            get_risk_data(input$risk_number, risk_fields) %>% pull(risk_impact)
    })
    output$risk_impact <- renderUI({
        numericInput(inputId = "selected_risk_impact",
                    label = "Impact",
                    width = 80,
                    value = get_risk_impact())
    })
    get_risk_probability <- reactive({
            get_risk_data(input$risk_number, risk_fields) %>% pull(risk_probability)
    })
    output$risk_probability <- renderUI({
        numericInput(inputId = "selected_risk_probability",
                  label = "Probability",
                  width = 80,
                  value = get_risk_probability())
    })
    # save risks ---
    # aggregate risk form data
    selected_risk_fields <- c("risk_number",
                              "selected_risk_name",
                              "selected_risk_description",
                              "selected_risk_impact",
                              "selected_risk_probability")
    aggregate_risk_data <- reactive({
        aggregated_risks <- sapply(selected_risk_fields, 
                                   function(x) {input[[x]]})
        aggregated_risks
    })
    save_risk <- function(x) {
        riskdb_conn <- dbConnect(SQLite(), riskdb_path)
        # Construct the update query by looping over the data fields
        names(x) <- c("risk_number",
                                         "risk_name",
                                         "risk_description",
                                         "risk_impact",
                                         "risk_probability")
        if (input$risk_number %in% (load_risks() %>% pull(risk_number))) {
            delete_risk()
            }
        query <- sprintf(
            "INSERT INTO risks (%s) VALUES ('%s')",
            paste(names(x), collapse = ", "),
            paste(x, collapse = "', '"))
        dbGetQuery(riskdb_conn, query)
        dbDisconnect(riskdb_conn)
    }
    observeEvent(input$update_risk, {
        save_risk(aggregate_risk_data())
    })
    delete_risk <- function() {
        riskdb_conn <- dbConnect(SQLite(), riskdb_path)
        delete_risk_query <- sprintf("DELETE FROM risks WHERE risk_number = %s",
                                 input$risk_number)
        dbGetQuery(riskdb_conn, delete_risk_query)
    }
    observeEvent(input$delete_risk, {
        delete_risk()
    })
    output$action_number <- renderUI({
        selectInput(inputId = "selected_action_number",
                    label = "Action number",
                    width = 100,
                    choices = load_actions_by_risk() %>% pull(action_number),
                    selected = load_actions_by_risk() %>% 
                        pull(action_number) %>% first())
    })



    
    
    
    # output$actionsDT <- DT::renderDataTable({
    #     # input$update_action | input$delete_action
    #     load_actions_by_risk() 
    # })
}

# shinyApp(ui = htmlTemplate("index.html"), server)















