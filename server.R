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
    # riskdb_path <- "risks.db"
    # riskdb_path <- "venus_expert.db"
    riskdb_path <- "venus_expert2.db"
# Declare global variables ----
    risk_fields <- c("risk_number",
                     "risk_name",
                     "risk_description",
                     "risk_impact",
                     "risk_probability"
    )
    selected_risk_fields <- c("risk_number",
                              "selected_risk_name",
                              "selected_risk_description",
                              "selected_risk_impact",
                              "selected_risk_probability")
    action_fields <- c("action_number",
                       "risk_number",
                       "action_description", 
                       "action_responsible",
                       "action_deadline")
    selected_action_fields <- c("selected_action_number",
                                "risk_number",
                                "selected_action_description",
                                "selected_action_responsible",
                                "selected_action_deadline")
# Functions for data loading ----
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
        actions <- dbGetQuery(riskdb_conn, query) # %>%
            # mutate(action_deadline = ymd(as.integer(action_deadline)))
        dbDisconnect(riskdb_conn)
        actions
    }
    risks <- load_risks()
    actions <- load_actions()
# HEATMAP ----
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
# rendering the plot (updated if either save_risk or delete is clicked)
    output$heatmap <- renderPlot({
        input$save_risk | input$delete_risk
        load_risks() %>% 
            filter(risk_probability | risk_impact %in% c(1, 2, 3, 4)) %>%
            mutate(r_color = risk_color_bw(
                risk_impact, risk_probability)) %>%
            ggplot(aes(x = risk_impact, y = risk_probability)) +
            geom_tile(data = heatmap_tiles, 
                      aes(x = impact, y = probability, 
                          fill = tile_color,
                          color = "black")) +
            # geom_point() +
            geom_text_repel(aes(label = risk_number,
                                    # glue("{risk_number} - {risk_name}"),
                                color = r_color,
                                size = 12
                                ),
                            segment.color = NA) +
            scale_x_continuous(breaks = c(0, 1, 2, 3, 4)) +
            scale_y_continuous(breaks = c(0, 1, 2, 3, 4)) +
            scale_fill_identity() +
            scale_color_identity() +
            theme_minimal() +
            labs(title = "Heatmap",
                 x = "Risk Impact",
                 y = "Risk Probability") +
            theme(legend.position = "none",
                  panel.grid = element_blank(),
                  plot.title = element_text(hjust = 0.5))
        
    })
# Top 5 risks table ----
    output$top5risks <- renderDT(
        rownames = FALSE,
        escape = TRUE,
        options = list(pageLength = 5),
        # input$save_risk | input$delete_risk
        load_risks() %>%
            mutate(criticality = risk_probability * risk_impact) %>%
            arrange(desc(criticality)) %>%
            select(risk_number, risk_name, risk_probability, risk_impact)
            
    )
# TEXT ANALYSIS ----
    output$top10words <- renderPlot({
        input$save_risk | input$delete_risk | 
            input$save_action | input$delete_action
# Word count ----
        risks <- load_risks()
        actions <- load_actions()
        risks_tidy <- risks$risk_description %>% 
            # I had to add as.tibble as the loading provided only a chr vector
            as.tibble() %>% 
            mutate(line = row_number()) %>%
            # and now amazing: converting everything into words!!!
            unnest_tokens(input = value, output = word) %>%
            filter(!word %in% get_stopwords(language = "en")$word,
                   !is.na(word)) %>%
            select(word)
        actions_tidy <- actions$action_description %>% 
            # I had to add as.tibble as the loading provided only a chr vector
            as.tibble() %>% 
            mutate(line = row_number()) %>%
            # and now amazing: converting everything into words!!!
            unnest_tokens(input = value, output = word) %>%
            filter(!word %in% get_stopwords(language = "en")$word,
                   !is.na(word)) %>%
            select(word)
        risks_actions_tidy <- bind_rows(
            risks_tidy, actions_tidy
        )
        risks_actions_tidy %>%
            count(word, sort = TRUE) %>%
            head(10) %>%
            mutate(word = fct_inorder(word)) %>%
            mutate(word = fct_rev(word)) %>%
# Plot ----
            ggplot(aes(y = n, x = word)) +
            geom_col(fill = "darkred", alpha = 0.7) +
            scale_y_continuous(breaks = 
                                   seq(0,nrow(risks_actions_tidy), 5)) +
            coord_flip() +
            theme_minimal() +
            theme(
                axis.text.y = element_text(size = 12)
            ) +
            labs(title = "Top ten words on risk descriptions",
                 subtitle = "",
                 y = "Number of occurrences",
                 x = "")
    })
# RISK MANAGEMENT ----
# Risk name ----
# Display the name of the selected risk
    output$risk_name <- renderUI({
        textInput(inputId = "selected_risk_name",
                  label = "Risk Name",
                  width = 600,
                  value = get_risk_name())
    })
    get_risk_name <- reactive({
        get_risk_data(input$risk_number, risk_fields) %>% pull(risk_name)
    })
# Get risk data ----
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
# Risk number ----
# Display the number of the last action for the selected risk
    output$action_number <- renderUI({
        input$new_action | input$delete_action
        selectInput(inputId = "selected_action_number",
                    label = "Action number",
                    width = 160,
                    choices = load_actions_by_risk() %>% pull(action_number),
                    selected = load_actions_by_risk() %>% 
                        pull(action_number) %>% last())
    })
    load_actions_by_risk <- function() {
        riskdb_conn <- dbConnect(SQLite(), riskdb_path)
        query <- glue(
            "SELECT * FROM actions WHERE risk_number = {input$risk_number}")
        actions <- dbGetQuery(riskdb_conn, query) %>%
            mutate(action_deadline = ymd(as.integer(action_deadline)))
        dbDisconnect(riskdb_conn)
        actions
    }
# Risk description ----
# Display the description of the selected risk
    output$risk_description <- renderUI({
        textAreaInput(
            width = 600,
            height = 200,
            inputId = "selected_risk_description",
            label = "Risk Description",
            value = get_risk_description())
    })
    get_risk_description <- reactive({
            get_risk_data(input$risk_number, risk_fields) %>% pull(risk_description)
    })
# Risk impact ----
# Display the impact of the selected risk
    output$risk_impact <- renderUI({
        numericInput(inputId = "selected_risk_impact",
                     label = "Impact",
                     width = 80,
                     value = get_risk_impact())
    })
    get_risk_impact <- reactive({
        get_risk_data(input$risk_number, risk_fields) %>% pull(risk_impact)
    })
# Risk probability ----
# Display the probability of the selected risk
    output$risk_probability <- renderUI({
        numericInput(inputId = "selected_risk_probability",
                     label = "Probability",
                     width = 80,
                     value = get_risk_probability())
    })    
    get_risk_probability <- reactive({
        get_risk_data(input$risk_number, risk_fields) %>% pull(risk_probability)
    })   
# Risk description ----
# Display the description of the selected action
    output$action_description <- renderUI({
        textAreaInput(
            width = 600,
            height = 200,
            inputId = "selected_action_description",
            label = "Action Description",
            value = get_action_description())
    })
    get_action_description <- reactive({
        get_action_data(input$selected_action_number, action_fields) %>% 
            pull(action_description)
    })
# Risk responsible ----
# Display the responsible of the selected action
    output$action_responsible <- renderUI({
        textInput(
            width = 160,
            inputId = "selected_action_responsible",
            label = "Action responsible",
            value = get_action_responsible())
    })
    get_action_responsible <- reactive({
        get_action_data(input$selected_action_number, action_fields) %>%
            pull(action_responsible)
        
    })
# Action deadline ----
# Display the deadline for the selected action
    output$action_deadline <- renderUI({
        textInput(
            # min = "2020-10-01",
            width = 160,
            inputId = "selected_action_deadline",
            label = "Action deadline",
            value = get_action_deadline())
    })
    get_action_deadline <- reactive({
        get_action_data(input$selected_action_number, action_fields) %>%
            pull(action_deadline)
        
    })
# Get actions data ----
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
# Save risk ----
# Save the selected risk
    observeEvent(input$save_risk, {
        save_risk(aggregate_risk_data())
    })
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
# Delete risk ----
# Delete the selected risk
    observeEvent(input$delete_risk, {
        delete_risk()
    })
    delete_risk <- function() {
        riskdb_conn <- dbConnect(SQLite(), riskdb_path)
        delete_risk_query <- sprintf("DELETE FROM risks WHERE risk_number = %s",
                                 input$risk_number)
        dbGetQuery(riskdb_conn, delete_risk_query)
    }
# Create action ----
# Create a new action
    observeEvent(input$new_action, {
        create_action()
    })
    create_action <- function() {
        new_action_nr <- load_actions() %>% pull(action_number) %>% last() + 1
        riskdb_conn <- dbConnect(SQLite(), riskdb_path)
        query <- glue(
            "INSERT INTO actions (action_number, risk_number) 
             VALUES ({new_action_nr}, {input$risk_number})")
        dbGetQuery(riskdb_conn, query)
        dbDisconnect(riskdb_conn)
    }
# Save action ----
# Save the selected action
    observeEvent(input$save_action, {
        save_action(aggregate_action_data())
        # actions <- load_actions()
    })
    aggregate_action_data <- reactive({
        aggregated_actions <- sapply(selected_action_fields, 
                                   function(x) {input[[x]]})
        aggregated_actions
    })
    save_action <- function(x) {
        riskdb_conn <- dbConnect(SQLite(), riskdb_path)
        # Construct the update query by looping over the data fields
        names(x) <- c("action_number",
                      "risk_number",
                      "action_description",
                      "action_responsible",
                      "action_deadline")
        if (input$selected_action_number %in% (load_actions() %>% pull(action_number))) {
            delete_action()
        }
        query <- sprintf(
            "INSERT INTO actions (%s) VALUES ('%s')",
            paste(names(x), collapse = ", "),
            paste(x, collapse = "', '"))
        dbGetQuery(riskdb_conn, query)
        dbDisconnect(riskdb_conn)
    }

# Deleted action ----
# Delete the selected action
    observeEvent(input$delete_action, {
        delete_action()
    })
    delete_action <- function() {
        riskdb_conn <- dbConnect(SQLite(), riskdb_path)
        delete_action_query <- 
            sprintf("DELETE FROM actions WHERE action_number = %s",
                                     input$selected_action_number)
        dbGetQuery(riskdb_conn, delete_action_query)
    }

# Downloads ----
    
    
    output$downloadRisks <- downloadHandler(

        # contentType = "text/csv",
        filename = function() {
            paste(Sys.Date(), "-risksbkp", ".xlsx", sep = "")
        },
        content = function(file) {
            openxlsx::write.xlsx(risks, file, sheetname = "Risks")
        }
    )
    output$downloadActions <- downloadHandler(
        # contentType = "text/csv",
        filename = function() {
            paste(Sys.Date(), "-actionsbkp", ".xlsx", sep = "")
        },
        content = function(file) {
            openxlsx::write.xlsx(actions, file, sheetname = "Actions")
        }
    )
}    












