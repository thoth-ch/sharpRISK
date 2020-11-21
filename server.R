# **** BACKEND APPLICATION **** ----
shinyServer(function(input, output) {
  
  # Helper files and Database connection ----
  riskdb_path <- "tridel.db"
  source_python("helper.py")
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
                     "action_deadline",
                     "action_status")
  selected_action_fields <- c("selected_action_number",
                              "risk_number",
                              "selected_action_description",
                              "selected_action_responsible",
                              "selected_action_deadline",
                              "selected_action_status")
  # Functions for data loading ----
  load_risks <- function() {
    riskdb_conn <- dbConnect(SQLite(), riskdb_path)
    query <- "SELECT * FROM risks"
    risks <- dbGetQuery(riskdb_conn, query)
    dbDisconnect(riskdb_conn)
    risks
  }
  # load_risks_reactive <- reactive({
  #   input$save_risk | input$delete_risk
  #   riskdb_conn <- dbConnect(SQLite(), riskdb_path)
  #   query <- "SELECT * FROM risks"
  #   risks_reactive <- dbGetQuery(riskdb_conn, query)
  #   dbDisconnect(riskdb_conn)
  #   risks_reactive
  # })
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
  # Risk Heatmap ----
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
      labs(
        # title = "Heatmap",
        x = "Risk Impact",
        y = "Risk Probability") +
      theme(legend.position = "none",
            panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"))
    
  })
  # Top 5 risks table ----
  output$top5risks <- renderDT(
    rownames = FALSE,
    escape = TRUE,
    options = list(pageLength = 5),
    # load_risks() %>%
    load_risks() %>%
      mutate(criticality = risk_probability * risk_impact) %>%
      arrange(desc(criticality)) %>%
      select(risk_number, risk_name, risk_probability, risk_impact)
  )

# TEXT ANALYSIS ----
  output$top10words <- renderPlot({
    input$save_risk | input$delete_risk | 
      input$save_action | input$delete_action
    ## Word count ----
    risks <- load_risks()
    actions <- load_actions()
    risks_tidy <- risks$risk_description %>% 
      # as_tibble required as the loading provided only a chr vector
      as_tibble() %>% 
      mutate(line = row_number()) %>%
      # converting everything into words
      unnest_tokens(input = value, output = word) %>%
      filter(!word %in% get_stopwords(language = "en")$word,
             !is.na(word)) %>%
      select(word)
    actions_tidy <- actions$action_description %>% 
      # as_tibble required as the loading provided only a chr vector
      as_tibble() %>% 
      mutate(line = row_number()) %>%
      # converting everything into words
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
      labs(title = "Top ten words",
           subtitle = "",
           y = "Number of occurrences",
           x = "")
  })
  ## Correlated words ----
  output$actions_corr <- renderPlot({
    actions_tidy <- actions %>% 
      select(risk_number, action_description) %>%
      as_tibble() %>% 
      unnest_tokens(input = action_description, output = word) %>%
      filter(!word %in% get_stopwords(language = "fr")$word,
             !is.na(word))
    actions_cors <- actions_tidy %>% 
      add_count(word) %>% 
      filter(n > stats::quantile(n, 0.7)) %>% 
      pairwise_cor(word, risk_number, sort = TRUE)
    
    set.seed(123)
    
    actions_graph <- actions_cors %>%
      filter(correlation > 0.5,
             !str_detect(item1, "\\d"),
             !str_detect(item2, "\\d")) %>% 
      graph_from_data_frame()
    
    ## with ggraph
    actions_graph %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(aes(label = name), repel = TRUE) +
      theme_void() + 
      labs(x = "",
           y = "",
           title = "Commonly Occuring Correlated Words",
           subtitle = "Per risk correlation higher than 0.5")
    
    ## with forceNetworks
    # graph <- as_tbl_graph(actions_graph) %>%
    #   activate(nodes) %>% 
    #   mutate(group = group_optimal())
    # nodes_tibble <- graph %>%
    #   activate(nodes) %>%
    #   as_tibble()
    # edges_tibble <- graph %>%
    #   activate(edges) %>%
    #   as_tibble() %>%
    #   mutate_all(~ . - 1)
    # forceNetwork(Links = edges_tibble,
    #              Nodes = nodes_tibble,
    #              Source = "from",
    #              Target = "to",
    #              NodeID = "name",
    #              Group = "group",
    #              opacity = 0.8,
    #              linkColour = "#EEEEEE",
    #              zoom = TRUE,
    #              legend = FALSE,
    #              fontSize = 12,
    #              bounded = TRUE)
  })
  
  
  # RISK MANAGEMENT ----
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
  # Display the number of the last risk for the selected risk
  output$risk_number <- renderUI({
    input$new_risk| input$delete_risk
    selectInput(inputId = "risk_number",
                label = "Risk number",
                
                width = 160,
                choices = load_risks() %>% pull(risk_number),
                selected = load_risks() %>% 
                  pull(risk_number) %>% last())
  })
  # load_actions_by_risk <- function() {
  #   riskdb_conn <- dbConnect(SQLite(), riskdb_path)
  #   query <- glue(
  #     "SELECT * FROM actions WHERE risk_number = {input$risk_number}")
  #   actions <- dbGetQuery(riskdb_conn, query) %>%
  #     mutate(action_deadline = ymd(as.integer(action_deadline)))
  #   dbDisconnect(riskdb_conn)
  #   actions
  # }
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
  # Risk description ----
  # Display the description of the selected risk
  output$risk_description <- renderUI({
    textAreaInput(
      width = 600,
      height = 150,
      inputId = "selected_risk_description",
      label = "Risk Description",
      value = get_risk_description())
  })
  get_risk_description <- reactive({
    get_risk_data(input$risk_number, risk_fields) %>% pull(risk_description)
  })
  # Risk impact ----
  # Display the impact of the selected risk
  # output$risk_impact <- renderUI({
  #   numericInput(inputId = "selected_risk_impact",
  #                label = "Impact",
  #                width = 80,
  #                value = get_risk_impact())
  # })
  output$risk_impact_slider <- renderUI({
    sliderInput(inputId = "selected_risk_impact",
                label = "Impact", min = 1, max = 4,
                width = 140,
                value = get_risk_impact()
    )
  })
  get_risk_impact <- reactive({
    get_risk_data(input$risk_number, risk_fields) %>% pull(risk_impact)
  })
  # Risk probability ----
  # Display the probability of the selected risk
  # output$risk_probability <- renderUI({
  #   numericInput(inputId = "selected_risk_probability",
  #                label = "Probability",
  #                width = 80,
  #                value = get_risk_probability())
  # })
  output$risk_probability_slider <- renderUI({
    sliderInput(inputId = "selected_risk_probability",
                label = "Probability", min = 1, max = 4,
                width = 140,
                value = get_risk_probability()
    )
  })
  get_risk_probability <- reactive({
    get_risk_data(input$risk_number, risk_fields) %>% pull(risk_probability)
  })
  # Create risk ----
  # Create a new risk
  observeEvent(input$new_risk, {
    create_risk()
  })
  create_risk <- function() {
    new_risk_nr <- load_risks() %>% pull(risk_number) %>% last() %>% as.numeric() %>% + 1
    riskdb_conn <- dbConnect(SQLite(), riskdb_path)
    query <- glue(
      "INSERT INTO risks (risk_number) 
      VALUES ({new_risk_nr})")
    dbGetQuery(riskdb_conn, query)
    dbDisconnect(riskdb_conn)
  }
  # Save risk ----
  # Save the selected risk
  observeEvent(input$save_risk, {
    save_risk(aggregate_risk_data())
    showNotification(type = "message", "Risk saved.", closeButton = FALSE)
  })
  aggregate_risk_data <- reactive({
    aggregated_risks <- sapply(selected_risk_fields, 
                               function(x) {input[[x]]})
    aggregated_risks["selected_risk_name"] <- str_replace_all(
      aggregated_risks["selected_risk_name"], pattern = "'", replacement = "´")
    aggregated_risks["selected_risk_description"] <- str_replace_all(
      aggregated_risks["selected_risk_description"], pattern = "'", replacement = "´")
    aggregated_risks
  })
  save_risk <- function(x) {
    riskdb_conn <- dbConnect(SQLite(), riskdb_path)
    if (input$risk_number %in% (load_risks() %>% pull(risk_number))) {
      delete_risk()
    }
    # Construct the update query by looping over the data fields
    names(x) <- c("risk_number",
                  "risk_name",
                  "risk_description",
                  "risk_impact",
                  "risk_probability")
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
    showNotification(type = "warning", "Risk deleted.", closeButton = FALSE)
  })
  delete_risk <- function() {
    riskdb_conn <- dbConnect(SQLite(), riskdb_path)
    delete_risk_query <- sprintf("DELETE FROM risks WHERE risk_number = %s",
                                 input$risk_number)
    dbGetQuery(riskdb_conn, delete_risk_query)
  }
  # Get action data ----
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
  # Action number ----
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
    actions <- dbGetQuery(riskdb_conn, query) #%>%
    # mutate(action_deadline = ymd(as.integer(action_deadline)))
    dbDisconnect(riskdb_conn)
    actions
  }
  # Action description ----
  # Display the description of the selected action
  output$action_description <- renderUI({
    textAreaInput(
      width = 600,
      height = 150,
      inputId = "selected_action_description",
      label = "Action Description",
      value = get_action_description())
  })
  get_action_description <- reactive({
    validate(need(input$selected_action_number, message = "Click New Action button to create an action"))
    get_action_data(input$selected_action_number, action_fields) %>% 
      pull(action_description)
  })
  # Action responsible ----
  # Display the responsible of the selected action
  output$action_responsible <- renderUI({
    textInput(
      width = 160,
      inputId = "selected_action_responsible",
      label = "Action responsible",
      value = get_action_responsible())
  })
  get_action_responsible <- reactive({
    req(input$selected_action_number)
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
    req(input$selected_action_number)
    get_action_data(input$selected_action_number, action_fields) %>%
      pull(action_deadline)
    
  })
  # Action status ----
  output$action_status <- renderUI({
    selectInput(
      width = 160,
      inputId = "selected_action_status",
      label = "Action Status",
      choices = c("Not-started", "On-going", "Late", "Done"),
      selected = get_action_status())
  })
  get_action_status <- reactive({
    req(input$selected_action_number)
    get_action_data(input$selected_action_number, action_fields) %>%
      pull(action_status)
  })
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
    showNotification(type = "message", "Action saved.", closeButton = FALSE)
  })
  aggregate_action_data <- reactive({
    aggregated_actions <- sapply(selected_action_fields, 
                                 function(x) {input[[x]]})
    aggregated_actions[3] <- str_replace_all(
      aggregated_actions[3], pattern = "'", replacement = "´")
    aggregated_actions
  })
  save_action <- function(x) {
    riskdb_conn <- dbConnect(SQLite(), riskdb_path)
    # Construct the update query by looping over the data fields
    names(x) <- c("action_number",
                  "risk_number",
                  "action_description",
                  "action_responsible",
                  "action_deadline",
                  "action_status")
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
  # Delete action ----
  # Delete the selected action
  observeEvent(input$delete_action, {
    req(input$selected_action_number)
    delete_action()
    showNotification(type = "warning", "Action deleted.", closeButton = FALSE)
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
  # Outstanding actions ----
  output$action_responsible2 <- renderUI({
    # input$new_action | input$delete_action
    selectInput(inputId = "selected_action_responsible2",
                label = "Action responsible",
                width = 160,
                choices = load_actions() %>% pull(action_responsible)
    )
  })
  actions_by_responsible <- function(action_responsible) {
    riskdb_conn <- dbConnect(SQLite(), riskdb_path)
    query <- glue(
      "
      SELECT actions.risk_number, risks.risk_name, actions.action_number, actions.action_description, actions.action_status
      FROM actions
      INNER JOIN risks
      ON actions.risk_number = risks.risk_number
      WHERE actions.action_responsible = '{action_responsible}'
      "
    )
    actions_by_responsible <- dbGetQuery(riskdb_conn, query)
    dbDisconnect(riskdb_conn)
    actions_by_responsible
  }
  output$actions_by_responsible <- renderDT(
    rownames = FALSE,
    escape = TRUE,
    options = list(pageLength = 5),
    actions_by_responsible(input$selected_action_responsible2)
  )
  # Actions by responsible plot
  output$action_resp_count <- renderPlot({
    action_responsibles <- unlist(action_responsibles_py) %>%
      as_tibble_col(column_name = "Responsible")

    action_responsibles %>%
      mutate(Responsible = fct_infreq(Responsible)) %>%
      ggplot(aes(x = Responsible)) +
      geom_bar(width = 0.5, fill = "cadetblue4") +
      theme_light() +
      labs(title = "Count of actions by responsible")

  })
  
})
