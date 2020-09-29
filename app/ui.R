# Load packages
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

shinyUI(fluidPage(
    
    includeCSS("risk_app.css"),
    
    titlePanel("sharpRisk"),
    tabsetPanel(
        type = "tabs",
        # Heatmap ----
        tabPanel("Heatmap",
            tags$br(),
            sidebarLayout(
                sidebarPanel(
                    tags$b("Select Input"),
                    tags$hr(),
                    textInput(
                        inputId = "risk_owner",
                        label = "Risk Owner")
                ),
                mainPanel(
                    plotOutput("heatmap"))
                )
            ),
        # Text Analysis ----
        tabPanel("Text Analysis",
            tags$br(),
            sidebarLayout(
                sidebarPanel(
                    tags$b("Select Input"),
                    tags$hr(),
                ),
                mainPanel(
                    plotOutput("top10words")
                )
            )
        ),
        # Risks ----
        # tabPanel("Risks",
        #          tags$br(),
        #     sidebarLayout(
        #         sidebarPanel(
        #             tags$b("Select Input"),
        #             tags$hr(),
        #         ),
        #         mainPanel(
        #             DT::dataTableOutput("risksDT"))   
        #         )
        #     ),
        # Risk Management ----
        tabPanel("Risk Management",
            tags$br(),
            sidebarLayout(
            sidebarPanel(
                numericInput(
                    inputId = "risk_number",
                    value = 1,
                    label = "Risk number",
                    width = 100
                ),
                       tags$b("Select Input"),
                       hr(),
                       actionButton("update_risk", "Update Risk"),
                       br(), br(),
                       actionButton("delete_risk", "Delete Risk")
                       ),
            mainPanel(
                column(12,
                    column(10,
                        br(),
                        
                        uiOutput("risk_name"),
                        uiOutput("risk_description"),
                        br(),
                    ),
                    column(2,
                        br(),
                        uiOutput("risk_impact"),
                        uiOutput("risk_probability"),
                        # textAreaInput(label = "Text test",
                        #               inputId = "free_text"
                        #               ),
                        # class = "column-border",
                        # htmlOutput(outputId = "risk_description",
                        #            class = "input-field",
                        #            # inline = TRUE,
                        #            container = tags$textarea,
                        #            rows = 6, cols = 50
                        #            ),
                    ))
                
            )
            ),
            sidebarLayout(
                sidebarPanel("test1"),
                mainPanel(
                    tags$b("Actions"),
                    br(),
                    br(),
                    DT::dataTableOutput("actionsDT")
                )
            )

        )
    )
))














