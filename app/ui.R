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
        tabPanel("Criticality analysis",
            tags$br(),
            sidebarLayout(
                sidebarPanel(width = 2,
                    textInput(
                        inputId = "deparment",
                        label = "Department")
                ),
                mainPanel(
                    plotOutput("heatmap"))
                )
            ),
        # Text Analysis ----
        tabPanel("Text Analysis",
            tags$br(),
            sidebarLayout(
                sidebarPanel(width = 2,
                    textInput(
                        inputId = "deparment",
                        label = "Department")                           
                ),
                mainPanel(
                    column(6,
                    plotOutput("top10words")
                    ),
                    column(6)
                )
            )
        ),
        # Risk Management ----
        tabPanel("Risk Management",
            tags$br(),
            sidebarLayout(
            sidebarPanel(width = 2,
                numericInput(
                    inputId = "risk_number",
                    value = 1,
                    label = "Risk number",
                    width = 120
                ),
                       hr(),
                       actionButton(width = 120, "save_risk", "Save Risk"),
                       br(), br(),
                       actionButton(width = 120, "delete_risk", "Delete Risk")
                       ),
            mainPanel(
                    column(10,
                        uiOutput("risk_name"),
                        uiOutput("risk_description"),
                        br(),
                    ),
                    column(2,
                        br(),
                        uiOutput("risk_impact"),
                        uiOutput("risk_probability"),
                    ))
            ),
            sidebarLayout(
            sidebarPanel(width = 2, 
                uiOutput("action_number"),
                actionButton(width = 120, "save_action", "Save Action"),
                br(), br(),
                actionButton(width = 120, "delete_action", "Delete Action"),
                # actionButton("delete_action")
                    ),
            mainPanel(
                column(12,
                    uiOutput("action_description")
                )
                )
            )

        )
    )
))














