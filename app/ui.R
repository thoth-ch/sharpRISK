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
                sidebarPanel(width = 2,
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
                sidebarPanel(width = 2,
                ),
                mainPanel(
                    plotOutput("top10words")
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
                    width = 100
                ),
                       hr(),
                       actionButton(width = 100, "save_risk", "Save Risk"),
                       br(), br(),
                       actionButton(width = 100, "delete_risk", "Delete Risk")
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
                actionButton(width = 100, "update_action", "Save Action"),
                br(), br(),
                actionButton(width = 100, "delete_action", "Delete Action"),
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














