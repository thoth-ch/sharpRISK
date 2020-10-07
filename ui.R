# App address
# https://rre.svr.nestle.com:3838/RDRamalhJo/RiskApp
# https://rre.svr.nestle.com:3838/NSTC/RiskApp

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
library(DT)

# **** USER INTERFACE **** ----
shinyUI(fluidPage(
    
    includeCSS("risk_app.css"),
    
    titlePanel("Risk Management App"),
    tabsetPanel(
        type = "tabs",
        # Heatmap tab----
        tabPanel("Heatmap",
                 column(2),
                 column(8,
                        tags$br(),
                        
                        tableOutput("table"),
                        plotOutput("heatmap"),
                        DTOutput("top5risks")),
                 column(2)
        ),
        # Text Analysis tab ----
        tabPanel("Text Analysis",
                 tags$br(),
                 column(6,
                        plotOutput("top10words")
                 ),
                 column(6)
                 
        ),
        # Risk Management tab ----
        tabPanel("Risk Management",
                 tags$br(),
                 sidebarLayout(
                     sidebarPanel(width = 2,
                                  # numericInput(
                                  #   inputId = "risk_number",
                                  #   value = 1,
                                  #   label = "Risk number",
                                  #   width = 160
                                  # ),
                                  uiOutput("risk_number"),
                                  # hr(),
                                  actionButton(width = 160, "new_risk", "New Risk"),
                                  br(), br(),
                                  actionButton(width = 160, "save_risk", "Save Risk"),
                                  br(), br(),
                                  actionButton(width = 160, "delete_risk", "Delete Risk"),
                                  br(), br(),
                                  downloadButton(width = 160, "downloadRisks", "Download Risks")
                     ),
                     mainPanel(
                         column(8,
                                uiOutput("risk_name"),
                                uiOutput("risk_description"),
                                br()
                         ),
                         column(4,
                                br(),
                                uiOutput("risk_probability"),
                                uiOutput("risk_impact")
                         ))
                 ),
                 sidebarLayout(
                     sidebarPanel(width = 2, 
                                  uiOutput("action_number"),
                                  actionButton(width = 160, "new_action", "New Action"),
                                  br(), br(),
                                  actionButton(width = 160, "save_action", "Save Action"),
                                  br(), br(),
                                  actionButton(width = 160, "delete_action", "Delete Action"),
                                  br(), br(),
                                  downloadButton(width = 160, "downloadActions", "Download Actions")
                     ),
                     mainPanel(
                         column(8,
                                uiOutput("action_description")
                         ),
                         column(2,
                                uiOutput("action_responsible"),
                                uiOutput("action_deadline")
                         )
                         
                     )
                 )
                 
        )
    )
    
))
