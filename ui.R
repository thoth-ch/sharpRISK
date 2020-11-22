
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
library(reticulate)
library(pdftools)
library(hunspell)
library(tidytext)
library(widyr)
library(gridExtra)
library(scales)
library(igraph)
library(tidygraph)
library(ggraph)
library(networkD3)

# use_condaenv("/home/joao/anaconda3/envs/Rpython")

# **** USER INTERFACE **** ----
shinyUI(fluidPage(
    
    tags$script(src = "javascript.js"),
    includeCSS("www/risk_app.css"),
    
    fluidRow(
        
        # div(p("test"),
        #     style = "background: url('www/recycling.jpg') no-repeat fixed center;")),
        titlePanel(
            tags$h1("tree", style = "text-align:center")),
        tabsetPanel(
            type = "tabs",
            # Heatmap tab----
            tabPanel("Heatmap",
                     column(1),
                     column(11,
                            tags$br(),
                            div(plotOutput("heatmap"),
                                style = "max-width: 1200px"),
                            div(DTOutput("top5risks"),
                                style = "max-width: 1200px")
                            ),
                     column(1)
            ),
            # Text Analysis tab ----
            tabPanel("Text Analysis",
                     tags$br(),
                     column(6,
                            plotOutput("top10words")
                     ),
                     column(3,
                            plotOutput("actions_corr"))
                     
            ),
            # Outstanding actions tab ----
            tabPanel("Outstanding actions",
                     sidebarLayout(
                         sidebarPanel(width = 2, 
                                      uiOutput("action_responsible2")
                         ),
                         mainPanel(
                             plotOutput("action_resp_count"),
                             DTOutput("actions_by_responsible")
                         )
                     )
                     
            ),
            # Risk Management tab ----
            tabPanel("Data Form",
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
                                    uiOutput("risk_probability_slider"),
                                    uiOutput("risk_impact_slider")
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
                                    uiOutput("action_deadline"),
                                    uiOutput("action_status")
                             )
                             
                         )
                     )
                     
            )
        )
    )
))
