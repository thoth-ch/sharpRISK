library(shiny)
library(DBI)
library(RSQLite)


heatdb_path <- "heatmap.db"
heatdb_conn <- dbConnect(SQLite(), dbname = heatdb_path)

# Heatmap
risks <- tbl(heatdb_conn, "risks")  

risks %>%
  ggplot(aes(x = risk_impact, y = risk_probability)) +
  geom_point() +
  geom_text(aes(label = risk_name, hjust = "left")) +
  scale_x_continuous(
    limits = c(-.5, 4.5),
    breaks = c(1, 2, 3, 4)) +
  scale_y_continuous(
    limits = c(-.5, 4.5),
    breaks = c(1, 2, 3, 4)) +
  theme_light() +
  labs(title = "Heatmap",
       x = "Impact",
       y = "Probability")

# Load data ----
  
loadData <- function() {
  query <- sprintf("SELECT * FROM %s", "risks")
  data <- dbGetQuery(heatdb_conn, query)
  dbDisconnect(heatdb_conn)
  data
}

# Whenever a field is filled, aggregate all form data

fields <- c("risk_name", "risk_number")

formData <- function(text) {
  data <- sapply(text, function(x) input[[x]])
  data
}

formData(fields)

 
# Save data

  saveData <- function(data) {
    heatdb_conn <- dbConnect(SQLite(), heatdb_path)
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      "risks", 
      paste(names(data), collapse = ", "),
      paste(data, collapse = "', '")
    )
    # Submit the update query and disconnect
    db(heatdb_conn, query)
    dbDisconnect(heatdb_conn)
  }
# construct query



query <- sprintf(
  "INSERT INTO %s (%s) VALUES ('%s')",
  "risks", 
  paste(names(data), collapse = ", "),
  paste(data, collapse = "', '")
)
  
  
  
  
  
  