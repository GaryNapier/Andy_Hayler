

# ANDY HAYLER APP ----

# DEPS ----
library(shiny)
library(dplyr)
library(plotly)

# DATA ---- 

# Read in data ---

URL <- "https://www.andyhayler.com/restaurant-guide?size=0"
# tab <- parse_data(URL)
# save(tab, file = "tab.RData")
load("tab.RData")

# Clean ----

tab <- clean_data(tab)


# SERVER ----

server <- function(input, output, session) {
  
  # Summary plots ----
  SummaryPlotsServer("summary_plots_ID", 
                     reactive({tab}), 
                     input$summary_plots_select)
}

