
# ANDY HAYLER APP ----

# UI ----

ui <- fluidPage(
  
  # Summary plots ----
  
  sidebarLayout(
    sidebarPanel(
      selectInput("summary_plots_select", 
                  "Number of reviews by...",
                  choices = c("Cuisine", 
                              "Country", 
                              "Rating", 
                              "Stars"))
    ), 
    
    # Summary plots ----
    SummaryPlotsUI("summary_plots_ID")
  )
  
)
