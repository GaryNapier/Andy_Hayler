
# SUMMARY PLOTS MODULE ----

# UI ----

SummaryPlotsUI <- function(id) {
  tagList(
    # OUT
    plotlyOutput(NS(id, "summary_plot_out"))
  )
}

# SERVER ----

SummaryPlotsServer <- function(id, tab, var) {
  moduleServer(id, function(input, output, session) {
    output$summary_plot_out <- renderPlotly({
      SummaryPlot(tab(), {{var}})
    })
  })
}

# DEMO ----

summary_plots_demo <- function() {
  
  n <- 100
  df <- data.frame(Stars = as.factor(sample(0:3, n, replace = T)), 
                   Rand_var = rnorm(n))
  
  ui <- fluidPage(
    SummaryPlotsUI("summary_plots_ID")
    )
  server <- function(input, output, session) {
    SummaryPlotsServer("summary_plots_ID", 
                  reactive({df}), 
                  Stars)
  }
  shinyApp(ui, server)
}

# summary_plots_demo()





