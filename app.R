#
# Suppose that you roll a 10-sided die four times and note the results. You then discard the
# smallest number? (If the smallest number occurs more than once you would only discard it
#                   once so that you still have three numbers.)
# Which is the more likely scenario: the sum of the remaining three numbers is less than 21
# or the sum of the remaining three numbers is 21 or more?
#

library(shiny); library(ggplot2); library(dplyr)

ui <- fluidPage(

    ## Application title
    titlePanel("Tutorial 3 – Discussion Question"),

    sidebarLayout(
        sidebarPanel(
            ## Input for the number of simulations
            numericInput("N_sim", "Number of Simulations:", min = 1, value = 100, step = 10),
            actionButton("singlePlay", "Simulate", class = "btn btn-primary"),
            hr()
        ),
        mainPanel(
            tabsetPanel(
                type = "tabs",
                id = "viewport",
                tabPanel("Results", verbatimTextOutput("donutPlot")),
                tabPanel("Empirical PDF", verbatimTextOutput("empPDF")),
                tabPanel("Data", dataTableOutput("currentData"))
            )
        )
    )
)

server <- function(input, output, session) {
    ## 
    logfile <- reactiveValues(
        previous.N_sim = 100,
        simulateData = tibble(Rolls = list(), Drop = numeric(), Sum = numeric(), Run_ID = numeric())
    )
    
    ## Ensure sensible values in the "N_sim" element
    observeEvent(input$N_sim, {
        ## Ensure that the field isn't blank
        if (isTruthy(input$N_sim)) {
            if (input$N_sim < 1 | !is.numeric(input$N_sim)) {
                ## Update to the last sensible value
                updateNumericInput(session, "N_sim", value = logfile$previous.N_sim)
            } else {
                ## Update the last sensible value
                logfile$previous.N_sim <- input$N_sim
            }
        }
    })
    
    ## Generate data via the "singlePlayer" element
    observeEvent(input$singlePlay, {
        ## Generate the 4d10 "N_sim" times
        x <- matrix(sample(x = 1:10, size = 4 * input$N_sim, replace = TRUE), nrow = input$N_sim, ncol = 4)

        ## Bind together the newly generated data to the current "logfile$simulateData" object
        tibble(
            Rolls = apply(x, 1, list), 
            Drop = apply(x, 1, function (y) {min(y)}), 
            Sum = apply(x, 1, function (y) {sum(y) - min(y)}), 
            Run_ID = as.numeric(input$singlePlay)
        ) %>%
        bind_rows(logfile$simulateData) ->
        logfile$simulateData
    })
    
    ## Visualise the donut plot of the discussion question
    #output$donutPlot
    
    ## Visualise the empirical probability density function
    #output$empPMF
    
    ## Table of the raw data
    # output$currentData <- renderDataTable(logfile$simulateData)
}

# Run the application 
shinyApp(ui = ui, server = server)
