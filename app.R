#
# Suppose that you roll a 10-sided die four times and note the results. You then discard the
# smallest number? (If the smallest number occurs more than once you would only discard it
#                   once so that you still have three numbers.)
# Which is the more likely scenario: the sum of the remaining three numbers is less than 21
# or the sum of the remaining three numbers is 21 or more?
#

library(shiny); library(ggplot2); library(dplyr); library(magrittr)

ui <- fluidPage(

    ## Application title
    titlePanel("Tutorial 3 - Discussion Question"),

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
                tabPanel(
                    "Results",
                    plotOutput("donutPlot"),
                    hr(),
                    plotOutput("diceRoll", height = "100px")
                ),
                tabPanel("Empirical PDF", verbatimTextOutput("empPDF")),
                tabPanel("Data", dataTableOutput("currentData"))
            )
        )
    )
)

processData <- function(input) {
    ##
    toPlot <- input %>%
        mutate(lessThan21 = if_else(Sum < 21, "B", "A")) %>%
        group_by(lessThan21) %>%
        summarise(count = n()) %>%
        mutate(fraction = count/sum(count),
               ymax = cumsum(fraction))

    ##
    if (nrow(toPlot) < 2) {
        if (toPlot$lessThan21[1] == "A") {
            toPlot %<>% add_row(lessThan21 = "B", count = 0, fraction = 0, ymax = 1, .after = 1)
        } else {
            toPlot %<>% add_row(lessThan21 = "A", count = 0, fraction = 0, ymax = 0, .before = 1)
        }
    }

    toPlot$ymin = c(0, head(toPlot$ymax, n = -1))
    toPlot$labelPosition <- (toPlot$ymax + toPlot$ymin) / 2

    return(toPlot)
}

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
    output$donutPlot <- renderPlot({
        ##
        req(nrow(logfile$simulateData) > 0)

        ##
        toPlot <- processData(logfile$simulateData)

        ##
        ggplot(toPlot, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = lessThan21)) +
            theme_void() +
            theme(legend.position = "none") +
            geom_rect() +
            coord_polar(theta = "y") +
            scale_fill_brewer(palette = "Set1") +
            # title("Rho(X >= 21)", parse = TRUE)
            # annotate("text", x = 4, y = c(0.25, 0.75), label = c("Rho(X >= 21)", "Rho(X < 21)"), size = 6, parse = TRUE) +
            # annotate("text", x = 4, y = c(0.27, 0.73), label = as.character(round(toPlot$count/sum(toPlot$count), 2)), size = 6, parse = TRUE) +
            xlim(c(-1, 4))
    })

    output$diceRoll <- renderPlot({
        ##
        req(nrow(logfile$simulateData) > 0)

    })

    ## Special case if N_Sim == 1

    ## Visualise the empirical probability density function
    #output$empPMF

    ## Table of the raw data
    output$currentData <- renderDataTable(logfile$simulateData)
}

# Run the application
shinyApp(ui = ui, server = server)
