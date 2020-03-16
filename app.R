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
                    div(
                        align = "center",
                        plotOutput("diceRoll", height = "100px", width = "400px")
                    )
                ),
                tabPanel(
                    "Empirical PDF", 
                    plotOutput("empPDF"),
                    hr(),
                    div(
                        align = "center",
                        plotOutput("diceRollToo", height = "100px", width = "400px")
                    ),
                    br(),
                    div(
                        strong("N: "), textOutput("N_current", inline = TRUE), br(),
                        strong("Mean: "), textOutput("mu_current", inline = TRUE)
                    )
                ),
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
    
    ##
    diePlot <- reactive({function(){
        ## Only print if we actually simulate data!
        req(nrow(logfile$simulateData) > 0)
        
        ## Set-up plot area
        par(mar = rep(0, 4), xaxs = "i", yaxs = "i")
        
        ## Draw the plot "empty"
        plot(0, type = "n", axes = FALSE, xlim = c(0.5, 4.5), ylim = c(0, 1))
        box()
        
        ## Do the lines between die
        innerPortLines_X <- c(1.5, 2.5, 3.5)
        segments(innerPortLines_X, rep(0, 3), innerPortLines_X, rep(1, 3))
        
        ##
        currentRoll <- unlist(logfile$simulateData$Rolls[nrow(logfile$simulateData)])
        
        ## 
        text(
            x = 1:4, y = rep(0.5, 4), labels = currentRoll,
            cex = 4, col = dplyr::if_else(1:4 == which.min(currentRoll), "tomato", "black")
        )}
    })

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
        {bind_rows(logfile$simulateData, .)} ->
        logfile$simulateData
    })

    ## Visualise the donut plot of the discussion question
    output$donutPlot <- renderPlot({
        ## Only print if we actually simulate data!
        req(nrow(logfile$simulateData) > 0)

        ## Load in the data
        toPlot <- processData(logfile$simulateData)

        ## Plot the two probabilities as a donut plot
        ggplot(toPlot, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = lessThan21)) +
            theme_void() +
            theme(legend.position = "none") +
            geom_rect() +
            coord_polar(theta = "y") +
            scale_fill_brewer(palette = "Set1") +
            annotate("text", x = 1.5, y = c(0.25, 0.75), label = c("Rho(X >= 21)", "Rho(X < 21)"), size = 6, parse = TRUE) +
            annotate("text", x = 2, y = c(0.28, 0.72), label = as.character(round(toPlot$count/sum(toPlot$count), 2)), size = 6, parse = TRUE) +
            xlim(c(-1, 4))
    })

    ##
    output$diceRoll <- renderPlot({diePlot()()})
    output$diceRollToo <- renderPlot({diePlot()()})

    ## Special case if N_Sim == 1

    ## Visualise the empirical probability density function
    output$empPDF <- renderPlot({
        ## Only print if we actually simulate data!
        req(nrow(logfile$simulateData) > 0)
        
        ## Load in the data
        toPlot <- logfile$simulateData
        
        ## Plot the data as a discrete "histogram"
        ggplot(toPlot, aes(x = Sum, y = ..prop..)) +
            theme_bw() +
            geom_bar(colour = "black", fill = "lightblue") +
            labs(title = "Empirical PDF of X", caption = "X ~ sum(4d10) - min(4d10)") +
            xlab("X") +
            ylab("Density") + 
            scale_x_continuous(breaks = seq(3, 30, by = 3), limits = c(2.4, 30.6), expand = expansion(add = 0.6))
    })
    
    ## UI elements in the empirical probabiltiy density function tab
    output$N_current <- renderText({
        ## Only print if we actually simulate data!
        req(nrow(logfile$simulateData) > 0)
        
        ## Return the number of simulations
        nrow(logfile$simulateData)
    })
    
    output$mu_current <- renderText({
        ## Only print if we actually simulate data!
        req(nrow(logfile$simulateData) > 0)
        
        ## Return the average of the simulations
        mean(logfile$simulateData$Sum)
    })

    ## Table of the raw data
    output$currentData <- renderDataTable(logfile$simulateData)
}

# Run the application
shinyApp(ui = ui, server = server)
