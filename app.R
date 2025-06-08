#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTime)
library(ggplot2)
library(tibble)
library(dplyr)
library(lubridate)
library(ggQC)

# Data simulation (original is linked to different SQL databases)
simulate_board_data <- function(hours = 12) {
    start_ts <- seq.POSIXt(from = now() - hours * 3600, by = "10 min", length.out = hours * 6)
    tibble(
        start_ts = start_ts,
        average_diameter = rnorm(length(start_ts), mean = 0.46, sd = 0.01),
        pinhole_category1 = rpois(length(start_ts), lambda = 45),
        cropped = sample(c(TRUE, FALSE), length(start_ts), replace = TRUE)
    )
}

AV_diam_UCL_Grade1 <- 0.480
AV_diam_UCL_Grade2 <- 0.450
Pinholes1_UCL_Grade1 <- 60
Pinholes1_UCL_Grade2 <- 55

ui <- fluidPage(
    titlePanel("Grade1-Grade2 Camera KPI - Demo"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("Ore", "Production time (h)", min = 1, max = 24, value = 12, step = 1),
            selectInput("Grade", "Select Grade", choices = c("Grade1", "Grade2"))
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Morphological_KPI_1", plotOutput("Morphological_KPI_1")),
                tabPanel("Morphological_KPI_2", plotOutput("Morphological_KPI_2"))
            )
        )
    )
)

server <- function(input, output) {
    
    board_data_reactive <- reactive({
        simulate_board_data(input$Ore)
    })
    
    output$Morphological_KPI_1 <- renderPlot({
        board_data <- board_data_reactive()
        Diam_AC <- ifelse(input$Grade == "Grade1", AV_diam_UCL_Grade1, AV_diam_UCL_Grade2)
        ggplot(board_data, aes(x = as.POSIXct(start_ts), y = average_diameter)) +
            geom_point() +
            geom_line() +
            stat_QC(method = "XmR") +
            stat_QC_labels(method = "XmR", digits = 3, text.size = 5) +
            ylab("Morphological_KPI_1 (mm)") +
            ggtitle("Simulation") +
            xlab(paste0("Last ", input$Ore, " hours")) +
            geom_hline(yintercept = Diam_AC, color = "green")
    })
    
    output$Morphological_KPI_2 <- renderPlot({
        board_data <- board_data_reactive() %>% filter(cropped == FALSE)
        Pinholes1_AC <- ifelse(input$Grade == "Grade1", Pinholes1_UCL_Grade1, Pinholes1_UCL_Grade2)
        ggplot(board_data, aes(x = as.POSIXct(start_ts), y = pinhole_category1)) +
            geom_point() +
            geom_line() +
            stat_QC(method = "XmR", auto.label = TRUE, label.digits = 4) +
            ylab("Morphological_KPI_1 (n)") +
            ggtitle("Simulation") +
            xlab(paste0("Last ", input$Ore, " hours")) +
            geom_hline(yintercept = Pinholes1_AC, color = "green")
    })
}

shinyApp(ui = ui, server = server)
