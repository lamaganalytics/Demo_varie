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
#weight_flow_rate<-500#Kg/h
die_holes_number<-1000
die_diameter<-1.7/1000#in meter
die_land<-6/1000#in meter
melt_density<-1200#Kg/m3
k<-6000
n=0.6
#m3/sec
# Data simulation (original is linked to different SQL databases)
simulate_plant_data <-
    tibble(
        start_ts = seq(1:900),
        weight_flow_rate=rep(c(200,300,400, 500, 600), length.out = length(start_ts)),
        
        volumetric_flow_rate=(weight_flow_rate/die_holes_number/3600)/melt_density,
        shear_rate=32*volumetric_flow_rate/(pi*die_diameter^3),
        shear_stress = k*shear_rate^n,
        average_pressure = shear_stress*4*die_land/die_diameter
    )




ui <- fluidPage(
    titlePanel("Viscosity check- Demo"),
    
        mainPanel(
            tabsetPanel(
                tabPanel("Melt_behaviour", plotOutput("Melt_1")),
                tabPanel("viscosty_indexes", plotOutput("Viscosity_index_2")),
                         tabPanel("Viscosity", plotOutput("Viscosity_plot")))
            
        )
    )


server <- function(input, output) {
    set.seed(123)
    
    board_data <- simulate_plant_data %>%
        mutate(
            average_pressure_noisy = average_pressure * (1 + rnorm(n(), 0, 0.02)),
            shear_stress_noisy     = average_pressure_noisy * die_diameter / (4 * die_land),
            eta_noisy              = shear_stress_noisy / shear_rate,          # viscosità apparente “misurata”
            eta_true               = (k * shear_rate^n) / shear_rate           # = K * gamma^(n-1)
        )
    
    # Fit unico (stesso usato dagli altri plot)
    fit <- lm(log(shear_stress_noisy) ~ log(shear_rate), data = board_data)
    n_est <- unname(coef(fit)[2])
    K_est <- exp(unname(coef(fit)[1]))
    
    output$Melt_1 <- renderPlot({
        ggplot(board_data %>% arrange(volumetric_flow_rate),
               aes(x = weight_flow_rate, y = average_pressure_noisy / 1e5)) +
            geom_point() +
            geom_smooth() +
            ylab("Melt pressure (bar)") +
            ggtitle("Simulation at constant T; die diam = 1.7 mm; die land = 6 mm; 1000 holes") +
            xlab("Flow rate (Kg/h)")
    })
    
    output$Viscosity_index_2 <- renderPlot({
        # scegli UNA base log; qui uso naturale ovunque
        fit <- lm(log(shear_stress_noisy) ~ log(shear_rate), data = board_data)
        n_est <- unname(coef(fit)[2])
        K_est <- exp(unname(coef(fit)[1]))
        
        ggplot(board_data %>% arrange(volumetric_flow_rate),
               aes(x = log(shear_rate), y = log(shear_stress_noisy))) +
            geom_point() +
            geom_smooth() +
            xlab("log(shear rate)") +
            ylab("log(shear stress)") +
            ggtitle(paste0("Fit (natural logs): n = ", round(n_est, 3),
                           " | K = ", format(round(K_est, 1), big.mark = ",")))
    })
    output$Viscosity_plot <- renderPlot({
        bd <- board_data %>%
            arrange(shear_rate) %>%
            mutate(
                eta_model = K_est * shear_rate^(n_est - 1)  # curva del modello stimato
            )
        
        ggplot(bd, aes(x = shear_rate, y = eta_noisy)) +
            geom_point(alpha = 0.7) +
            geom_line(aes(y = eta_model)) +              # modello stimato
            geom_line(aes(y = eta_true), linetype = "dashed") + # curva “true” (opzionale)
            scale_x_log10() +
            scale_y_log10() +
            xlab("Shear rate (1/s, log scale)") +
            ylab("Apparent viscosity (Pa·s, log scale)") +
            ggtitle(paste0("Viscosity vs shear rate — n_est = ", round(n_est, 3),
                           ", K_est = ", format(round(K_est, 1), big.mark = ",")))
    })
}


shinyApp(ui = ui, server = server)
