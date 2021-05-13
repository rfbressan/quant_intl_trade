#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)
library(plotly)

source("help_funcs.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "superhero"),
    # use this in non shinydashboard app
    # setBackgroundColor(color = "ghostwhite"),
    # useShinydashboard(),
    
    # Application title
    titlePanel("Replication of Artuç et. al. (2008)"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("beta",
                        "beta",
                        min = 0.9,
                        max = 0.99,
                        step = 0.01,
                        value = 0.97),
            sliderInput("nu",
                        "v",
                        min = 0.11,
                        max = 0.91,
                        step = 0.10,
                        value = 0.31),
            sliderInput("t_shock",
                        "Opening time",
                        min = 1,
                        max = 10,
                        value = 10),
            sliderInput("pft",
                        "Free-trade price",
                        min = 0.5,
                        max = 0.9,
                        step = 0.1,
                        value = 0.7),
            sliderInput("C",
                        "Moving Cost",
                        min = 1,
                        max = 5,
                        value = 1),
            actionButton("go",
                         "Simulate!",
                         icon = icon("running")),
            br(),
            br(),
            img(src = "logo-desktop.png", height = 64, width = 202),
            br(),
            # br(),
            # em("Developed by"),
            em("Rafael Felipe Bressan, \uA9 2021."),
        width = 3),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("upperPlot", height = "360px"),
            plotlyOutput("lowerPlot", height = "360px"),
           # DT::dataTableOutput("sim_dt"),
        width = 9)
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Other global variables
    Tss <- 30
    tol <- 1e-5
    
    # Compute the model only when the action button is pressed!
    sim_dt <- eventReactive(input$go, {
        beta <- input$beta
        nu <- input$nu
        L_bar <- 2
        Kx <- 1
        Ky <- 1
        C <- input$C
        pft <- input$pft
        t_shock <- input$t_shock
        convergence <- FALSE
        # set price vector
        prices <- c(rep(1, t_shock), rep(0.7, Tss + 1 - t_shock))
        # theta is a vector of exogenous parameters
        theta <- c(beta = beta, nu = nu, L_bar = L_bar, Kx = Kx, Ky = Ky, C = C, 
                   pft = pft)
        theta_bench <- theta
        theta_bench["pft"] <- 1
        # Start by computing the free-trade steady-state
        ss_ft <- steady_state(theta)
        ss_aut <- steady_state(theta_bench)
        #' Then follows the algorith from ACM (2008) p. 05
        #' 
        #' Guess values for $V_{t+1}^i$. Linear interpolation between steady states
        z_xt <- seq(ss_aut["V_x"], ss_ft["V_x"], length.out = Tss + 1)
        z_yt <- seq(ss_aut["V_y"], ss_ft["V_y"], length.out = Tss + 1)
        #' While there is no convergence we keep computing $\bar\mu_t^i$, $L_{t+1}^i$, 
        #' $w_{t+1}^i$ and update $z_t^i$
        while (!convergence) {
            mu_xt <- mu_i(z_xt, z_yt, theta)
            mu_yt <- mu_i(z_yt, z_xt, theta)
            L_1_lst <- L_1(mu_xt, mu_yt, theta)
            w_xt <- w_it(L_1_lst[[1]], prices, theta, "x")
            w_yt <- w_it(L_1_lst[[2]], prices, theta, "y")
            ztil_xt <- ztil_it(w_xt, mu_xt, z_xt, ss_ft["w_x"], theta)
            ztil_yt <- ztil_it(w_yt, mu_yt, z_yt, ss_ft["w_y"], theta)
            convergence <- abs(max(c(z_xt - ztil_xt, z_yt - ztil_yt))) < tol
            z_xt <- ztil_xt
            z_yt <- ztil_yt
        }
        #' By the end we have the time path of all endogenous variables: $\bar\mu_t^i$, 
        #' $L_t^i$, $w_t^i$ and $V_t^i$.
        #' 
        #' Create a tibble with all data
        tibble(
            sector = rep(c("X", "Y"), each = Tss + 1),
            wage = c(w_xt, w_yt), 
            labor = c(L_1_lst[[1]], L_1_lst[[2]]),
            value = c(z_xt, z_yt),
            mu = c(mu_xt, mu_yt),
            t = rep(0:30, 2)
        )
        
    })

    output$upperPlot <- renderPlotly({
        req(input$go)
        p1 <- ggplotly(
            ggplot(sim_dt(), aes(t, labor, color = sector)) +
                geom_line() +
                scale_color_brewer(type = "qual", palette = "Dark2") +
                scale_x_continuous(breaks = seq(0, 30, by = 2)) +
                labs(x = "time",
                     y = "Labor force in each sector") +
                theme_classic() +
                theme(legend.position = "none"),
            tooltip = c("x", "y", "sector")
        )
        p2 <- ggplotly(
            ggplot(sim_dt(), aes(t, wage, color = sector)) +
                geom_line() +
                scale_color_brewer(type = "qual", palette = "Dark2") +
                scale_x_continuous(breaks = seq(0, 30, by = 2)) +
                labs(x = "time",
                     y = "Real wages") +
                theme_classic() +
                theme(legend.position = "none"),
            tooltip = c("x", "y", "sector")
        )
        subplot(p1, p2, titleX = TRUE, titleY = TRUE)
    })
    
    output$lowerPlot <- renderPlotly({
        req(input$go)
        p1 <- ggplotly(
            ggplot(sim_dt(), aes(t, value, color = sector)) +
                geom_line() +
                scale_color_brewer(type = "qual", palette = "Dark2") +
                scale_x_continuous(breaks = seq(0, 30, by = 2)) +
                labs(x = "time",
                     y = "Worker's value function") +
                theme_classic() +
                theme(legend.position = "none"),
            tooltip = c("x", "y", "sector")
        )
        p2 <- ggplotly(
            ggplot(sim_dt(), aes(t, mu, color = sector)) +
                geom_line() +
                scale_color_brewer(type = "qual", palette = "Dark2") +
                scale_x_continuous(breaks = seq(0, 30, by = 2)) +
                labs(x = "time",
                     y = "Worker's threshold value") +
                theme_classic() +
                theme(legend.position = "none"),
            tooltip = c("x", "y", "sector")
        )
        subplot(p1, p2, titleX = TRUE, titleY = TRUE)
    })
    
    # output$sim_dt <- renderDataTable({
    #     req(input$go)
    #     sim_dt()
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
