#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(dplyr)
library(fst)

# Global variables for the app
# Always load the baseline welfare
base_welfare <- read_fst("base_bycountry.fst", as.data.table = TRUE)
countries <- base_welfare[, out_country]

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),
    # use this in non shinydashboard app
    # setBackgroundColor(color = "ghostwhite"),
    useShinydashboard(),
    
    # Application title
    titlePanel("The Covid-19 pandemic and world trade"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p("There are 4 scenarios to choose from."),
            p("1. Baseline year 2014"),
            p("2. 10% Increased trade costs due to Covid-19"),
            p("3. 3% reduction in productivity"),
            p("4. Combination of scenarios 2 and 3"),
            selectInput("scenario",
                        "Choose a scenario:",
                        c("Baseline",
                          "Increase in trade costs",
                          "Productivity reduction",
                          "Combination"),
                        selected = 1),
            br(),
            img(src = "logo-desktop.png", height = 96, width = 304),
            br(),
            br(),
            em("Developed by"),
            p("Rafael Felipe Bressan, \uA9 2021.")
        ),

        # Show a Tab Panel with multiple outputs
        mainPanel(
            tabsetPanel(
                id = "tabset",
                tabPanel("Dashboard",
                         fluidRow(infoBoxOutput("wld_gdp")
                                  ),
                         fluidRow(infoBoxOutput("biggest"),
                                  infoBoxOutput("richest"),
                                  infoBoxOutput("expensive")
                                  ),
                         fluidRow(infoBoxOutput("smallest"),
                                  infoBoxOutput("poorest"),
                                  infoBoxOutput("cheapest")
                                  ),
                         div(),
                         h3("Welfare Metrics"),
                         fluidRow(
                             infoBoxOutput("bestWelfare"),
                             infoBoxOutput("worstWelfare"),
                             infoBoxOutput("braWelfare")
                         )
                         ),
                tabPanel("Welfare", 
                         plotlyOutput("welfarePlot", height = "300px"),
                         plotlyOutput("welfareChange", height = "300px")
                         ),
                tabPanel("Country",
                         selectInput("country", "Select a country",
                                     countries,
                                     selected = "BRA"),
                         fluidRow(
                             infoBoxOutput("hi_prod"),
                             infoBoxOutput("low_prod")
                         ),
                         plotlyOutput("ind_share", height = "300px")
                         ),
                tabPanel("Table",
                         # plotlyOutput("valuesPlot"),
                         DT::dataTableOutput("dataset", width = "90%")
                         )
            )
           
        )
    )
)

server <- function(input, output) {
    dataset <- reactive({
        fst_name <- switch(input$scenario,
                      "Baseline" = "base_bycountry.fst",
                      "Increase in trade costs" = "sc1_bycountry.fst",
                      "Productivity reduction" = "sc2_bycountry.fst",
                      "Combination" = "sc3_bycountry.fst",
                      "base_bycountry.fst"
        )
        read_fst(fst_name, as.data.table = TRUE)
    })
    gdp_bycountry <- reactive({
        dataset()[, by = out_country, sum(wage_i*employed_i)][order(-V1)]
    })
    trade_dt <- reactive({
        req(input$scenario)
        fst_name <- switch(input$scenario,
                           "Baseline" = "base_trade.fst",
                           "Increase in trade costs" = "sc1_trade.fst",
                           "Productivity reduction" = "sc2_trade.fst",
                           "Combination" = "sc3_trade.fst",
                           "base_trade.fst"
        )
        read_fst(fst_name, as.data.table = TRUE)
    })
    productivity <- reactive({
        req(input$country)
        trade_dt()[out_country == input$country, by = c("out_country", "out_ind"),
                   .(z_ik = first(z_ik))]
    })
    ind_shares <- reactive({
        req(input$country)
        trade_dt()[in_country == input$country, by = c("in_country", "out_ind"), 
                   .(alpha_jk = first(alpha_jk))][order(-alpha_jk)]
    })
    
    # Compute outputs
    # Dashboard
    output$wld_gdp <- renderInfoBox({
        infoBox("World GDP", 
                gdp_bycountry()[, prettyNum(sum(V1), big.mark = ",")], 
                icon = icon("globe"),
                color = "navy", fill = TRUE)
    })
    
    output$biggest <- renderInfoBox({
        infoBox("Biggest", 
                prettyNum(gdp_bycountry()[1, V1], big.mark = ","),
                subtitle = gdp_bycountry()[1, out_country],
                icon = icon("flag"),
                color = "green", fill = TRUE)
    })
    
    output$smallest <- renderInfoBox({
        infoBox("Smallest", 
                prettyNum(gdp_bycountry()[.N, V1], big.mark = ","),
                subtitle = gdp_bycountry()[.N, out_country],
                icon = icon("flag"),
                color = "red", fill = TRUE)
    })
    
    
    output$richest <- renderInfoBox({
        infoBox("Richest", 
                prettyNum(dataset()[order(-wage_i)][1, wage_i], big.mark = ","),
                subtitle = dataset()[order(-wage_i)][1, out_country],
                icon = icon("flag"),
                color = "green", fill = TRUE)
    })
    
    output$poorest <- renderInfoBox({
        infoBox("Poorest", 
                prettyNum(dataset()[order(-wage_i)][.N, wage_i], big.mark = ","),
                subtitle = dataset()[order(-wage_i)][.N, out_country],
                icon = icon("flag"),
                color = "red", fill = TRUE)
    })
    
    output$expensive <- renderInfoBox({
        infoBox("Most Expensive", 
                prettyNum(dataset()[order(-p_i)][1, p_i], big.mark = ","),
                subtitle = dataset()[order(-p_i)][1, out_country],
                icon = icon("dollar-sign"),
                color = "green", fill = TRUE)
    })
    
    output$cheapest <- renderInfoBox({
        infoBox("Cheapest", 
                prettyNum(dataset()[order(-p_i)][.N, p_i], big.mark = ","),
                subtitle = dataset()[order(-p_i)][.N, out_country],
                icon = icon("dollar-sign"),
                color = "red", fill = TRUE)
    })
    
    output$bestWelfare <- renderInfoBox({
        infoBox("Best", 
                prettyNum(dataset()[order(-welfare_i)][1, welfare_i], big.mark = ","),
                subtitle = dataset()[order(-welfare_i)][1, out_country],
                icon = icon("glass-cheers"),
                color = "green", fill = TRUE)
    })
    
    output$worstWelfare <- renderInfoBox({
        infoBox("Worst", 
                prettyNum(dataset()[order(-welfare_i)][.N, welfare_i], big.mark = ","),
                subtitle = dataset()[order(-welfare_i)][.N, out_country],
                icon = icon("trash-alt"),
                color = "red", fill = TRUE)
    })
    
    output$braWelfare <- renderInfoBox({
        infoBox("Brazil", 
                prettyNum(dataset()[out_country == "BRA", welfare_i], big.mark = ","),
                # subtitle = dataset()[order(-p_i)][.N, out_country],
                icon = icon("futbol"),
                color = "blue", fill = TRUE)
    })
    # Country
    output$hi_prod <- renderInfoBox({
        req(input$country)
        infoBox("Most productive industry", 
                prettyNum(productivity()[, max(z_ik)], digits = 6), 
                productivity()[which.max(z_ik), out_ind],
                icon = icon("cogs"),
                color = "green",
                fill = TRUE)
    })
    
    output$low_prod <- renderInfoBox({
        req(input$country)
        infoBox("Least productive industry", 
                prettyNum(productivity()[, min(z_ik)], digits = 6), 
                productivity()[which.min(z_ik), out_ind],
                icon = icon("cogs"),
                color = "red",
                fill = TRUE)
    })
    output$ind_share <- renderPlotly({
        req(input$country)
        ggplotly(
            ggplot(ind_shares(), 
                   aes(x = out_ind, y = alpha_jk)) +
            geom_col(aes(fill = out_ind), alpha = 0.7) +
            labs(x = "Industry Code", y = "Participation in GDP") +
            scale_color_viridis_d() +
            theme_classic() +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 90))
            
        )
    })

    # Table
    output$dataset <- DT::renderDataTable({
        dt <- dataset()[, -c("trade", "trade_pc")]
        num_cols <- colnames(dt)[sapply(dt, is.numeric)]
        DT::datatable(
            dt,
            options = list(pageLength = 10)
        ) %>% 
            DT::formatRound(num_cols, 3)
    })
    # Welfare
    output$welfarePlot <- renderPlotly({
        req(input$scenario)
        ggplotly(
            ggplot(dataset(), aes(x = log(wage_i), y = welfare_i, color = out_country)) +
            geom_point(aes(size = gamma_i), alpha = 0.6) +
            labs(x = "log(wage)", y = "Welfare") +
            scale_color_viridis_d() +
            theme_classic() +
            theme(legend.position = "none")
        )
    })
    
    output$welfareChange <- renderPlotly({
        req(input$scenario)
        # Welfare percentage change
        chg_welfare <- base_welfare[, .(out_country, welfare_i)
        ][
            dataset()[, .(out_country, welfare_i)], 
            on = "out_country"
        ][, .(out_country,
              chg_welfare = log(i.welfare_i / welfare_i)*100)]
        ggplotly(
            ggplot(chg_welfare, aes(x = out_country, y = chg_welfare)) +
                geom_col(aes(fill = out_country), alpha = 0.6) +
                labs(x = "Country", y = "Percentage change in Welfare") +
                scale_color_viridis_d() +
                theme_classic() +
                theme(legend.position = "none",
                      axis.text.x = element_text(angle = 90))
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
