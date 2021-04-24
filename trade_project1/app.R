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
            p("There are 3 scenarios to choose from."),
            p("1. Baseline year 2014"),
            p("2. 30% Increased trade costs due to Covid-19"),
            p("3. 10% reduction in productivity"),
            selectInput("scenario",
                        "Choose a scenario:",
                        c("Baseline",
                          "Increase in trade costs",
                          "Productivity reduction"),
                        selected = 1),
            br(),
            img(src = "logo-desktop.png", height = 96, width = 304)
        ),

        # Show a Tab Panel with multiple outputs
        mainPanel(
            tabsetPanel(
                id = "tabset",
                tabPanel("Dashboard",
                         fluidRow(infoBoxOutput("wld_gdp")
                                  ),
                         fluidRow(infoBoxOutput("biggest"),
                                  infoBoxOutput("smallest")
                                  ),
                         fluidRow(infoBoxOutput("richest"),
                                  infoBoxOutput("poorest")
                                  )
                         ),
                tabPanel("Welfare", 
                         plotlyOutput("welfarePlot"),
                         plotlyOutput("welfareChange")
                         ),
                tabPanel("Table",
                         # plotlyOutput("valuesPlot"),
                         DT::dataTableOutput("dataset", width = "90%")
                         )
            )
           
        )
    ),
    br(),
    em("Developed by"),
    p("Rafael Felipe Bressan, \uA9 2021.")
)

server <- function(input, output) {
    # Always load the baseline welfare
    base_welfare <- read_fst("base_bycountry.fst", as.data.table = TRUE)
    dataset <- reactive({
        fst_name <- switch(input$scenario,
                      "Baseline" = "base_bycountry.fst",
                      "Increase in trade costs" = "sc1_bycountry.fst",
                      "Productivity reduction" = "sc2_bycountry.fst",
                      "base_bycountry.fst"
        )
        read_fst(fst_name, as.data.table = TRUE)
    })
    gdp_bycountry <- reactive({
        dataset()[, by = out_country, sum(wage_i*employed_i)][order(-V1)]
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
