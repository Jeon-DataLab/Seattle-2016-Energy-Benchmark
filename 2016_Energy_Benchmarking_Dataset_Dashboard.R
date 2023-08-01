library(shiny)
library(dplyr)
library(plotly)

# Load data
data <- read.csv("2016_Building_Energy_Benchmarking.csv")

#Exploratory Analysis
str(data)
summary(data)
summary(data$TotalGHGEmissions)

ui <- fluidPage(

    titlePanel("Seattle Building Energy Benchmarking"), 
  
  sidebarLayout(
    sidebarPanel(
      selectInput("property_type", "Property Type:", choices = unique(data$PrimaryPropertyType))
    ),
    
    mainPanel(
      
      plotlyOutput("plot2"),
      plotlyOutput("plot3"),
      plotlyOutput("plot4")
    )
  )
)

server <- function(input, output) {
  
  # Filter data based on user input
  filtered_data <- reactive({
    data %>%
      filter(PrimaryPropertyType == input$property_type) %>%
      select(Electricity.kWh., TotalGHGEmissions, PrimaryPropertyType, 
             YearBuilt, GHGEmissionsIntensity, SiteEnergyUse.kBtu., 
             Electricity.kBtu. )
  })
  
# plot 1
  output$plot2 <- renderPlotly({
    sample_data <- sample_n(filtered_data(), 30, replace = TRUE)
    p <- ggplot(sample_data, aes(x = Electricity.kBtu., y = SiteEnergyUse.kBtu.)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(title = "Electricity Use vs. SiteEnergy Use by Primary Property Type",
           x = "Electricity (kBtu)",
           y = "SiteEnergy Use")
    
    ggplotly(p)%>% 
      layout(xaxis = list(title = list(text = "Electricity (kBtu)", font = list(size = 14)), 
                          tickfont = list(size = 7)), 
             yaxis = list(title = list(text = "SiteEnergy Use", font = list(size = 14)), 
                          tickfont = list(size = 7)))
    
  })
  
  output$plot3 <- renderPlotly({
    sample_data2 <- sample_n(filtered_data(), 30, replace = TRUE)
    p <- ggplot(sample_data2, aes(x = Electricity.kBtu., y = TotalGHGEmissions)) + 
      geom_point() + 
      geom_smooth(method = "lm") +
      labs(title = "Electricity Use vs. Total GHG Emissions by Primary Property Type",
           x = "Electricity (kBtu)",
           y = "Total GHG Emissions")
    
    plotly_build(ggplotly(p)) %>% 
      layout(xaxis = list(title = list(text = "Electricity (kBtu)", font = list(size = 14)), 
                          tickfont = list(size = 7)), 
             yaxis = list(title = list(text = "SiteEnergy Use", font = list(size = 14)), 
                          tickfont = list(size = 7)))
  })
  
  
  output$plot4 <- renderPlotly({
    sample_data3 <- sample_n(filtered_data(), 30, replace = TRUE)
    p <- ggplot(sample_data3, aes(x=YearBuilt, y= TotalGHGEmissions)) + 
      geom_point() + 
      geom_smooth(method = "lm") +
      labs(title="GHG Emissions Intensity through building timeline by Primary Property Type",
           x="YearBuilt",
           y="Total GHG Emissions")
    
    ggplotly(p) %>% 
      layout(xaxis = list(title = list(text = "Year Built", font = list(size = 14)), 
                          tickfont = list(size = 7)), 
             yaxis = list(title = list(text = "Total GHG Emissions", font = list(size = 14)), 
                          tickfont = list(size = 7)))
  })
  
}

shinyApp(ui, server)

ui <- fluidPage(
  titlePanel("Seattle Building Energy Benchmarking"),
  sidebarLayout(
    sidebarPanel(
      selectInput("property_type", "Property Type:", choices = unique(data$PrimaryPropertyType)),
      
      #sliderinputs 
      sliderInput("SiteEnergyUse",
                  "Site Energy Use (kBtu):",
                  min = min(0),
                  max = max(873923712),
                  value = c(0,873923712),
                  step = 1),
      
      sliderInput("SteamUse",
                  "Steam Use (kBtu):",
                  min = (0),
                  max = (134943456),
                  value = c(0,134943456),
                  step = 1),
      
      sliderInput("Electricity",
                  "Electricity (kBtu):",
                  min = 0,
                  max = 657074389,
                  value = c(0, 657074389),
                  step = 1),
      
      sliderInput("NaturalGas",
                  "Natural Gas (kBtu):",
                  min = min(0),
                  max = max(297909000),
                  value = c(0,297909000),
                  step = 1),
      
      sliderInput("TotalGHGEmissions",
                  "Total GHG Emissions:",
                  min = 0,
                  max = 16870.98,
                  value = c(0,16870.98),
                  step = 1)
      
    ),
    
    mainPanel(
      plotlyOutput("plot5") #plot 5
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    data %>%
      filter(PrimaryPropertyType == input$property_type,
             
             SiteEnergyUse.kBtu. >= input$SiteEnergyUse[1],
             SiteEnergyUse.kBtu. <= input$SiteEnergyUse[2],
             
             SteamUse.kBtu. >= input$SteamUse[1],
             SteamUse.kBtu. <= input$SteamUse[2],
             
             Electricity.kBtu. >= input$Electricity[1],
             Electricity.kBtu. <= input$Electricity[2],
             
      ) %>%
      select(Electricity.kWh., TotalGHGEmissions, PrimaryPropertyType, YearBuilt, GHGEmissionsIntensity, SiteEnergyUse.kBtu., Electricity.kBtu. )
  })
  
  output$plot5 <- renderPlotly({
    plot_data <- filtered_data()
    plot_ly(plot_data, x = ~TotalGHGEmissions, y = ~GHGEmissionsIntensity, showlegend = FALSE, color = ~PrimaryPropertyType, type = 'scatter', mode = 'markers', hoverinfo = 'text',
            text = ~paste("Property Type: ", PrimaryPropertyType, "Total GHG Emissions: ", TotalGHGEmissions, "GHG Emissions Intensity: ", GHGEmissionsIntensity)
    ) %>%
      add_markers() %>% # Add markers for data points
      add_lines(x = ~lm(TotalGHGEmissions ~ GHGEmissionsIntensity, data = plot_data)$fitted.values, # Add trendline using linear regression
                y = ~GHGEmissionsIntensity,
                line = list(color = 'black'),
                name = 'Trendline') %>%
      layout(title="Total GHG Emissions vs. GHG Emissions Intensity by Primary Property Type",
             xaxis = list(title="Total GHG Emissions"),
             yaxis = list(title="GHG Emissions Intensity")
      )
  })
}

shinyApp(ui = ui, server = server)
