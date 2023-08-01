## Dataset - Seattle Gov. 2016 Building Energy Benchmarking: https://data.seattle.gov/dataset/2016-Building-Energy-Benchmarking/2bpz-gwpy
## Main source used as a reference: https://gelijuani.shinyapps.io/credit/#section-graphs

#Package load
install.packages("DT")
install.packages("plotly")
install.packages("hrbrthemes")
install.packages("viridis")

library(flexdashboard)
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(DT)
library(plotly)
library(shiny)

data <- read_csv("2016_Building_Energy_Benchmarking.csv") #Read Dataset

data <- data %>% rename(SiteEnergyUse_kBtu = `SiteEnergyUse(kBtu)`, Electricity_kBtu = `Electricity(kBtu)`, NaturalGas_kBtu = `NaturalGas(kBtu)`) #Re name for fitting of the dataset

Categorical.Variables <- c("BuildingType", "PrimaryPropertyType", "Neighborhood") #For Sidebar Variable

Numeric.Variables <- c("SiteEnergyUse_kBtu", "Electricity_kBtu", "NaturalGas_kBtu", "TotalGHGEmissions") #Sidebar Variable 2

Select.Vars <- c("OSEBuildingID", "YearBuilt", "BuildingType", "PrimaryPropertyType", 
                 "Neighborhood", "SiteEnergyUse_kBtu", "Electricity_kBtu", "NaturalGas_kBtu", "TotalGHGEmissions") #Assigning

# Omits NAs
data2 <- data %>% select(Select.Vars) %>% drop_na(Numeric.Variables)

# IQR Method -> Omits NA
data2 <- data2 %>% mutate_at(vars(Numeric.Variables), ~ifelse(. < quantile(., 0.25) - 1.5*IQR(.) | . > quantile(., 0.75) + 1.5*IQR(.), NA, .))

theme <- theme_ipsum() + theme(plot.caption = element_text(hjust=0, size=8), #Theme Setting
                               plot.title = element_text(hjust = 0, size=12, face="bold"),
                               axis.title.x = element_text(size=10),
                               axis.title.y = element_text(size=10))  

ui <- fluidPage( #Ui panel
  titlePanel("Seattle Building Energy Data Dashboard - Interactive Year"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Year:", #Slidebar : Year
                  min = min(data$YearBuilt),
                  max = max(data$YearBuilt),
                  value = c(min(data$YearBuilt), max(data$YearBuilt)),
                  step = 1),
      
      selectInput(inputId="categorical_variable", label = "Select Categorical Variable:", #Categorical slidebar
                  choices = Categorical.Variables, selected = Categorical.Variables[2]),
      
      selectInput(inputId="numeric_variable", label = "Select Numeric Variable:", #Numerical slidebar
                  choices = Numeric.Variables, selected = Numeric.Variables[1])
    ),
    
    mainPanel( #Plot
      plotOutput(outputId="boxplot"),
      plotOutput(outputId="densityplot"),
      plotOutput(outputId="bargraph")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    data2 %>%
      filter(YearBuilt >= input$year[1] & YearBuilt <= input$year[2])
  })

  output$boxplot <- renderPlot({ #Boxplot
    p <- ggplot(filtered_data(), aes_string(x = input$numeric_variable, 
                                  y = input$categorical_variable,  
                                  fill = input$categorical_variable)) +
      geom_boxplot() + 
      scale_fill_viridis(discrete = T, alpha=0.4, option="A") +
      theme 
    print(p)
  })
  
  output$densityplot <- renderPlot({ #Densityplot
    p <- ggplot(filtered_data(), aes_string(x = input$numeric_variable, fill = input$categorical_variable)) +
      geom_density(alpha = 0.4) +
      scale_fill_viridis(discrete = T, alpha=0.4, option="A") +
      theme 
    print(p)
  })
  
  output$bargraph <- renderPlot({ #Bargraph
    p <- ggplot(filtered_data(), aes_string(x = input$categorical_variable, fill = input$categorical_variable)) +
      geom_bar() +
      theme + theme(axis.text.x=element_blank()) + 
      scale_fill_viridis(discrete = T, alpha=0.4, option="A") 
    
    print(p)
  })
}

shinyApp(ui, server)

