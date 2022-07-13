# Eric McKenzie
# Homework 6 - Case Study 3
#
#
display.mode = "showcase"

library(shiny)
library(tidyr)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  titlePanel("Parameters that Affect Life Expectancy"),
  
  sidebarLayout(
    sidebarPanel(

    # This dropdown list selects the paramter to compare
        selectInput("Parameter", label = h3("Parameter"),
        choices = list( "Total Health Care Expenditure" = "Total.expenditure",
                        "Percentage Expenditure" = "percentage.expenditure",
                        "Alcohol" = "Alcohol",
                        "Thinness 1-19 years old" = "thinness..1.19.years",
                        "Thinness 5-9 years old" = "thinness.5.9.years",
                        "BMI" = "BMI",
                        "Schooling" = "Schooling",
                        "GDP" = "GDP",
                        "Measles" = "Measles",
                        "Polio" = "Polio",
                        "HIV AIDS" = "HIV.AIDS",
                        "Hepatitis B" = "Hepatitis.B",
                        "Diphtheria" = "Diphtheria"
                       ), selected = 1),
        
        radioButtons("sepStatus", "Separate Developed vs Developing Nations:",
                     choices = list("Yes" = "Yes", "No" = "No"),
                     selected = "No",
                     inline = F,
                     width = "100%")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = "distPlot"),
      textOutput("RSQ")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
  
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    
    LE = read.csv("Life Expectancy Data.csv")
  
    Graph_Theme = theme(
      axis.title.x = element_text(size = 20),
      axis.text.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      legend.title = element_text(size=20), 
      legend.text = element_text(size=20))
  
    LE2 <- LE %>% drop_na()
      
    colname = input$Parameter
  
    #x <- LE2$colname
  
    if (input$sepStatus == "Yes")
    {
      ggplot(LE2, aes(x = .data[[colname]], y=Life.expectancy, color=Status)) + 
        geom_point(shape=18) + 
        ylab("Life Expectancy") +
        xlab(colname) +
        geom_smooth(method=lm, level=0.95, fullrange=TRUE) +
        Graph_Theme
    }
    else
    {
      ggplot(LE2, aes(x = .data[[colname]], y=Life.expectancy)) + 
        geom_point(shape=18) + 
        ylab("Life Expectancy") +
        xlab(colname) +
        geom_smooth(method=lm, level=0.95, fullrange=TRUE)  +
        Graph_Theme
    }
    
  })
  
  # Show R squared
  output$RSQ <- renderText({
    LE = read.csv("Life Expectancy Data.csv")
    LE2 <- LE %>% drop_na()
    colname = input$Parameter
    
    ml = lm(LE2[[colname]]~LE2$Life.expectancy, data = LE2)
    r2 = summary(ml)$r.squared
    paste ("R Squared between ", input$Parameter, " and average life expectancy:\n", r2)
      
  })

}
  

shinyApp(ui = ui, server = server)
