library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(rworldmap)
library(forcats)

suicides <-
  read.csv(
    "master.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )
options(scipen=999)
countries <- unique(suicides$�.�country)

ui <- fluidPage(
  titlePanel("Analiza danych o samob�jstwach od 1985 do 2016"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "var",
        label = h3("Wybierz rodzaj wykresu"),
        choices = list(
          "Og�lna ilo�� samob�jstw" = "a",
          "Procent samob�jstw na populacje" = "b",
          "Procent kobiet i m�czyzn kt�rzy pope�nili samob�jstwo" = "c",
          "Ilo�� samob�jstw w krajach" = "d",
          "Reprezentacja ilo�ci samob�jstw na mapie �wiata" = "e",
          "Samob�jstwa w poszczg�lnych generacjach" = "f",
          "Podzia� samob�jstw ze wzgl�du na kategorie wiekowe" = "g"
        ),
        selected = "a"
      ),
      conditionalPanel(
        "input.var != 'd' && input.var != 'e'",
        selectInput(
          "varCountry",
          label = h3("Wybierz kraj"),
          choices = c("Wszystkie", countries),
          selected = "Wszystkie"
        )
      ), 
      sliderInput(
        "range",
        label = h3("Zakres lat"),
        min = 1985,
        max = 2016,
        value = c(1985, 1987),
        sep = ""
      ),
      conditionalPanel(
        "input.var == 'd'",
        sliderInput(
          "tops",
          label = h3("Ilo�� pozycji"),
          min = 3,
          max = 30,
          value = 3,
          sep = ""
        )
      )
    ),
    mainPanel(
      conditionalPanel("input.var == 'a'",
                       plotOutput(outputId = "firstPlot")),
      conditionalPanel("input.var == 'b'",
                       plotOutput(outputId = "secondPlot")),
      conditionalPanel("input.var == 'c'",
                       plotOutput(outputId = "thirdPlot")),
      conditionalPanel("input.var == 'd'",
                       plotOutput(outputId = "fourthPlot")),
      conditionalPanel("input.var == 'e'",
                       plotOutput(outputId = "fifthPlot")),
      conditionalPanel("input.var == 'f'",
                       plotOutput(outputId = "sixthPlot")),
      conditionalPanel("input.var == 'g'",
                       plotOutput(outputId = "seventhPlot"))
    )
  )
)



server <- function(input, output) {
  
  output$firstPlot <- renderPlot({
    min <- input$range[1]
    max <- input$range[2]
    
    suicidesByYear <- suicides %>%
      filter(
        if (input$varCountry == "Wszystkie")
          �.�country %in% countries
        else
          �.�country %in% input$varCountry) %>%
      filter(between(year, min, max)) %>%
      group_by(year) %>%
      summarise(suicides_no = sum(suicides_no))
    
    ggplot(suicidesByYear, aes(x = reorder(year, year), y = suicides_no)) +
      geom_bar(stat = "identity",
               color = "white",
               fill = "orange") +
      labs(x = "",
           y = "Liczba samob�jstw") +
      ggtitle("Liczba samob�jstw w zakresie dat") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$secondPlot <- renderPlot({
    min <- input$range[1]
    max <- input$range[2]
    
    suicidesByYear <- suicides %>%
      filter(
        if (input$varCountry == "Wszystkie")
          �.�country %in% countries
        else
          �.�country %in% input$varCountry) %>%
      filter(between(year, min, max)) %>%
      group_by(year) %>%
      summarise(suicides_no = (sum(suicides_no) / sum(population)) * 10000)
    
    ggplot(suicidesByYear, aes(x = year, y = suicides_no)) +
      geom_line() +
      labs(x = "",
           y = "Liczba samob�jstw") +
      ggtitle("Procent samob�jstw w zakresie dat") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, size = 11)) +
      scale_x_continuous(breaks = seq(min, max, 1))
    
  })
  
  
  output$thirdPlot <- renderPlot({
    min <- input$range[1]
    max <- input$range[2]
    
    numberOfSuicides <- suicides %>%
      filter(
        if (input$varCountry == "Wszystkie")
          �.�country %in% countries
        else
          �.�country %in% input$varCountry) %>%
      filter(between(year, min, max)) %>%
      summarise(count = sum(suicides_no))
    suicidesPcBySex <- suicides %>%
      filter(
        if (input$varCountry == "Wszystkie")
          �.�country %in% countries
        else
          �.�country %in% input$varCountry) %>%
      filter(between(year, min, max)) %>%
      group_by(sex) %>%
      summarise(suicides_pc = (sum(suicides_no)/numberOfSuicides$count)*100)
    
    pie(
      suicidesPcBySex$suicides_pc,
      labels = round(suicidesPcBySex$suicides_pc, digits = 1),
      clockwise = TRUE,
      col = c("red", "orange"),
      border = "white",
      cex = 0.9,
      main = "Procent kobiet i m�czyzn kt�rzy pope�nili samob�jstwo"
    )
    legend(
      "bottomright",
      legend = suicidesPcBySex$sex,
      bty = "n",
      fill = c("red", "orange")
    )
  })
  
  
  output$fourthPlot <- renderPlot({
    min <- input$range[1]
    max <- input$range[2]
    
    suicidesByCountry <- suicides %>%
      filter(between(year, min, max)) %>%
      group_by(�.�country) %>%
      summarise(suicides_no = sum(suicides_no))
    
    suicidesByCountry <-
      slice(suicidesByCountry[order(-suicidesByCountry$suicides_no), ], 1:input$tops)
    ggplot(suicidesByCountry, aes(x = reorder(�.�country,-suicides_no), y = suicides_no)) +
      geom_bar(stat = "identity", color = "white", fill = "black") +
      coord_flip() +
      labs(x = "",
           y = "Liczba samob�jstw") +
      ggtitle("Kraje z najwy�sz� liczb� samob�jstw") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  output$fifthPlot <- renderPlot({
    min <- input$range[1]
    max <- input$range[2]
    
    suicidesByCountry <- suicides %>%
      filter(between(year, min, max)) %>%
      group_by(�.�country) %>%
      summarise(suicides_no = sum(suicides_no))
    
    mapped_data <-
      joinCountryData2Map(suicidesByCountry,
                          joinCode = "NAME",
                          nameJoinColumn = "�.�country")
    
    par(mai = c(0, 0, 0.2, 0),
        xaxs = "i",
        yaxs = "i")
    mapCountryData(mapped_data, nameColumnToPlot = "suicides_no")
  })
  
  
  output$sixthPlot <- renderPlot({
    min <- input$range[1]
    max <- input$range[2]
    
    numberOfSuicides <- suicides %>%
      filter(
        if (input$varCountry == "Wszystkie")
          �.�country %in% countries
        else
          �.�country %in% input$varCountry) %>%
      filter(between(year, min, max)) %>%
      group_by(generation) %>%
      summarise(count = sum(suicides_no)) %>%
      mutate(
        generation = fct_relevel
        (
          generation,
          "G.I. Generation",
          "Silent",
          "Boomers",
          "Generation X",
          "Millenials",
          "Generation Z"
        )
      )
    
    ggplot(numberOfSuicides, aes(x = generation, y = count)) +
      geom_bar(stat = "identity", color = "white", fill = "cyan") +
      labs(x = "",
           y = "Liczba samob�jstw") + 
      ggtitle("Generacje") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  output$seventhPlot <- renderPlot({
    min <- input$range[1]
    max <- input$range[2]
    
    numberOfSuicides <- suicides %>%
      filter(
        if (input$varCountry == "Wszystkie")
          �.�country %in% countries
        else
          �.�country %in% input$varCountry) %>%
      filter(between(year, min, max)) %>%
      group_by(age) %>%
      summarise(count = sum(suicides_no)) %>%
      mutate(
        age = fct_relevel
        (
          age,
          "75+ years",
          "55-74 years",
          "35-54 years",
          "25-34 years",
          "15-24 years",
          "5-14 years"
        )
      )
    
    ggplot(numberOfSuicides, aes(x = age, y = count)) +
      geom_bar(stat = "identity", color = "black", fill = "red") +
      labs(x = "",
           y = "Liczba samob�jstw") + 
      ggtitle("Grupy wiekowe") +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

shinyApp(ui = ui, server = server)
