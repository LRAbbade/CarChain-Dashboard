library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(httr)
library(jsonlite)
library(plotly)

getCarData <- function(carPlate) {
  ip <- "35.237.108.201"
  port <- "6653"
  url <- paste('http://', ip, ':', port, '/car/timeSeries/sum/', carPlate, sep="")
  print(paste("get request at:", url))
  r <- GET(url)      # get request to API    (httr)
  print(paste("Response:", r$status_code))
  r_content <- content(r, "text")         # retrieve json content as string
  r_content_json <- fromJSON(r_content)   # parses json into a list  (jsonlite)
  return (r_content_json)
}

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "CarChain"),
                    dashboardSidebar(sidebarMenu(
                      menuItem("Buscar Veículo", tabName = "searchTab", icon = icon("fas fa-area-chart"))
                        ),
                      collapsed = TRUE),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "searchTab",
                                fluidRow(
                                  box(title = "Buscar Veículo", status = "primary", width = '12',
                                      textInput("plate", "Placa do veículo", ""),   # input$plate
                                      actionButton("search", "Buscar", icon = icon("search"), class = "btn-lg")    # input$search
                                  )
                                ),
                                fluidRow(
                                  box(title = "Resultados", status = "primary",
                                      width = "12",
                                      div(style="font-size: 18px;", textOutput("out")),
                                      plotlyOutput("plot") # output$plot
                                  )
                                )    
                        )
                      )
                    )
)

server <- function(input, output) {
  observeEvent(input$search, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Buscando Dados", value = 100)
    
    carPlate <- input$plate
    
    r <- getCarData(carPlate)
    
    if (length(r$x_axis) < 1) {
      output$out <- renderText(paste("Placa ", carPlate, " nao encontrada.", sep=""))
    } else {
      r$x_axis <- as.POSIXct(r$x_axis)
      
      output$out <- renderText(paste("Kilometragem mais recente: ", tail(r$y_axis, n=1), " metros.", sep=""))
      output$plot <- renderPlotly({ 
          plot_ly(x = r$x_axis, y = r$y_axis, type = "scatter", name = paste("Carro ", carPlate, sep=""), mode = "lines")
        })
    }
  })
}

shinyApp(ui, server)