#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(ggplot2)
library(DT)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(id = "tabs", title = "Cabo Verde", 

    navbarMenu(title = "Cabo Verde", 
    # Application title
    tabPanel("About Cabo Verde",
    # Sidebar with a slider input for number of bins 
      fluidPage(
       
        h2("Welcome to Cabo Verde !!") ,
        h3("Overview"),
        p(style="font-size:14pt","Cabo Verde is an is an island country and archipelagic state off the 
         western coast of Africa. It consists of 10 volcanic islands. 
          it has a population of over 500,000 people. Known for its 
          Creole Portuguese-African culture, music, and scenic landscapes, 
          Cabo Verde has grown into a stable democracy with a service-based economy. 
          The country’s unique geographic and cultural position makes it a vibrant 
          intersection of Africa, Europe, and Latin America"),
        img(src = "background.jpg", style = "width:100%"))),
    
      tabPanel("A Brief History",
               fluidPage(
                 h2("A Brief History")
               )),
          

        # Show a plot of the generated distribution
        navbarMenu(Title = "Maps of Cabo Verde", 
          tabPanel("Map",
                   h2("A Map of Cabo Verde")))
    


# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$map <- renderLeaflet({
      leaflet() |> 
        addTiles() |>
        setView(lng = -23.6, lat = 15.1, zoom = 6) |>
        addMarkers(lng = -23.5, lat = 14.9, popup = "Praia (Capital)")})
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)
