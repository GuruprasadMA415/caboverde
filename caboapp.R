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
library(plotly)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
 


# ---- 
# Data for bar graph 
ex_data <- read.csv("data/exports_2023.csv")
ex_summary <- ex_data |> group_by(Section) |> 
  summarize(total_trade = sum(Trade.Value, na.rm = TRUE ))

total_trade_sum = sum(ex_summary$total_trade)

ex_summary <- ex_summary |> mutate(percent = (total_trade/total_trade_sum)*100)

top_exports <- ex_data |> group_by(Section, HS4) |> 
  summarise(total_trade = sum(Trade.Value, na.rm = TRUE), .groups = "drop") |>
  arrange(Section, desc(total_trade)) |>
  group_by(Section) |>
  slice_max(order_by = total_trade, n = 3, with_ties = FALSE) |>
  ungroup()

top3_ranked <- top_exports |>
  group_by(Section) |>
  mutate(rank = row_number()) |>
  ungroup()

top3_ranked <- top3_ranked |>
  mutate(label = paste0(HS4, " (", round(total_trade, 0), ")"))

top3_wide <- top3_ranked |>
  select(Section, rank, label) |>
  pivot_wider(names_from = Section, values_from = label)

tool_tip <- top3_ranked |>
  arrange(Section, rank) |>
  group_by(Section) |>
  summarise(tooltip = paste0(rank, ". ", HS4, " (", round(total_trade, 0), ")", collapse = "<br>"), .groups = "drop")


plot_data <- ex_summary |> left_join(tool_tip, by = "Section")


# ----

#trade map data 
export_map_dat <- read.csv("data/export_country.csv")

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- left_join(world, export_map_dat, by = c("admin" = "Country"))
pal <- colorNumeric(palette = c("skyblue", "turquoise", "darkblue"), domain = world$Trade.Value, na.color = "lightgray")

import_data <- read.csv("data/import_country.csv")
import_data <- import_data |> rename(trade_value = Trade.Value)
world <- left_join(world, import_data, by = c("admin" = "Country"))
pal2 <- colorNumeric(palette = c("lightpink", "hotpink", "purple"), domain = world$trade_value, na.color = "lightgray")




#----
#population demographics 

urban_pop <- read.csv("data/urbanpop.csv")
urban_pop <- urban_pop |> filter(Country.Name == "Cabo Verde") 
urban_pop <- urban_pop |> select(-Indicator.Name, -Indicator.Code, -Country.Code) 
urban_pop <- urban_pop |> 
  pivot_longer(
      cols = starts_with("X"),
      names_to = "Year",
      values_to = "Value") |> 
  mutate(
    Year = gsub("X", "", Year),        # Remove the "X"
    Year = as.integer(Year),
    Value = as.numeric(Value)) |> 
  select(Year, Value)

total_pop <- read.csv("data/totalpop.csv") 
total_pop <- total_pop |> filter(Country.Name == "Cabo Verde") 
total_pop <- total_pop |> select(-Indicator.Name, -Indicator.Code, -Country.Code) 
total_pop <- total_pop |> 
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "Value") |> 
  mutate(
    Year = gsub("X", "", Year),        # Remove the "X"
    Year = as.integer(Year),
    Value = as.numeric(Value)) |> 
  select(Year, Value)


total_pop <- left_join(urban_pop, total_pop, by = "Year")
colnames(total_pop) <- c("Year", "Urban.Percent", "Total.Population")
total_pop <- total_pop[-nrow(total_pop), ]
total_pop <- total_pop |> mutate(UrbanPop = Total.Population * Urban.Percent / 100,
                                 RuralPop = Total.Population - UrbanPop)

pop_plot <- total_pop |>
  filter((Year %% 5 == 0 & Year >= 1960) | Year == 2023) |>  # every 10 years from 1950
  select(Year, UrbanPop, RuralPop) |> 
  pivot_longer(cols = c(UrbanPop, RuralPop),
               names_to = "AreaType",
               values_to = "Population")




 
  

# ----





# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(id = "tabs", title = "Cabo Verde", 

    #opening menu
    navbarMenu(title = "Cabo Verde", 
      tabPanel("About Cabo Verde",
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
          img(src = "background.jpg", style = "width:100%")
          )
      ),
    
        tabPanel("A Brief History",
          fluidPage(
            h2("A Brief History"), 
            img(src = "flag.png", style = "width:100%"),
            p(), 
            p(style = "font-size:14pt", "Cabo Verde, a formal Portuguese Colony until its independence in 1975, served as 
              a major transatlantic hub during the time of European maritime trade and the Transatlantic Trade. This colonial 
              legacy laid the foundation for its modern economy, which remains closely tied to global trade routes. 
              By 2023, Cabo Verde’s exports — primarily fish, clothing, and footwear — reflect a small but 
              growing export economy focused on labor and ocean-based industries."),
            p(),
            img(src = "portugalcolonization.jpeg", style = "wideth:100%"), 
            p(style = "font-size:14pt", "Alongside these economic developments, the country has experienced significant 
            urbanization, with over two-thirds of its population now living in urban areas, 
            particularly on the islands of Santiago and São Vicente, signaling a shift from rural subsistence to 
            more centralized, service-driven living.")
               )
    )),
          

    #maps menu 
    tabPanel(title = "Maps of Cabo Verde",
               tabPanel("Map",
                        fluidPage(
                          h2("A Map of Cabo Verde"),
                          leafletOutput("map")
                        )
               )
    ),
                 
    
    #data and graphs menu 
    navbarMenu(title = "Economy (2023)", 
      tabPanel("Exports",
          fluidPage(
              h2("Trade Balance and Exports of Cabo Verde"), 
              p(style = "font-size:14pt","Cabo Verde's economy is ranked number 171 our of 195 countries in terms of 
                 GDP. In 2023, Cabo Verde's top exports were processed fish and refined petroleum. Cabo Verde 
                 exports mainly to Spain, Portugal, Togo, Italy and India. ")),
              h2("Cabo Verde's Major Exports"),
              plotlyOutput("exports")
                          
                ),
        tabPanel("Trade Maps",
          fluidPage(
            h2("Showing Where Cabo Verde Exports To" ), 
            leafletOutput("exportmap"), 
            p(), 
            h2("Countries Cabo Verde Imports From"), 
            leafletOutput("importmap")
                  )
                 )
            
              ),
    
     tabPanel(title = "Population Demographics", 
            fluidPage(
              h2("Bar graph depicting the Urbanization of Cabo Verde's Population"),
              p(), 
              p(style="font-size:14pt", "Throughout the 1950s to the present, we can see a trend in Cabo Verde's population 
                shifting from rural communities to urban centers. This is partly due to the decline in funding and support from
                Portugal and a decline in Agriculture due to a series of droughts. Many people moved to urban centers looking for work and 
                economic opportunity. "), 
              sliderInput("year_range", "Select Year Range:",
                          min = 1960, max = 2023, step = 5,
                          value = c(1960, 2023), sep = ""),
              plotOutput("stackedbargraph")
              
            )
     ),
  tabPanel(title = "References",
     fluidPage(
       h2("Works Cited"),
       p(), 
       p(style="font-size:14pt","“Cape Verde.” Wikipedia: The Free Encyclopedia, Wikimedia Foundation, 6 May 2025,"),
       p("https://en.wikipedia.org/wiki/Cape_Verde."), 
       p(),
       p(style="font-size:14pt", "World Bank. 'Population, Total - Cabo Verde.' World Bank Open Data, 2023,",
       p("https://data.worldbank.org/indicator/SP.POP.TOTL?locations=CV."), 
       p(), 
       p(style="font-size:14pt", "World Bank. 'Urban Population (% of Total Population) - Cabo Verde.' World Bank Open Data, 2023,"),
       p("https://data.worldbank.org/indicator/SP.URB.TOTL.IN.ZS?locations=CV&end=2023&start=1960.")
     )
  )
)
)
)

    


# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$map <- renderLeaflet({
      leaflet() |> 
        addTiles() |>
        setView(lng = -23.6, lat = 15.1, zoom = 6) |>
        addMarkers(lng = -23.5, lat = 14.9, popup = "Praia (Capital)")})
    
    output$exports <-renderPlotly({
      plot_data <- ex_summary |> left_join(tool_tip, by = "Section")
      
      plot1 <- ggplot(plot_data, aes(x = Section, y = total_trade, fill = Section,  
                                      text = paste0("<b>Category: </b>", Section, 
                                                    "<br><b>Total Trade Value: </b>", total_trade, 
                                                    "<br><b>Biggest Exports: <br></b>", tooltip))) + 
        geom_bar(stat = "identity") + 
        coord_flip() +
        labs(
          title = paste("Cabo Verde Exports by Sector"), 
          x = "Sector",
          y = "Trade Value (USD)") + 
        theme_minimal()
      
      
        interactive_plot1 <- ggplotly(plot1, tooltip = "text")
        
    })
    
    output$exportmap <- renderLeaflet({
        leaflet(world) |>
          addTiles() |>
          addPolygons(
            fillColor = ~pal(Trade.Value),
            weight = 1,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.8,
            highlight = highlightOptions(
              weight = 2,
              color = "#666",
              fillOpacity = 0.9,
              bringToFront = TRUE),
              label = ~paste(admin, ": $", Trade.Value, "(USD)"),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto"
            )) |> 
              addLegend(
                pal = pal,
                values = world$Trade.Value,
                title = "Export Value in USD",
                position = "bottomright"
              ) |> 
        addMarkers(
          lng = -23.0418,
          lat = 16.5388,
          popup = "Cabo Verde"
        )
 
    })
    
    output$importmap <- renderLeaflet({
      leaflet(world) |>
        addTiles() |>
        addPolygons(
          fillColor = ~pal2(trade_value),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.8,
          highlight = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE),
          label = ~paste(admin, ": $", trade_value, "(USD)"),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          )) |> 
        addLegend(
          pal = pal2,
          values = world$trade_value,
          title = "Import Value in USD",
          position = "bottomright"
        ) |> 
        addMarkers(
          lng = -23.0418,
          lat = 16.5388,
          popup = "Cabo Verde"
        )
      
    })
    
    output$stackedbargraph <- renderPlot({
      filtered_df <- pop_plot |> 
        filter(Year >= input$year_range[1],
               Year <= input$year_range[2])
      
      # Make sure Urban is plotted first (at bottom)
      filtered_df$AreaType <- factor(filtered_df$AreaType, levels = c("UrbanPop", "RuralPop"))
      
      ggplot(filtered_df, aes(x = factor(Year), y = Population, fill = AreaType)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("UrbanPop" = "hotpink", "RuralPop" = "#33a02c"),
                          labels = c("Urban", "Rural")) +
        labs(x = "Year", y = "Population", fill = "Area Type") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
}
   


# Run the application 
shinyApp(ui = ui, server = server)
