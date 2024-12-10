library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(fresh)
library(tidyverse)
library(ggplot2)

#load in the data
crop_prices2 <- read_csv("data/crop_prices2.csv")
crop_prices  <- crop_prices2|>
  mutate(season = str_replace(sub_category, " Crop", ""))


#Global variables for the crop tab
seasons <- c("Spring", "Summer", "Fall", "Winter", "Special")
crops_select <-c("potato", "potatoes", "grapes")
crops_professon <-c("none", "tiller")



#graph functions
# Function to create a calendar
create_calendar <- function(events = NULL) {
  days <- data.frame(
    day = 1:28,
    week = rep(1:4, each = 7),
    weekday = factor(
      rep(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), 4),
      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    )
  )
  
  if (!is.null(events)) {
    event_days <- events |>
      filter(growth_time != 0)|>
      rowwise() |>
      mutate(days = ifelse(growth_time == 28,
                           list(seq(from = 1, to = 1, by = 1)),
                           list(seq(from = 28, to = 1, by = -growth_time)))) |>
      unnest(cols = c(days)) |>
      select(day = days, item)
    
    event_days <- event_days |>
      filter(day != 28) |> #remove the last day because you cannot plant on the last day of the season
      group_by(day) |>
      summarize(event_name = paste(unique(item), collapse = " & ")) |>
      ungroup()
    
    days <- days |>
      left_join(event_days, by = "day")
  }
  
  ggplot(days, aes(x = weekday, y = -week, fill = event_name)) +
    geom_tile(color = "black", size = 0.8) +
    geom_text(aes(label = day), size = 5, vjust = -1) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 16, face = "bold")
    ) +
    scale_x_discrete(position = "top") +
    labs(title = "Crop Growth Calendar", fill = "Crops")
}

create_crop_barchart <-function(dataset = NULL){
  dataset|>
    mutate(item = fct_reorder(item, regular_price)) |> 
    pivot_longer(cols = regular_price:iridium_price, names_to = "quality", values_to = "sell_price")|>
    group_by(item)|>
    arrange(sell_price)|>
    mutate(quality = fct_reorder(quality, sell_price)) |> 
    ggplot(aes(x = item, y = sell_price, fill = quality)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(
      values = c("regular_price" = "darkgreen", "silver_price" = "grey", "gold_price" = "gold", "iridium_price" = "purple")
    )+
    labs(
      title = "Grouped Bar Chart of Item Prices",
      x = "Item Name",
      y = "Sell Price",
      fill = "Quality")+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Example usage:
# Define recurring events
#events <- data.frame(
#  start_day = c(1, 1, 1),       # Start day of each event
#  interval = c(3, 5, 2),        # Recurrence interval (e.g., every 3 days, every 5 days, every 7 days)
#  event_name = c("Peppers", "Greenbeans", "Grapes") # Event names
#)

# Plot the calendar
#create_calendar(events)


#use for ui customizations
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#144683"
  ),
  adminlte_sidebar(
    dark_bg = "#67c64e",
    dark_hover_bg = "#4a9030",
    dark_color = "#253252"
  ),
  adminlte_global(
    content_bg = "#d0eeeb",
    box_bg = "#d19b53", 
    info_box_bg = "#e7763c"
  )
)

# Define UI
ui <- dashboardPage(freshTheme = mytheme,
  dashboardHeader(title = "SV Item Profits", titleWidth = "40%"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Crops", tabName = "crops", icon = icon("seedling")),
      menuItem("Animals", tabName = "animals", icon = icon("paw")),
      menuItem("Minerals", tabName = "minerals", icon = icon("gem")),
      menuItem("Fish", tabName = "fish", icon = icon("fish")),
      menuItem("Conclusions", tabName = "conclusions", icon = icon("check-circle"))
    )
  ),
  
  dashboardBody(
    includeCSS("www/styles.css"),
    tabItems(
      # About Tab
      tabItem(tabName = "about",
            
              img(src = "https://cdn2.steamgriddb.com/logo_thumb/681a03489989b894eee8f630ae093be6.png", height = "30%"),
              h3("Welcome to the Stardew Valley Item Profits Visualizer!"),
              h4("This dashboard provides insights into various aspects of farming, 
                including crops, animals, minerals, and fishing for the well loved game Stardew Valley.")
      ),
      # Crops Tab
      tabItem(tabName = "crops",
              h2("Crops Overview"),
              fluidRow(
                column(width = 4,
                  box(title = "Inputs", 
                      width = NULL, 
                      radioButtons("season", "Select the Season", seasons),
                      radioButtons("crop_prof", "Select your Farmer's Professon", crops_professon),
                      checkboxGroupInput("crop", "Select Crops", crops_select)
                    
                ),
                box(title = "Profits Table", width = NULL, plotOutput("cropTable"))),
                box(title = "Crop Growth Data", width = 8, plotOutput("cropPlot")),
                box(title = "Crop Growth Calendar", width = 8, plotOutput("cropCalendar"))
                
                
              )
      ),
      # Animals Tab
      tabItem(tabName = "animals",
              h2("Animals Overview"),
              fluidRow(
                box(title = "Animal Stats", width = 6, plotOutput("animalPlot")),
                box(title = "Livestock Inventory", width = 6, tableOutput("animalTable"))
              )
      ),
      # Minerals Tab
      tabItem(tabName = "minerals",
              h2("Minerals Overview"),
              fluidRow(
                box(title = "Mining Efficiency", width = 6, plotOutput("mineralPlot")),
                box(title = "Mineral Inventory", width = 6, tableOutput("mineralTable"))
              )
      ),
      # Fish Tab
      tabItem(tabName = "fish",
              h2("Fishing Overview"),
              fluidRow(
                box(title = "Fish Species", width = 6, plotOutput("fishPlot")),
                box(title = "Catch Records", width = 6, tableOutput("fishTable"))
              )
      ),
      # Conclusions Tab
      tabItem(tabName = "conclusions",
              h2("Conclusions"),
              p("Grow things, plant things, make money."),
              textOutput("summary")
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {

  #ALL THE OUTPUTS FOR THE CROPS TAB
  # Default selected season
  updateRadioButtons(session, "season", selected = "Spring")
  # Default selected professon
  updateRadioButtons(session, "crop_prof", selected = "none")
  
  # Filter crops based on selected season
  filtered_season <- reactive({
    crop_prices |>
      filter(season == input$season)
  })
  
  filtered_crops <- reactive({
    filtered_season() |>
      filter(profession == input$crop_prof)
  })
  
  # Dynamically update crop selection based on season
  observe({
    crop_options <- filtered_crops()$item %>% unique()
    updateCheckboxGroupInput(session, "crop", choices = crop_options, selected = crop_options)
  })
  
  # Filter events based on selected crops
  filtered_events <- reactive({
    filtered_crops() %>%
      filter(item %in% input$crop)
  })
  
  # Render the crop calendar
  output$cropCalendar <- renderPlot({
    validate(
      need(nrow(filtered_events()) > 0, "No crops match the selected criteria!")
    )
    create_calendar(events = filtered_events())
  })
  
  # Render the crop plot
  output$cropPlot <- renderPlot({
    validate(
      need(nrow(filtered_events()) > 0, "No crops match the selected criteria!")
    )
    create_crop_barchart(filtered_events())
  })
  
  # Static placeholder for crop table
  output$cropTable <- renderTable({
    data.frame(Crop = c("Wheat", "Corn"), Yield = c(500, 450))
  })
  
  # Placeholder plots for other tabs
  output$animalPlot <- renderPlot(plot(pressure))
  output$animalTable <- renderTable(data.frame(Animal = c("Cows", "Sheep"), Count = c(50, 30)))
  output$mineralPlot <- renderPlot(plot(airquality$Temp, airquality$Solar.R))
  output$mineralTable <- renderTable(data.frame(Mineral = c("Gold", "Silver"), Quantity = c(20, 50)))
  output$fishPlot <- renderPlot(hist(rnorm(100)))
  output$fishTable <- renderTable(data.frame(Fish = c("Salmon", "Trout"), Weight = c(10, 8)))
  output$summary <- renderText("This is where your summary will go.")
}



# Run the app
shinyApp(ui, server)