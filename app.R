library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(fresh)
library(tidyverse)
library(ggplot2)

#load in the data
crop_prices2 <- read_csv("workflow/crop_prices2.csv")

#TEMP VARIABLES UNTIL WE GET THE DATA
crops <- data.frame(
  crop_name = c("Wheat", "Corn", "Tomato", "Lettuce", "Pumpkin", "Carrot", "Rice", "Soybean"),
  season = c("Spring", "Summer", "Summer", "Spring", "Fall", "Spring", "Summer", "Fall"),
  start_day = c(1, 1, 2, 2, 3, 4, 4, 5 ),       # Start day of each event
  interval = c(3, 5, 2, 2, 3, 4, 5, 7)
  )
seasons <- c("Spring", "Summer", "Fall", "Winter")

crops_select<-c("potato", "potatoes", "grapes")

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
      rowwise() |>
      mutate(days = list(seq(from = start_day, to = 28, by = interval))) |>
      unnest(cols = c(days)) |>
      select(day = days, crop_name)
    
    event_days <- event_days |>
      group_by(day) |>
      summarize(event_name = paste(unique(crop_name), collapse = " & ")) |>
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
              p("Summarize your farming game's data and insights here."),
              textOutput("summary")
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Filter crops based on season and crop selection
  updateRadioButtons(session, "season", selected = "Spring")
  filtered_crops <- reactive({
    crops %>%
      filter(season %in% input$season)
  })
  
  # Update crop options dynamically based on selected season
  observe({
    crop_options <- filtered_crops()$crop_name
    updateCheckboxGroupInput(session, "crop", choices = crop_options, selected = crop_options)
  })
  
  # Reactive for crop events based on selected crops
  filtered_events <- reactive({
    filtered_crops() %>%
      filter(crop_name %in% (input$crop)) %>%
      select(crop_name, start_day, interval)
  })
  
  # Render the crop calendar
  output$cropCalendar <- renderPlot({
    create_calendar(events = filtered_events())
  })
  
  # Other placeholders
  output$cropPlot <- renderPlot(plot(cars))
  output$cropTable <- renderTable(data.frame(Crop = c("Wheat", "Corn"), Yield = c(500, 450)))
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