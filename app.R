library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(fresh)
library(tidyverse)
library(ggplot2)
library(knitr)
library(DT)
library(plotly)
library(sf)
library(maps)
library(ggimage)
library(purrr)
library(raster)



#load in the data
crop_prices2 <- read_csv("data/crop_prices2.csv")
crop_prices  <- crop_prices2|>
  mutate(season = str_replace(sub_category, " Crop", ""),
         seed_price = ifelse(!is.na(general_store), 
                             general_store, 
                             ifelse(!is.na(oasis_store), oasis_price, "not listed")))

fish_prices <- read_csv("data/fish_prices.csv")

animal_table <- read_csv("data/animal_table.csv")

minerals_prices <- read_csv("data/minerals_prices.csv")



#Global variables for the crop tab
seasons <- c("Spring", "Summer", "Fall", "Winter", "Special")
crops_select <-c("potato", "potatoes", "grapes")
crops_professon <-c("none", "tiller")
crops_quality <- c("regular_price", "silver_price", "gold_price", "iridium_price")

#global variables for minerals tab
mineral_types <- c("foraged mineral", "gem", "geode mineral", "geode")
mine_select <-c("gem1", "gem2", "gem3")
mineral_professon <-c("none", "gemologist")


#Global variables for the fish tab
fish_locations <- c("The Beach", "River", "Night Market", 
                    "Ginger Island", "Mountain Lake","Secret Woods",
                    "Sewers", "Mutant Bug Lair", "Witch's Swamp",
                    "Crab Pot", "Mines", "Cindersap Forest Pond", "Desert")
fish_select <-c("fishy1", "fishy2", "fishy3")
fish_professon <-c("none", "fisher", "angler")
map_shape <- readRDS("data/map_shape.rds")
stardewmap_df <-read_csv("data/stardewmap_df.csv")




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
      dplyr::select(day = days, item)
    
    event_days <- event_days |>
      filter(day != 28) |> #remove the last day because you cannot plant on the last day of the season
      group_by(day) |>
      summarize(plant_crops = paste(unique(item), collapse = " & ")) |>
      ungroup()
    
    days <- days |>
      left_join(event_days, by = "day")
  }
  
  cal <- ggplot(days, aes(x = weekday, y = -week, fill = plant_crops)) + #Maybe put tiles in tiles?
    geom_tile(color = "black", size = 0.8) +
    geom_text(aes(label = day), size = 5, vjust = -1) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 16, face = "bold"),
      legend.position = "none"
    ) +
    scale_x_discrete(position = "top") +
    labs(title = "Crop Growth Calendar", fill = "Crops")
  
  fig <- ggplotly(cal, tooltip = "fill")
  fig
}

create_crop_barchart <-function(dataset = NULL){
  
    plot <- dataset|>
      mutate(item = fct_reorder(item, regular_price)) |> 
      pivot_longer(cols = regular_price:iridium_price, names_to = "quality", values_to = "sell_price")|>
      group_by(item)|>
      arrange(sell_price)|>
      mutate(quality = fct_reorder(quality, sell_price)) |> 
      ggplot(aes(x = item, y = sell_price, fill = quality)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(
        labels = c(
          "regular_price" = "Regular",
          "silver_price" = "Silver",
          "gold_price" = "Gold",
          "iridium_price" = "Iridium"
        ),
        values = c("regular_price" = "darkgreen", 
                   "silver_price" = "grey", 
                   "gold_price" = "gold", 
                   "iridium_price" = "purple"),
      )+
      labs(
        title = "Grouped Bar Chart of Item Prices",
        x = "Item Name",
        y = "Sell Price",
        fill = "Quality")+
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(plot)
}

create_fish_map <- function(dataset = NULL){
  xy <- read_csv("data/xy.csv")
  dataset<- dataset|>
    left_join(xy) |>
    filter(!is.na(x)) |>
    mutate(x = ifelse(item == "Angler", 815, x),
           y = ifelse(item == "Angler", 500, y),
           x = ifelse(item == "Ms._Angler", 815, x),
           y = ifelse(item == "Ms._Angler", 500, y),
           x = ifelse(item == "Crimson", 805, x),
           y = ifelse(item == "Crimson", 70, y),
           x = ifelse(item == "Son_of_Crimsonfish", 805, x),
           y = ifelse(item == "Son_of_Crimsonfish", 70, y),
    ) |>  
    st_as_sf(coords = c("x", "y"))
    
  ggplot() +
    geom_sf(data = map_shape) +
    geom_image(data = stardewmap_df, aes(x, y, image = image), size = 1.496) +
    geom_sf(data = dataset, color = "red", size = 4) +
    theme(
      panel.background = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
}

create_basic_barchart <- function(dataset = NULL){
  plot <- dataset|>
    mutate(item = fct_reorder(item, sell_price),
           profession = fct_reorder(profession, sell_price) )|> 
    ggplot(aes(x = item, y = sell_price, fill = profession)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Grouped Bar Chart of Item Prices",
      x = "Item Name",
      y = "Sell Price",
      fill = "Profession")+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(plot)
}



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
    box_bg = "white", 
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
                column(width = 5,
                  box(title = "Inputs",
                      collapsible = TRUE,
                      width = NULL, 
                      radioButtons("season", "Select the Season", seasons),
                      radioButtons("crop_prof", "Select your Farmer's Profession", crops_professon),
                      checkboxGroupInput("crop", "Select Crops", crops_select)
                ),
                box(title = "Crop Table",
                    collapsible = TRUE,
                    width = NULL, 
                    selectInput("crops_qual", "Crop Quality", choices = list("Regular Quality" = 1, 
                                                                            "Silver Quality" = 2, 
                                                                            "Gold Quality" = 3,
                                                                            "Iridium Quality" = 4),
                                selected = 1),
                    div(style = 'overflow-x: scroll', DT::dataTableOutput("cropTable")))
                    
 
                ),
                box(title = "Crop Sell Prices Plot", width = 7, plotlyOutput("cropPlot")),
                box(title = "Crop Growth Calendar", width = 7,  plotlyOutput("cropCalendar"))
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
              fluidRow(column(width = 5,
                              box(title = "Inputs",
                                  collapsible = TRUE,
                                  width = NULL, 
                                  radioButtons("mine_type", "Select mineral type", mineral_types),
                                  checkboxGroupInput("mineral", "Select minerals", mine_select)
                              ),
                              box(title = "Minerals Table",
                                  collapsible = TRUE,
                                  width = NULL, 
                                  div(style = 'overflow-x: scroll', DT::dataTableOutput("mineTable")))
                              ),
                       box(title = "Mineral Sell Prices Plot", width = 7, plotlyOutput("minePlot"))
              )
      ),
      # Fish Tab
      tabItem(tabName = "fish",
              h2("Fishing Overview"),
              fluidRow(
                column(width = 5,
                       box(title = "Inputs",
                           collapsible = TRUE,
                           width = NULL, 
                             radioButtons("f_location", "Select Fish Location", fish_locations),
                             radioButtons("f_prof", "Select your Farmer's Profession", fish_professon),
                             checkboxGroupInput("fish_choice", "Select Fishes", fish_select)
                         ),
                         box(title = "Fish Table",
                             collapsible = TRUE,
                             width = NULL, 
                             selectInput("fish_qual", "Fish Quality", choices = list("Regular Quality" = 1, 
                                                                                      "Silver Quality" = 2, 
                                                                                      "Gold Quality" = 3,
                                                                                      "Iridium Quality" = 4),
                                         selected = 1),
                             div(style = 'overflow-x: scroll', DT::dataTableOutput("fishTable")))
                         
                         
                  ),
                box(title = "Fish Map Location", width = 7, plotOutput("fishMap")),
                box(title = "Fish Sell Prices", width = 7, plotlyOutput("fishPlot")),

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
  #default quality?
  updateSelectInput(session, "crops_qual", selected = "regular_price")

  
  # Filter crops based on selected season
  filtered_season <- reactive({
    crop_prices |>
      filter(season == input$season)
  })
  
  #filter the profession based on user input
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
  
  #select the crop quality based on user input
  filtered_prices <- reactive({
    if(as.character(input$crops_qual) == "4"){
      filtered_events()|>
        dplyr::select(item, iridium_price, growth_time, seed_price )
    }else if(as.character(input$crops_qual) == "2"){
      filtered_events()|>
        dplyr::select(item, silver_price, growth_time, seed_price )
    }else if(as.character(input$crops_qual) == "3"){
      filtered_events()|>
        dplyr::select(item, gold_price, growth_time, seed_price )
    }else{
      filtered_events()|>
        dplyr::select(item, regular_price, growth_time, seed_price)
      }
  })
  
  
  
  # Render the crop calendar
  output$cropCalendar <- renderPlotly({
    validate(
      need(nrow(filtered_events()) > 0, "No crops match the selected criteria!")
    )
    create_calendar(events = filtered_events())
  })
  
  # Render the crop plot
  output$cropPlot <- renderPlotly({
    validate(
      need(nrow(filtered_events()) > 0, "No crops match the selected criteria!")
    )
    create_crop_barchart(filtered_events())
  })
  
  # crop table
  output$cropTable<- DT::renderDataTable({DT::datatable(filtered_prices())})

  # MINERAL Outputs 
  
  # Default selected season
  observe({
    updateRadioButtons(session, "mine_type", selected = "foraged mineral")
  })
  
  # Filter minerals based on sub category
  filtered_mineral_cat <- reactive({
    req(input$mine_type) # Ensure input$mine_type is available
    minerals_prices |>
      filter(sub_category == input$mine_type)
  })
  
  # Dynamically update minerals selection based on category
  observe({
    mine_options <- filtered_mineral_cat()$item %>% unique()
    updateCheckboxGroupInput(session, "mineral", choices = mine_options, selected = mine_options)
  })
  
  # Filter events based on selected minerals
  filtered_minerals <- reactive({
    req(filtered_mineral_cat(), input$mineral) # Ensure dependencies are available
    filtered_mineral_cat() |>
      filter(item %in% input$mineral)
  })
  
  # Render the mine plot
  output$minePlot <- renderPlotly({
    validate(
      need(nrow(filtered_minerals()) > 0, "No items match the selected criteria!")
    )
    create_basic_barchart(filtered_minerals())
  })
  
  # Render the mine table
  output$mineTable <- DT::renderDataTable({
    req(filtered_minerals()) # Ensure filtered_minerals() is available
    DT::datatable(filtered_minerals()|>
                    dplyr::select(-category, -sub_category))
  })
  
  
  
#Fish Outputs
  updateRadioButtons(session, "f_location", selected = "River")
  # Default selected profession
  updateRadioButtons(session, "f_prof", selected = "none")
  #default quality?
  updateSelectInput(session, "fish_qual", selected = "regular_price")
  
  
  # Filter crops based on selected season
  filtered_location <- reactive({
    fish_prices |>
      filter(sub_category == input$f_location)
  })
  
  #filter the profession based on user input
  filtered_fish_prof <- reactive({
    filtered_location() |>
      filter(profession == input$f_prof)
  })
  
  # Dynamically update crop selection based on season
  observe({
    fish_options <- filtered_fish_prof()$item %>% unique()
    updateCheckboxGroupInput(session, "fish_choice", choices = fish_options, selected = fish_options)
  })
  
  # Filter events based on selected crops
  filtered_selected_fishes <- reactive({
    filtered_fish_prof() %>%
      filter(item %in% input$fish_choice)
  })
  
  #select the crop quality based on user input
  filtered_fish_prices <- reactive({
    if(as.character(input$fish_qual) == "4"){
      filtered_selected_fishes()|>
        dplyr::select(item, iridium_price, sub_category)
    }else if(as.character(input$fish_qual) == "2"){
      filtered_selected_fishes()|>
        dplyr::select(item, silver_price, sub_category )
    }else if(as.character(input$crops_qual) == "3"){
      filtered_selected_fishes()|>
        dplyr::select(item, gold_price, sub_category)
    }else{
      filtered_selected_fishes()|>
        dplyr::select(item, regular_price, sub_category)
    }
  })
  
  
  
  # Render the fish map
  output$fishMap <- renderPlot({create_fish_map(filtered_fish_prices())})
  
  # Render the fish bar plot
  output$fishPlot <- renderPlotly({
    validate(
      need(nrow(filtered_events()) > 0, "No fishes match the selected criteria!")
    )
    create_crop_barchart(filtered_selected_fishes())
  })
  
  # fish table
  output$fishTable<- DT::renderDataTable({DT::datatable(filtered_fish_prices())})

  # Placeholder plots for other tabs
  output$animalPlot <- renderPlot(plot(pressure))
  output$animalTable <- renderTable(data.frame(Animal = c("Cows", "Sheep"), Count = c(50, 30)))
  output$summary <- renderText("This is where your summary will go.")
}



# Run the app
shinyApp(ui, server)