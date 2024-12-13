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



# load in the data
crop_prices2 <- read_csv("data/crop_prices3.csv")

fish_prices <- read_csv("data/fish_prices.csv")

animal_product_prices <-  read_csv("data/animal_table.csv")

minerals_prices <- read_csv("data/minerals_prices.csv")



# global variables for the crop tab
seasons <- c("Spring", "Summer", "Fall", "Winter", "Special")
crops_select <-c("potato", "potatoes", "grapes")
crops_professon <-c("none", "tiller")
crops_quality <- c("regular_price", "silver_price", "gold_price", "iridium_price")

# global variables for Animals tab
animal_select <-c("sheepy", "eggs", "cheese")
animal_profession <-c("none", "Rancher", "Artisan")

# global variables for minerals tab
mineral_types <- c("foraged mineral", "gem", "geode mineral", "geode")
mine_select <-c("gem1", "gem2", "gem3")
mineral_professon <-c("none", "gemologist")


# global variables for the fish tab
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
    labs(title = "Calendar", fill = "Crops")
  
  fig <- ggplotly(cal, tooltip = "fill")
  fig
}

create_crop_barchart <-function(dataset = NULL){
  
    plot <- dataset|>
      mutate(item = fct_reorder(item, regular_price)) |> 
      # pivoting so we can filter for quality
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
        # assigning prices to their respective colors from the game
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
  # plotly for interactiveness
  ggplotly(plot)
}

# function for our map of where to find the fish
create_fish_map <- function(dataset = NULL){
  
  # read in coordinates of locations in game
  xy <- read_csv("data/xy.csv")
  dataset <- dataset|>
    # joining locations with our fish prices dataset
    left_join(xy) |>
    #filtering for the legendary subcategory which is not a location
    filter(!is.na(x)) |>
    # manually enter different coordinates of legnedary fish 
    # since they are found at the location but not in that specific spot
    mutate(x = ifelse(item == "Angler", 815, x),
           y = ifelse(item == "Angler", 500, y),
           x = ifelse(item == "Ms._Angler", 815, x),
           y = ifelse(item == "Ms._Angler", 500, y),
           x = ifelse(item == "Crimson", 805, x),
           y = ifelse(item == "Crimson", 70, y),
           x = ifelse(item == "Son_of_Crimsonfish", 805, x),
           y = ifelse(item == "Son_of_Crimsonfish", 70, y),
    ) |>  
    # make it an sf object with xy as the coord system
    st_as_sf(coords = c("x", "y"))
  
  ggplot() +
    # using map_shape which is a polygon of the dimensions of our image
    geom_sf(data = map_shape) +
    # layering game map on top of the polygon and sizing it to be perfect
    geom_image(data = stardewmap_df, aes(x, y, image = image), size = 1.496) +
    # layer for our points from the fish prices
    geom_sf(data = dataset, color = "red", size = 4) +
    # removing axes, and axes ticks and lines
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
    # fct reorder for ordering of bars after plotting
    mutate(item = fct_reorder(item, sell_price),
           profession = fct_reorder(profession, sell_price) )|> 
    ggplot(aes(x = item, y = sell_price, fill = profession)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Item Sell Prices By Item Quality",
      x = "Item Name",
      y = "Sell Price",
      fill = "Profession")+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(plot)
}

create_profit_bar <- function(dataset = NULL){
  plot<- dataset |>
    # fct reorder for ordering of bars after plotting
    mutate(quality = fct_reorder(quality, profit_increase),
           item = fct_reorder(item, profit_increase)) |>
    ggplot(aes(x = item, y = profit_increase, fill = quality)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(
      # assigning prices to their respective colors from the game
      values = c("Regular" = "darkgreen", 
                 "Silver" = "grey", 
                 "Gold" = "gold", 
                 "Iridium" = "purple")
    ) +
    labs(
      title = "Profit % Increase after Proccesing Product",
      x = "Item Name",
      y = "Profit % Increase",
      fill = "Quality")+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(plot)
}

create_sell_bar <- function(dataset = NULL){
  plot <- dataset |>
    # fct reorder for ordering of bars after plotting
    mutate(quality = fct_reorder(quality, sell_price),
           item = fct_reorder(item, sell_price)) |>
    ggplot(aes(x = item, y = sell_price, fill = quality)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(
      # assigning prices to their respective colors from the game
      values = c("Regular" = "darkgreen", 
                 "Silver" = "grey", 
                 "Gold" = "gold", 
                 "Iridium" = "purple")
    ) +
    labs(
      title = "Profit Increase after Proccesing Product",
      x = "Item Name",
      y = "Profit Increase",
      fill = "Quality")+
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
      # for all tabs 
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Crops", tabName = "crops", icon = icon("seedling")),
      menuItem("Animals", tabName = "animals", icon = icon("paw")),
      menuItem("Minerals", tabName = "minerals", icon = icon("gem")),
      menuItem("Fishes", tabName = "fishes", icon = icon("fish")),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("check-circle"))
    )
  ),
  
  dashboardBody(
    includeCSS("www/styles.css"),
    tabItems(
      # About Tab
      tabItem(tabName = "about",
              img(src = "https://cdn2.steamgriddb.com/logo_thumb/681a03489989b894eee8f630ae093be6.png", height = "30%"),
              h3("Welcome to the Stardew Valley Item Profits Visualizer!"),
              h4("Introduction"),
              h5(HTML("Stardew Valley is an indie farming simulation game developed by ConcernedApe, 
              where players inherit their grandfather's old farm in the small town of Stardew Valley. 
              In the game, you can grow crops, raise animals, mine, fish, engage with the townspeople, 
              and of course, make lots of money! While the game encourages players to pursue their goals at 
              their own pace, one of the most rewarding aspects is maximizing the amount of money made at the 
              end of each day. Whether you are cultivating crops, raising livestock, or engaging in
              artisan production, there are multiple ways to make money in the game. 
              Thus, this site opens the opportunity to discover: <b>what items in the game can make you the most money?</b>")),
              h4("Data Acquisition"),
              h5(HTML("The data was acquired from web scraping the 
              <a href='https://stardewvalleywiki.com/Stardew_Valley_Wiki' target='_blank'>Stardew Valley Wiki</a>, 
              with the majority of the process described 
              <a href='https://rahartos.github.io/MiniProject2/MiniProject2Submission.html' target='_blank'>here</a>. 
              For each category, multiple functions were created to scrape the necessary 
              data using the rvest package in R. Variables scraped include: item name, sell price, and profession. 
              Sell price was split into 4 different variables based on their quality (regular, silver, gold, and iridium quality). 
              The variables category and subcategory were also included for each of the items. 
              Depending on the category, additional variables were scraped, such as seed price for the crop category."
              )),
              h5(HTML("Feel free to explore each of the item tabs! All data used for this site can be found in this 
                      <a href='https://github.com/Rahartos/StardewItemProfits/tree/main/data' target='_blank'>repo</a>."))
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
                box(title = "About",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 7,
                  h5("Crops are plants that are grown from seeds to be harvested for the purpose of profit, 
                  food, or gifting. Generally, each crop is seasonal. 
                  It can be planted only during its designated season, 
                     and when seasons change (after the 28th day), the crop will wither and die."),
                  h5("There are multiple ways to examine sell prices by using the inputs.
                  By selecting the season, you can view the crops for that season. 
                  Although out-of-season crops can be grown in the greenhouse throughout the year, 
                  special crops, like those that grow on trees, take an entire season 
                  to grow to maturity. There are also other special crops that can be grown through quests. 
                  There is also an option to select your farming profession.
                  If you reach farming level 5, you can select the tiller profession 
                  which increases the price that you sell your crops for.
                  Lastly, there is an option to view only the crop you are interested in seeing, 
                  this will change the graph, calendar and table.
")),
                box(title = "Crop Sell Prices", 
                    width = 7, 
                    plotlyOutput("cropPlot"),
                    h5("This is a graph showing the sell prices of selected crops. 
                       The quality of crops comes in four levels: regular, silver, gold, and iridium, 
                       ranging from worst to best, respectively. When deciding whether to sell your crops, 
                       give to villagers, or cook with them, use this graph to determine which crops are better 
                       to keep or sell.")),
                box(title = "Crop Growth Calendar", 
                    width = 7,  
                    plotlyOutput("cropCalendar"),
                    h5("This plot shows what dates to plant your crops during the season base their growth time.
                       By hovering over each date, you can see what crops to plant. 
                       It is recommended to only select 1-4 crops at a time."))
                )
      ),
      # Animals Tab
      tabItem(tabName = "animals",
              h2("Animals Overview"),
              fluidRow(
                  column(width = 5,
                         box(title = "Inputs",
                             collapsible = TRUE,
                             width = NULL, 
                             radioButtons("anim_prof", "Select your Farmer's Profession", animal_profession),
                             checkboxGroupInput("animal", "Select Animal Products", animal_select)
                         ),
                         box(title = "Animal Products Table",
                             collapsible = TRUE,
                             width = NULL, 
                             div(style = 'overflow-x: scroll', DT::dataTableOutput("animalTable")))
     
                  ),
                  box(title = "About", 
                      width = 7, 
                      h5("Animal products can be found and collected from barn or coop animals, and are valuable 
                         since they can be turned into processed goods using items such as a loom or a 
                         mayonnaise machine. Sell prices can be further increased with the rancher profession 
                         (unlocked and level 5 farming) and the artisan profession (unlocked at level 10 farming 
                         on the tiller’s branch). The inputs give the option to indicate what profession your 
                         farmer is at, and the animal products you are interested in seeing. The table also 
                         gives valuable information on which animals the animal products come from, and 
                         what the animal products turn into after processing.")),
                  box(title = "Animal Items Base Sell Prices", 
                      width = 7, 
                      plotlyOutput("animalPlot1"),
                      h5("This graph shows the sell prices of the selected animal products. 
                      The quality of animal products comes in four levels: 
                      regular, silver, gold, and iridium, ranging from most to least gold, respectively.")),
                  box(title = "Animal Items Max Possible Profit Percentages", 
                      width = 7, 
                      plotlyOutput("animalPlot2"),
                      h5("This graph shows how much the profit percentage increases after processing an animal product. 
                      To maximize your profits with animal products, it is best to turn the products into processed goods,
                      as it significantly increases your profit margins compared to selling them raw. 
                      After an item is processed, its quality is no longer retained, so it is best to process animal 
                      products with low quality."))
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
                       box(title = "About",
                           width = 7,
                           h5("Minerals can be found based on the 4 different categories: foraged minerals, gems, 
                              geode minerals, and geodes. Foraged minerals are found on the ground, while gems can 
                              be found in crates and barrels in The Mines and Skull Cavern. Geode minerals are extracted 
                              from geodes, while geodes are found primarily by breaking rocks in The Mines. Here you 
                              can explore the different mineral categories and which minerals sell for the most.")),
                       box(title = "Mineral Sell Prices Plot", 
                           width = 7, 
                           plotlyOutput("minePlot"),
                           h5("This bar graph arranges the minerals from least to greatest in terms of sell prices. 
                              With the Gemologist profession (unlocked and mining level 5), sell prices for minerals are further increased.
                              Objectively speaking, minerals tend to not sell for much, and are better used as gifts for villagers."))
              )
      ),
      # Fish Tab
      tabItem(tabName = "fishes",
              h2("Fishing Overview"),
              fluidRow(
                column(width = 5,
                       box(title = "Inputs",
                           collapsible = TRUE,
                           width = NULL, 
                           selectInput("f_location", "Select Fish Location", choices = fish_locations, selected = "The Beach"),
                             radioButtons("f_prof", "Select your Fisher's Profession", fish_professon),
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
                box(title = "About",
                    width = 7,
                    h5("Fish can be caught in oceans, lakes, rivers, and certain underground locations. 
                        Most fish can be caught with a fishing pole, while few require crab pots to catch 
                        (which can be placed by bodies of water). "),
                    h5("By selecting the location, you can view the fish you can catch in that location. 
                       There is also the option to specify your fishing profession, between none, fisher 
                       (unlocked at fishing skill 5), and angler (unlocked at fishing skill 10 on the 
                       fisher profession track), as they increase the price that you sell your fish for. 
                       Lastly, there is an option to view only the fish you are interested in seeing, 
                       as this will change the plot and table output.")),
                box(title = "Fish Map Location", 
                    width = 7, 
                    plotOutput("fishMap"),
                    h5("Certain quests and recipes require fish, sometimes it’s difficult to remember where 
                      these fish are located. This map makes it easier by showing the general locations of 
                      selected fish with red dots. Simply select the location, and the fish that can be found 
                      there will be given.")),
                box(title = "Fish Sell Prices", 
                    width = 7, 
                    plotlyOutput("fishPlot"),
                    h5("This barchart shows the prices of selected fish. The quality of fish comes in four 
                      levels: regular, silver, gold, and iridium, ranging from worst to best, respectively. 
                      When deciding whether to sell your fish, give to villagers, or cook with them, 
                      use this graph to determine what will make you the most money.")
                    ),
              )
      ),
      # Conclusions Tab
      tabItem(tabName = "conclusion",
              h2("Conclusion"),
              textOutput("summary"),
              img(src = "StardewJobs.png", width = "100%")
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {

  #ALL THE OUTPUTS FOR THE CROPS TAB
  # Default selected season
  updateRadioButtons(session, "season", selected = "Spring")
  # Default selected profession
  updateRadioButtons(session, "crop_prof", selected = "none")
  #default quality?
  updateSelectInput(session, "crops_qual", selected = "Regular Quality")

  
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
  
  
#Animal Outputs
  # Default selected profession
  updateRadioButtons(session, "anim_prof", selected = "none")
  #default quality?
  updateSelectInput(session, "anim_qual", selected = "Regular Quality")
  
  
  #filter the profession based on user input
  filtered_animal_prof <- reactive({
    animal_product_prices |>
      filter(profession == input$anim_prof)
  })
  
  # Dynamically update animal product selection 
  observe({
    anim_options <- filtered_animal_prof()$item %>% unique()
    updateCheckboxGroupInput(session, "animal", choices = anim_options, selected = anim_options)
  })
  
  # Filter based on selected animals
  filtered_animals <- reactive({
    filtered_animal_prof() %>%
      filter(item %in% input$animal)
  })

  
  # Render the animal plot
  output$animalPlot1 <- renderPlotly({
    validate(
      need(nrow(filtered_animals()) > 0, "No items match the selected criteria!")
    )
    create_sell_bar(filtered_animals())
  })
  
  output$animalPlot2 <- renderPlotly({
    validate(
      need(nrow(filtered_animals()) > 0, "No items match the selected criteria!")
    )
    create_profit_bar(filtered_animals())
  })
  
  # crop animal
  output$animalTable<- DT::renderDataTable({DT::datatable(filtered_animals())})
  
  
  
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

  #text outputs
  output$summary <- renderText("This is where your summary will go.")
}



# Run the app
shinyApp(ui, server)