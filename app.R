library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(fresh)

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
    box_bg = "#e7763c", 
    info_box_bg = "#e7763c"
  )
)

# Define UI
ui <- dashboardPage(freshTheme = mytheme,
  dashboardHeader(title = "Stardew Item Profits", titleWidth = "40%"),
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
                box(title = "Crop Growth Data", width = 6, plotOutput("cropPlot")),
                box(title = "Crop Yield Table", width = 6, tableOutput("cropTable"))
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
  # Placeholder outputs
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