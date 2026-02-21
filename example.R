library(shiny)
runExample("01_hello")
library(shiny)
library(bslib)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  # App title ----
  title = "Hello Shiny!",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    # Input: Slider for the number of bins ----
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 1,
      max = 50,
      value = 30
    )
  ),
  # Output: Histogram ----
  plotOutput(outputId = "distPlot")
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}

library(shiny)

shinyApp(ui = ui, server = server)

library(shiny)
library(bslib)

# Define UI ----
ui <- page_sidebar(
  
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

ui <- page_sidebar(
  title = "title panel",
  sidebar = sidebar("sidebar"),
  "main contents"
)

ui <- page_sidebar(
  title = "title panel",
  sidebar = sidebar("Sidebar", position = "right"),
  "Main contents"
)

ui <- page_fluid(
  layout_sidebar(
    sidebar = sidebar("Sidebar"),
    "Main contents"
  )   
)

ui <- page_sidebar(
  title = "title panel",
  sidebar = sidebar("Sidebar"),
  card()
)

ui <- page_sidebar(
  title = "title panel",
  sidebar = sidebar("Sidebar"),
  card(
    "Card content"
  )
)

ui <- page_sidebar(
  title = "title panel",
  sidebar = sidebar("Sidebar"),
  card(
    card_header("Card header"),
    "Card body"
  )
)

ui <- page_sidebar(
  title = "title panel",
  sidebar = sidebar("Sidebar"),
  value_box(
    title = "Value box",
    value = 100,
    showcase = bsicons::bs_icon("bar-chart"),
    theme = "teal"
  ),
  card("Card"),
  card("Another card")
)


# Define UI ----
ui <- page_fluid(
  titlePanel("Basic widgets"),
  layout_columns(
    col_width = 3,
    card(
      card_header("Buttons"),
      actionButton("action", "Action"),
      submitButton("Submit")
    ),
    card(
      card_header("Single checkbox"),
      checkboxInput("checkbox", "Choice A", value = TRUE)
    ),
    card(
      card_header("Checkbox group"),
      checkboxGroupInput(
        "checkGroup",
        "Select all that apply",
        choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
        selected = 1
      )
    ),
    card(
      card_header("Date input"),
      dateInput("date", "Select date", value = "2014-01-01")
    ),
    card(
      card_header("Date range input"),
      dateRangeInput("dates", "Select dates")
    ),
    card(
      card_header("File input"),
      fileInput("file", label = NULL)
    ),
    card(
      card_header("Help text"),
      helpText(
        "Note: help text isn't a true widget,",
        "but it provides an easy way to add text to",
        "accompany other widgets."
      )
    ),
    card(
      card_header("Numeric input"),
      numericInput("num", "Input number", value = 1)
    ),
    card(
      card_header("Radio buttons"),
      radioButtons(
        "radio",
        "Select option",
        choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
        selected = 1
      )
    ),
    card(
      card_header("Select box"),
      selectInput(
        "select",
        "Select option",
        choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
        selected = 1
      )
    ),
    card(
      card_header("Sliders"),
      sliderInput(
        "slider1",
        "Set value",
        min = 0,
        max = 100,
        value = 50
      ),
      sliderInput(
        "slider2",
        "Set value range",
        min = 0,
        max = 100,
        value = c(25, 75)
      )
    ),
    card(
      card_header("Text input"),
      textInput("text", label = NULL, value = "Enter text...")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

ui <- page_sidebar(
  title = "censusVis",
  sidebar = sidebar(
    helpText(
      "Create demographic maps with information from the 2010 US Census."
    ),
    selectInput(
      "var",
      label = "Choose a variable to display",
      choices = 
        c("Percent White",
          "Percent Black",
          "Percent Hispanic",
          "Percent Asian"),
      selected = "Percent White"
    ),
    sliderInput(
      "range",
      label = "Range of interest:",
      min = 0, 
      max = 100, 
      value = c(0, 100)
    )
  ),
  textOutput("selected_var")
)

server <- function(input, output) {
  
  output$selected_var <- renderText({
    "You have selected this"
  })
  
}

server <- function(input, output) {
  
  output$selected_var <- renderText({
    paste("You have selected", input$var)
  })
  
}




library(shiny)
library(bslib)
library(ggplot2)

# Get the data

file <- "https://github.com/rstudio-education/shiny-course/raw/main/movies.RData"
destfile <- "movies.RData"

download.file(file, destfile)

# Load data

load("movies.RData")

# Define UI

ui <- page_sidebar(
  sidebar = sidebar(
    # Select variable for y-axis
    selectInput(
      inputId = "y",
      label = "Y-axis:",
      choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
      selected = "audience_score"
    ),
    # Select variable for x-axis
    selectInput(
      inputId = "x",
      label = "X-axis:",
      choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
      selected = "critics_score"
    )
  ),
  # Output: Show scatterplot
  card(plotOutput(outputId = "scatterplot"))
)

# Define server

server <- function(input, output, session) {
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
}

# Create a Shiny app object

shinyApp(ui = ui, server = server)

install.packages('rsconnect')
rsconnect::setAccountInfo(name='lin-xianwei',
                          token='CB0B6AB651FD88FA830DB2361AE347F6',
                          secret='KpRjX88eBotAORN9b2mWch7rIHHxWyFhu4Uf/Ewx')
