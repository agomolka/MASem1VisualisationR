install.packages("shiny")
install.packages("eurostat")
install.packages("dplyr")
install.packages("rnaturalearth")
install.packages("data.table")
install.packages("rnaturalearthdata")
install.packages("tidyverse")
install.packages("sf")
install.packages("scales")

library(shiny)
library(eurostat)
library(dplyr)
library(rnaturalearth)
library(data.table)
library(rnaturalearthdata)
library(tidyverse)
library(sf)
library(scales)

observe_file <- function() {
  file <- file.path("demo_r_mwk_ts.tsv.gz")
  info <- file.info(file)
  max(info$mtime)
}

load_data <- function() {
  data <- read.table(file = file.path("demo_r_mwk_ts.tsv.gz"), sep = "\t", dec = ".", header = T)
  processed_data <- as.data.frame(rbindlist(lapply(eu_countries$code, function(country){
    x <- t(data[grep(country,data[,1]),])
    x <- x[-1,]
    options(warn=-1)
    x <- data.frame(
      week = gsub("X","",rownames(x)), 
      female = as.integer(gsub(" p","",x[,1])),
      male = as.integer(gsub(" p","",x[,2])),
      total = as.integer(gsub(" p","",x[,3])),
      country = country
    )
    options(warn=0)
    rownames(x) <- NULL
    x <- x[order(x$week),]
    
    return(x)
  })))
}

filtered_eu_countries <- eu_countries %>%
  filter(code != "UK")

ui <- fluidPage(
  sidebarPanel(
    checkboxGroupInput("countries", "Available countries:", filtered_eu_countries$name),
    radioButtons("sex", "Sex", c("male", "female")),
    uiOutput("render_slider"),
    helpText("Use the button to download the latest data from eurostat"),
    actionButton("data_download", "Download")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Table", tableOutput('table')), 
      tabPanel("Map", plotOutput("map")),
      tabPanel("Plot", plotOutput("plot"))
    )
  )
)

server <- function(input, output) {
  data <- reactivePoll(1000, session = NULL, checkFunc = observe_file, valueFunc = load_data)
  
  slider_values <- reactive({
    from_year <- input$date_range[1]
    to_year <- input$date_range[2]
    
    if (is.null(from_year) || is.null(to_year)) {
      from_year <- 2000
      to_year <- 2022
    }
    
    c(from_year, to_year)
  })
  
  processed_data_all_countries <- reactive({
    from_year <- slider_values()[1]
    to_year <- slider_values()[2]

    data() %>%
      mutate(
        year = regmatches(week, regexpr("^[0-9]{4}", week)),
      ) %>%
      filter(between(year, from_year, to_year)) %>%
      transmute(
        country = eu_countries$name[match(country, eu_countries$code)],
        sex = input$sex,
        week = week,
        number = case_when(
          input$sex == "male" ~ male,
          input$sex == "female" ~ female
        )
      )
  })
  
  processed_data <- reactive({
    from_year <- slider_values()[1]
    to_year <- slider_values()[2]

    selected_countries_short <- eu_countries %>%
      filter(name %in% input$countries)
    
    data() %>%
      filter(country %in% selected_countries_short$code) %>%
      mutate(
        year = regmatches(week, regexpr("^[0-9]{4}", week)),
        week_short = regmatches(week, regexpr("[0-9]{2}$", week))
      ) %>%
      filter(between(year, from_year, to_year)) %>%
      transmute(
        country = eu_countries$name[match(country, eu_countries$code)],
        sex = input$sex,
        week = week,
        number = case_when(
          input$sex == "male" ~ male,
          input$sex == "female" ~ female
        )
      )
  })
  
  output$table = renderTable({
    processed_data() 
  })
  
  output$render_slider <- renderUI({
    years_range <- regmatches(data()$week, regexpr("^[0-9]{4}", data()$week))
    min <- as.integer(min(years_range))
    max <- as.integer(max(years_range))
    if (is.null(min) || is.null(max)) {
      min <- 2000
      max <- 2022
    }
    sliderInput(
      "date_range",
      "Dates:",
      min = min,
      max = max,
      value = c(min, max),
      sep = "",
      step = 1
    )
  })

  output$map <- renderPlot({
    map_data <- processed_data_all_countries() %>%
      replace(is.na(.), 0) %>%
      group_by(country) %>%
      summarise(number = sum(number))

    world_map <- ne_countries(scale = 50, returnclass = 'sf')

    european_union_map <- 
      world_map %>% 
      filter(name %in% filtered_eu_countries$name)
    
    bbox_europe <- st_bbox(c(xmin = -10, ymin = 20, xmax = 50, ymax = 80), crs = st_crs(european_union_map))
    european_union_map_cropped <- st_crop(european_union_map, bbox_europe)
    
    df <- map_data %>%
      transmute(
        country = country,
        some_value = number
      )
    
    map <- european_union_map_cropped %>% 
      left_join(df, by = c("name" = "country"))
    
    ggplot(data = map) +
      geom_sf(mapping = aes(fill = some_value)) +
      scale_fill_gradient(name = "Number", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50", labels = comma) +
      labs(title = "the sum of deaths for the given period, for the given sex") +
      theme(plot.title.position = "plot", legend.key.height= unit(1.5, 'cm'), legend.key.width= unit(1.5, 'cm'), legend.key.size=unit(1, "point"))
  }, height = 900, width = 800)
  
  output$plot <- renderPlot({
    plot_data <- processed_data() %>%
      replace(is.na(.), 0) %>%
      mutate(year = regmatches(week, regexpr("^[0-9]{4}", week))) %>%
      group_by(country, year) %>%
      summarise(number = sum(number))

    ggplot(plot_data, aes(x = year, y = number, color = country)) +
      geom_point() + 
      geom_line(aes(color = country, group = country)) +
      scale_y_continuous(labels = comma) +
      ggtitle("time series for selected sex, for each country") +
      theme_light() 
  })

  observeEvent(input$data_download, {
    download.file(
      url = "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/demo_r_mwk_ts.tsv.gz",
      destfile = file.path(".", "demo_r_mwk_ts.tsv.gz"), 
      method = "curl"
    )
  })
}

shinyApp(ui = ui, server = server)
