library(shiny)
library(ggplot2)
library(shinythemes)
library(magick)
library(shiny.router)
library(dplyr)


MainData <- read.csv("complete_phones_data.csv")
MainData$release_date <- as.numeric(MainData$release_date)
MainData <- subset(MainData, !is.na(MainData$release_date))
Top_phone_data <- read.csv("top_20_phones_2017-2023.csv")
phones_data <- MainData %>%
  mutate(
    os_category = case_when(
      grepl("iOS", os_type, ignore.case = TRUE) ~ "iOS",
      grepl("Android", os_type, ignore.case = TRUE) ~ "Android",
      grepl("HarmonyOS", os_type, ignore.case = TRUE) ~ "HarmonyOS",
      grepl("EMUI", os_type, ignore.case = TRUE) ~ "EMUI",
      TRUE ~ "Other"
    ),
    release_date_numeric = as.numeric(release_date),
    storage_numeric = sapply(storage, function(x) {
      if (is.na(x) || x == "") return(NA)
      x_clean <- gsub(" storage.*", "", x)
      storage_values <- as.numeric(gsub("[^0-9]", "", unlist(strsplit(x_clean, "/"))))
      max(storage_values, na.rm = TRUE)
    }),
    display_size_numeric = as.numeric(gsub("\"", "", display_size)),
    camera_numeric = as.numeric(gsub("[^0-9]", "", gsub("MP.*", "", camera))),
    ram_numeric = sapply(ram, function(x) {
      if (is.na(x) || x == "") return(NA)
      ram_values <- as.numeric(gsub("[^0-9]", "", unlist(strsplit(x, "/"))))
      max(ram_values, na.rm = TRUE)
    }),
    battery_numeric = as.numeric(gsub("[^0-9]", "", battery))
  )

# Define the filtering function
filter_phones <- function(data, brand_input = NULL, os_type_input = NULL, release_date_input = NULL, 
                          storage_min = NULL, storage_max = NULL, 
                          display_min = NULL, display_max = NULL, 
                          camera_min = NULL, camera_max = NULL, 
                          ram_min = NULL, ram_max = NULL, 
                          battery_min = NULL, battery_max = NULL) {
  data %>%
    filter((is.null(brand_input) | brand == brand_input) &
             (is.null(os_type_input) | os_category == os_type_input) &
             (is.null(release_date_input) | release_date_numeric >= release_date_input) &
             (is.null(storage_min) | is.na(storage_numeric) | storage_numeric >= storage_min) &
             (is.null(storage_max) | is.na(storage_numeric) | storage_numeric <= storage_max) &
             (is.null(display_min) | is.na(display_size_numeric) | display_size_numeric >= display_min) &
             (is.null(display_max) | is.na(display_size_numeric) | display_size_numeric <= display_max) &
             (is.null(camera_min) | is.na(camera_numeric) | camera_numeric >= camera_min) &
             (is.null(camera_max) | is.na(camera_numeric) | camera_numeric <= camera_max) &
             (is.null(ram_min) | is.na(ram_numeric) | ram_numeric >= ram_min) &
             (is.null(ram_max) | is.na(ram_numeric) | ram_numeric <= ram_max) &
             (is.null(battery_min) | is.na(battery_numeric) | battery_numeric >= battery_min) &
             (is.null(battery_max) | is.na(battery_numeric) | battery_numeric <= battery_max)) %>%
    arrange(desc(release_date_numeric))
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .card { margin-bottom: 15px; flex: 0 0 30%; box-sizing: border-box; }
    .card-img-top { width: 100%; height: auto; }
    .card-title a { text-decoration: none; color: #007bff; }
    .card-title a:hover { text-decoration: underline; }
    .card-body { padding: 10px; }
    .card-deck { display: flex; flex-wrap: wrap; justify-content: space-around; }
  "))),
  theme = shinytheme("darkly"),
  titlePanel("PhoneVista"),
  
  tabsetPanel(
    tabPanel("All Phones",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("brand",
                                    "Select Brand: ",
                                    choices = unique(MainData$brand),
                                    selected = c("Oneplus")),
                 sliderInput("year",
                             "Release Year: ",
                             min = min(MainData$release_date),
                             max = max(MainData$release_date),
                             value = c(min(MainData$release_date), max(MainData$release_date))),
                 selectInput("model",
                             "Select Model: ",
                             choices = NULL)
               ),
               mainPanel(
                 plotOutput("image"),
                 br(),br(),
                 dataTableOutput("table"),
                 br(),br(),
                 h1(strong("Other Releases this Year")),
                 br(),br(),
                 dataTableOutput("other")
                 
               )
             )
    ),
    tabPanel("Top Phones",
             tabsetPanel(
               tabPanel("Phones",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("top20_year",
                                        "Year: ",
                                        choices = unique(Top_phone_data$year)),
                            selectInput("top20_model",
                                        "Select Model: ",
                                        choices = NULL)
                          ),
                          mainPanel(
                            plotOutput("top20_image"),
                            br(),br(),
                            dataTableOutput("top20_table"),
                          )
                        )
               ),
               tabPanel("Visualizing Trends",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("Dominance_year",
                                        "Year Range: ",
                                        min = min(Top_phone_data$year),
                                        max = max(Top_phone_data$year),
                                        value = c(min(Top_phone_data$year), max(Top_phone_data$year)),
                                        step = 1),
                            
                          ),
                          
                          mainPanel(
                            h2("Top Brand Year by Year"),
                            plotOutput("Dominance_plot"),
                            h2("SIM Card Trend"),
                            plotOutput("Sim_plot"),
                            h2("Weight Trend"),
                            plotOutput("Weights"),
                            h2("Weight VS Battery Size"),
                            plotOutput("WBS"),
                            h2("Battery Size"),
                            plotOutput("BatSize"),
                            
                          )
                        )
               )
             )
    ),
    tabPanel(
      "Compare Phones",
      sidebarLayout(
        sidebarPanel(
          h3(strong("Select 1st Phone")),
          selectInput("compare_brand1",
                      "Select Brand: ",
                      choices = unique(MainData$brand),
                      selected = c("Oneplus")),
          sliderInput("compare_year1",
                      "Release Year: ",
                      min = min(MainData$release_date),
                      max = max(MainData$release_date),
                      value = c(min(MainData$release_date), max(MainData$release_date))),
          selectInput("compare_model1",
                      "Select Model: ",
                      choices = NULL),
          
          h3(strong("Select 2nd Phone")),
          selectInput("compare_brand2",
                      "Select Brand: ",
                      choices = unique(MainData$brand),
                      selected = c("Oneplus")),
          sliderInput("compare_year2",
                      "Release Year: ",
                      min = min(MainData$release_date),
                      max = max(MainData$release_date),
                      value = c(min(MainData$release_date), max(MainData$release_date))),
          selectInput("compare_model2",
                      "Select Model: ",
                      choices = NULL)
          
        ),
        mainPanel(
          fluidRow(
            column(6, plotOutput("compare_image1")),  
            column(6, plotOutput("compare_image2"))   
          ),
          br(),
          dataTableOutput("compare_table")
        )
      )
    ),
    tabPanel("Phone Suggester",
             sidebarLayout(
               sidebarPanel(
                 selectInput("brand", "Brand", choices = c( sort(unique(phones_data$brand)))),
                 selectInput("os_type", "Operating System", choices = c( sort(unique(phones_data$os_category))), selected = "Any"),
                 numericInput("release_date", "Release Year (From)", value = 2020, min = 2000, max = 2024),
                 numericInput("storage_min", "Min Storage (GB)", value = 128),
                 numericInput("storage_max", "Max Storage (GB)", value = 1024),
                 numericInput("display_min", "Min Display Size (inches)", value = 5.5),
                 numericInput("display_max", "Max Display Size (inches)", value = 7.0),
                 numericInput("camera_min", "Min Camera Resolution (MP)", value = 12),
                 numericInput("camera_max", "Max Camera Resolution (MP)", value = 108),
                 sliderInput("ram_range", "RAM (GB)", min = 2, max = 16, value = c(4, 12), step = 1),
                 numericInput("battery_min", "Min Battery Capacity (mAh)", value = 3000),
                 numericInput("battery_max", "Max Battery Capacity (mAh)", value = 5000),
                 actionButton("apply_filters", "Apply Filters")
               ),
               mainPanel(uiOutput("filtered_phones_ui"))
             )
    ),
  )
)

# Define server logic
server <- function(input, output, session) {
  options(shiny.legacy.datatable = TRUE)
  # Update model choices based on selected brand and year range in "All Phones" tab
  observeEvent(list(input$brand, input$year), {
    updateSelectInput(session, "model", 
                      choices = subset(MainData, 
                                       MainData$brand %in% input$brand & 
                                         MainData$release_date <= input$year[2] & 
                                         MainData$release_date >= input$year[1])$device_name)
  })
  
  # Display the selected model's image in "All Phones" tab
  output$image <- renderPlot({
    selected_image <- MainData$image_link[which(MainData$device_name == input$model)]
    if (length(selected_image) > 0) {
      plot(image_read(selected_image))
    }
  })
  output$table <- renderDataTable({
    my_data <- MainData %>% select(-c(phone_url, device_id, image_link))
    
    my_data <- my_data[which(my_data$device_name == input$model),]
    
    # Transpose the data and convert it back to a data frame
    Values <- as.data.frame(t(my_data), stringsAsFactors = FALSE)
    
    # Set proper column names (the original row names become new column headers)
    rownames(Values) <- c("Brand", "Model Name", "Release Year", "Body Detail", "OS Type", "Storage", "Display Size", "Display Resolution", "Camera", "Video", "RAM", "Chipset", "Battery", "Battery Type")
    # Set row names as the original column names (to represent the fields)
    transposed_data <- cbind(Features = rownames(Values), Values = Values)
    colnames(transposed_data) <- c("Features", "Values")
    
    transposed_data  # Return the transposed data frame
  })
  
  output$other <- renderDataTable({
    select <- MainData$release_date[which(MainData$device_name == input$model)]
    tab <- MainData[which(MainData$release_date == select),]
    tab <- tab %>% select(c(brand, device_name, camera, ram, battery))
    colnames(tab) <- c("Brand", "Model Name", "Camera", "RAM", "Battery")
    tab
  })
  
  
  # Update top20 model choices based on selected year in "Top Phones" tab
  observeEvent(input$top20_year, {
    updateSelectInput(session, "top20_model", 
                      choices = subset(Top_phone_data, 
                                       Top_phone_data$year == input$top20_year)$phone_name)
  })
  
  # Display the selected top20 model's image in "Top Phones" tab
  output$top20_image <- renderPlot({
    selected_image <- Top_phone_data$image_link[which(Top_phone_data$phone_name == input$top20_model)]
    if (length(selected_image) > 0) {
      plot(image_read(selected_image))
    }
  })
  output$top20_table<-renderDataTable({
    mytopphone_data <- Top_phone_data %>% select(-c(phone_link, image_link))
    
    mytopphone_data <- mytopphone_data[which(mytopphone_data$phone_name == input$top20_model),]
    
    # Transpose the data and convert it back to a data frame
    Values <- as.data.frame(t(mytopphone_data), stringsAsFactors = FALSE)
    
    # Set proper column names (the original row names become new column headers)
    rownames(Values) <- c("Release Year", "Brand", "Model Name","Network","Launch Year","Status","Dimension","Weight","Build","Sim","Other Body Features","Display Type","Display Size","Display Resolution","Display Protection","Display Other features","OS","Chipset","CPU","GPU","Memory Slot","Internal Memory","Main Camera","Selfie Camera","Sensors","Battery Size","Charging Capacity","Test Scores")
    
    # Set row names as the original column names (to represent the fields)
    tpphone_data <- cbind(Features = rownames(Values), Values = Values)
    colnames(tpphone_data) <- c("Features", "Values")
    
    tpphone_data
  })
  
  # Plot brand dominance across years in Brand Dominance across years tab
  output$Dominance_plot <- renderPlot({
    
    gg_plot_data <- Top_phone_data %>%
      filter(year >= input$Dominance_year[1] & year <= input$Dominance_year[2]) %>%
      group_by(year, brand_name) %>%
      summarise(phone_count = n()) %>%
      ungroup()
    
    
    ggplot(gg_plot_data, aes(x = year, y = phone_count, color = brand_name, group = brand_name)) +
      geom_line(size = 0.5) +
      geom_point(size = 2) +
      labs(x = "Year", y = "Number of Phones", color = "Brand") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  output$Sim_plot <- renderPlot({
    gg_plot_data <- Top_phone_data %>%
      mutate(sim = sub(" .*", "", sim)) %>%
      group_by(year, sim) %>%
      summarise(phone_count = n()) %>%
      ungroup()
    
    
    ggplot(gg_plot_data, aes(x = year, y = phone_count, color = sim, group = sim)) +
      geom_line(size = 0.5) +
      geom_point(size = 2) +
      labs(x = "Year", y = "Number of Phones", color = "SIM") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  output$Weights <- renderPlot({
    gg_plot_data <- Top_phone_data %>%
      mutate(weight = sub(" .*","",weight))
    
    plot(gg_plot_data$weight, xlab="Year", ylab="Weight",xaxt="n", pch=16)
  })
  output$WBS <- renderPlot({
    gg_plot_data <- Top_phone_data %>%
      mutate(weight = sub(" .*","",weight))
    
    
    ggplot(gg_plot_data, aes(x = battery_size....battery_sizes, y = weight)) +
      geom_point(size = 2) +
      labs(x = "Battery Size", y = "Weight")
  })
  output$BatSize <- renderPlot({
    ggplot(Top_phone_data, aes(x = year, y = battery_size....battery_sizes)) + 
      stat_summary(fun = "mean", geom = "bar", fill = "lightblue", color = "black") + 
      labs(title = "Average Battery Size by Year", x = "Year", y = "Average Battery Size (mAh)")
  })
  
  output$Box_plot<-renderPlot({
    
    unique_values <- unique(MainData$`battery_size....battery_sizes`)
    print(unique_values)
    
    # Clean and parse the battery size column with more checks
    # Remove non-numeric characters and convert to numeric
    MainData$battery_size <- suppressWarnings(as.numeric(gsub("[^0-9]", "", MainData$`battery_size....battery_sizes`)))
    
    # Print the number of NA values after conversion for diagnosis
    cat("Number of NA values in battery_size after parsing: ", sum(is.na(MainData$battery_size)), "\n")
    
    # Filter out rows with missing or invalid battery size data
    battery_data <- data %>% filter(!is.na(battery_size))
    
    # If the number of valid rows is too low, print a message
    if (nrow(battery_data) == 0) {
      cat("No valid rows found for battery size.\n")
    } else {
      # Create the box plot using ggplot2
      ggplot(battery_data, aes(x = brand_name, y = battery_size)) +
        geom_boxplot(fill = "steelblue", color = "black") +
        theme_minimal() +
        labs(
          title = "Distribution of Battery Sizes by Brand",
          x = "Brand Name",
          y = "Battery Size (mAh)"
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
    }
  })
  observeEvent(list(input$compare_brand1, input$compare_year1), {
    updateSelectInput(session, "compare_model1", 
                      choices = subset(MainData, 
                                       MainData$brand %in% input$compare_brand1 & 
                                         MainData$release_date <= input$compare_year1[2] & 
                                         MainData$release_date >= input$compare_year1[1])$device_name)
  })
  observeEvent(list(input$compare_brand2, input$compare_year2), {
    updateSelectInput(session, "compare_model2", 
                      choices = subset(MainData, 
                                       MainData$brand %in% input$compare_brand2 & 
                                         MainData$release_date <= input$compare_year2[2] & 
                                         MainData$release_date >= input$compare_year2[1])$device_name)
  })
  output$compare_image1 <- renderPlot({
    selected_image1 <- MainData$image_link[which(MainData$device_name == input$compare_model1)]
    if (length(selected_image1) > 0) {
      plot(image_read(selected_image1))
    }
  })
  output$compare_image2 <- renderPlot({
    selected_image2 <- MainData$image_link[which(MainData$device_name == input$compare_model2)]
    if (length(selected_image2) > 0) {
      plot(image_read(selected_image2))
    }
  })
  output$compare_table <- renderDataTable({
    my_data <- MainData %>% select(-c(phone_url, device_id, image_link))
    
    my_data1 <- my_data[which(my_data$device_name == input$compare_model1),]
    my_data2<- my_data[which(my_data$device_name == input$compare_model2),]
    
    # Transpose the data and convert it back to a data frame
    Values1 <- as.data.frame(t(my_data1), stringsAsFactors = FALSE)
    Values2 <- as.data.frame(t(my_data2), stringsAsFactors = FALSE)
    
    rownames(Values1) <- rownames(Values2) <- c("Brand", "Model Name", "Release Year", "Body Detail", "OS Type", "Storage", "Display Size", "Display Resolution", "Camera", "Video", "RAM", "Chipset", "Battery", "Battery Type")
    
    # Combine both data frames into a single table for side-by-side comparison
    compare_phones_data <- cbind(Features = rownames(Values1), Phone1 = Values1[, 1], Phone2 = Values2[, 1])
    colnames(compare_phones_data) <- c("Features", "Phone 1", "Phone 2")
    
    
    compare_phones_data  # Return the transposed data frame
  })
  filtered_phones <- eventReactive(input$apply_filters, {
    filter_phones(
      data = phones_data,
      brand_input = if (input$brand != "Any") input$brand else NULL,
      os_type_input = if (input$os_type != "Any") input$os_type else NULL,
      release_date_input = input$release_date,
      storage_min = input$storage_min,
      storage_max = input$storage_max,
      display_min = input$display_min,
      display_max = input$display_max,
      camera_min = input$camera_min,
      camera_max = input$camera_max,
      ram_min = input$ram_range[1],
      ram_max = input$ram_range[2],
      battery_min = input$battery_min,
      battery_max = input$battery_max
    )
  })
  
  output$filtered_phones_ui <- renderUI({
    phones <- filtered_phones()
    
    if (nrow(phones) == 0) {
      return(h4("No phones match the selected criteria."))
    }
    
    phone_cards <- lapply(1:nrow(phones), function(i) {
      phone <- phones[i, ]
      tags$div(class = "card mb-3",
               tags$img(src = phone$image_link, class = "card-img-top", alt = phone$device_name),
               tags$div(class = "card-body",
                        tags$h5(class = "card-title", tags$a(href = phone$phone_url, target = "_blank", phone$device_name)),
                        tags$p(class = "card-text", paste("Brand:", phone$brand)),
                        tags$p(class = "card-text", paste("OS:", phone$os_type)),
                        tags$p(class = "card-text", paste("Storage:", phone$storage)),
                        tags$p(class = "card-text", paste("Display Size:", phone$display_size)),
                        tags$p(class = "card-text", paste("Display Resolution:", phone$display_resolution)),
                        tags$p(class = "card-text", paste("Camera:", phone$camera)),
                        tags$p(class = "card-text", paste("RAM:", phone$ram)),
                        tags$p(class = "card-text", paste("Battery:", phone$battery))
               )
      )
    })
    
    tags$div(class = "card-deck", do.call(tagList, phone_cards))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

