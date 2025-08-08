###load data###
response_data <- read.csv("C:/Users/jessi/OneDrive/Documentos/PITUN-BootCamp/Lab-3/shiny-app/updated_qualitative_coding_with_extra columns.csv", header = TRUE) 
#head(response_data)

tag_data <- read.csv("C:/Users/jessi/OneDrive/Documentos/PITUN-BootCamp/Lab-3/shiny-app/tag_freq.csv")

# Clean column types
# Recalculate the percentage properly
tag_data$name_clean <- trimws(tolower(tag_data$name))
tag_data$percentage <- tag_data$count / sum(tag_data$count)
tag_data$tag <- tag_data$name
tag_data$label <- paste0(
  tag_data$name, ": ",
  tag_data$count, " (",
  round(tag_data$percentage * 100, 1), "%)"
)
tag_data <- na.omit(tag_data)  # remove any rows with NA

#create tag count per response
tag_cols <- c("tag.1", "tag.2", "tag.3", "tag.4", "tag.5", "tag.6")
response_data$tag_count <- apply(response_data[, tag_cols], 1, function(row) {
  sum(!is.na(row) & row != "")
})
for (col in tag_cols) {
  response_data[[col]] <- trimws(tolower(response_data[[col]]))
}



#assign XY positions for grid 
grid <- expand.grid(
  position_X = 1:15,
  position_Y = 1:10
)[1:nrow(response_data), ]

response_data <- cbind(response_data, grid)

#wrap long answers
library(stringr)

response_data$wrapped_answer <- str_wrap(response_data$Answer, width = 60)


#install.packages("treemapify")
library(shiny)
library(plotly)
library(ggplot2)
library(treemapify)
ui <- fluidPage(
  titlePanel("What is AI? An Analysis of 152 Middle School Student Responses"),
  p(HTML('We asked over 150 middle school students over February to June 2024 the question \"What is AI?\" Here are their responses.')),
  
  plotOutput('tagBar', height = '200px'),
  
  selectInput("tag_filter", "Filter by tag(s):",
              choices = setNames(tag_data$name_clean, tag_data$name),
              selected = NULL,
              multiple = TRUE),
  
  checkboxGroupInput("count_filter", "filter by tag count",
                     choices = 1:6,
                     selected = 1:6,
                     inline = TRUE),
  
  plotlyOutput("responseDots", height = "500px")
  
) 

server <- function(input, output, session) {
  # Column list for tag matching
  tag_cols <- c("tag.1", "tag.2", "tag.3", "tag.4", "tag.5", "tag.6")
  
  # Generate combined tag string for tooltip
  response_data$tag_string <- apply(response_data[, tag_cols], 1, function(row) {
    tags <- row[!is.na(row) & row != ""]
    paste(tags, collapse = ", ")
  })
  
  # Reactive selected tag from dropdown
  selected_tag <- reactive({
    if (is.null(input$tag_filter) || length(input$tag_filter) == 0) return(NULL)
    input$tag_filter
  })
  # Final filtered data based on tag count AND dropdown tag
  filtered_responses <- reactive({
    data <- subset(response_data, tag_count %in% input$count_filter)
    
    tag <- selected_tag()
    if (is.null(tag)) return(data)
    
    matches <- apply(data[, tag_cols], 1, function(row) {
      any(trimws(tolower(row)) == tolower(tag))
    })
    
    data[matches, ]
  })
  
  
  # Treemap (non-interactive)
  output$tagBar <- renderPlot({
    ggplot(tag_data, aes(
      area = percentage,
      fill = name,
      label = label
    )) +
      geom_treemap(color = "white") +
      geom_treemap_text(
        colour = "black",
        place = "center",
        grow = TRUE
      ) +
      theme_void() +
      theme(legend.position = "none")
  })
  
  # Dots with hover tooltip
  output$responseDots <- renderPlotly({
    p <- ggplot(filtered_responses(), aes(
      x = position_X,
      y = position_Y,
      size = tag_count,
      text = paste0(
        "Tag count: ", tag_count, "\n",
        "Tags: ", tag_string, "\n\n",
        "Answer: ", wrapped_answer
      )
    )) +
      geom_point(alpha = 0.7, color = "black") +
      scale_size_continuous(range = c(4, 14)) +
      theme_void() +
      coord_fixed()
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        margin = list(t = 10, b = 0, l = 0, r = 0)
      )
  })
}
shinyApp(ui=ui, server=server)

