# --- Libraries ---
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)

# --- Load data ---
response_data <- read.csv(
  "C:/Users/jessi/OneDrive/Documentos/PITUN-BootCamp/Lab-3/shiny-app/updated_qualitative_coding_with_extra columns.csv",
  header = TRUE, stringsAsFactors = FALSE
)

# Define tag columns and normalize case/spacing in response_data
tag_cols <- c("tag.1","tag.2","tag.3","tag.4","tag.5","tag.6")
stopifnot(all(tag_cols %in% names(response_data)))
for (col in tag_cols) {
  response_data[[col]] <- trimws(tolower(response_data[[col]]))
}

# (Optional) Read tag_freq file if you still want it
# tag_data <- read.csv("C:/Users/jessi/OneDrive/.../tag_freq.csv", header = TRUE, stringsAsFactors = FALSE)

# Recompute tag_data directly from response_data so it always matches
library(dplyr); library(tidyr); library(stringr)
tag_long <- response_data |>
  pivot_longer(all_of(tag_cols), names_to = "slot", values_to = "tag") |>
  mutate(tag = trimws(tolower(tag))) |>
  filter(!is.na(tag), tag != "")

tag_data <- tag_long |>
  count(tag, name = "count") |>
  mutate(
    name_clean = tag,
    name = stringr::str_to_title(tag),  # or keep tag as-is if you prefer
    percentage = count / sum(count),
    label = paste0(name, ": ", count, " (", round(percentage*100, 1), "%)")
  )

# Quick checks
#cat("Unique tags in tag_data (rebuilt):\n"); print(sort(unique(tag_data$name_clean)))
#cat("Unique tags in responses (from tag columns):\n"); print(sort(unique(tag_long$tag)))

tag_data <- read.csv("C:/Users/jessi/OneDrive/Documentos/PITUN-BootCamp/Lab-3/shiny-app/tag_freq.csv",
                     header = TRUE, stringsAsFactors = FALSE)
cat("Unique tags in tag_data:\n"); print(sort(unique(tag_data$name)))
cat("Unique tags in responses (from tag columns):\n")
print(sort(unique(unlist(response_data[, tag_cols]))))

# --- Ensure a consistent text column for tooltips ---
ensure_answer_text <- function(d) {
  cand <- intersect(c("answer_text","wrapped_answer","Answer","answer","Response","response"),
                    names(d))
  d$answer_text <- if (length(cand)) str_wrap(as.character(d[[cand[1]]]), 60) else ""
  d
}
response_data <- ensure_answer_text(response_data)

# --- Clean tag_data & palette ---
stopifnot(all(c("name","count") %in% names(tag_data)))
tag_data <- tag_data %>%
  mutate(
    name_clean = trimws(tolower(name)),
    label = paste0(name, ": ", count)
  )

all_tags <- sort(unique(tag_data$name_clean))
pal <- setNames(hue_pal()(length(all_tags)), all_tags)

# --- Tag columns & per-response counts ---
tag_cols <- c("tag.1","tag.2","tag.3","tag.4","tag.5","tag.6")
stopifnot(all(tag_cols %in% names(response_data)))
for (col in tag_cols) response_data[[col]] <- trimws(tolower(response_data[[col]]))
response_data$tag_count <- apply(response_data[, tag_cols], 1, function(row) sum(!is.na(row) & row != ""))

# --- Fixed 10x15 grid positions ---
grid <- expand.grid(position_X = 1:15, position_Y = 1:10)[1:nrow(response_data), ]
response_data <- bind_cols(response_data, grid)

# --- UI ---
ui <- fluidPage(
  titlePanel('152 "What is AI" responses'),
  p(HTML('We asked over 152 middle school students over February to June 2024 the question "What is AI?" Here are their responses.')),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("tag_filter", "Filter by tag(s):",
                  choices = setNames(tag_data$name_clean, tag_data$name),
                  selected = NULL, multiple = TRUE),
      checkboxGroupInput("count_filter", "Filter by tag count",
                         choices = 1:6, selected = 1:6, inline = TRUE),
      h4("Conceptual Categories by Frequency"),
      plotlyOutput("tagBar", height = "380px"),
      width = 6
    ),
    mainPanel(
      h4("Conceptual Categories For Each Response"),
      p(HTML("Each tile represents one student's response. The tile's colors represent the conceptual category that appears first in the response.")),
      plotlyOutput("responseGrid", height = "520px"),
      width = 6
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # Filtered responses (reactive)
  filtered_responses <- reactive({
    df <- response_data
    if (!is.null(input$tag_filter) && length(input$tag_filter) > 0) {
      keep <- apply(df[tag_cols], 1, function(row) any(row %in% input$tag_filter))
      df <- df[keep, , drop = FALSE]
    }
    df <- df[df$tag_count %in% input$count_filter, , drop = FALSE]
    ensure_answer_text(df)
  })
  
  # Sidebar bar chart (interactive)
  # helper
  count_tags <- function(df) {
    df |>
      tidyr::pivot_longer(all_of(tag_cols), names_to = "slot", values_to = "tag") |>
      dplyr::filter(!is.na(tag), tag != "") |>
      dplyr::mutate(name_clean = trimws(tolower(tag)),
                    name = stringr::str_to_title(name_clean)) |>
      dplyr::count(name_clean, name, name = "count") |>
      dplyr::arrange(desc(count))
  }
  
  output$tagBar <- renderPlotly({
    df <- filtered_responses()              # already respects count_filter
    tc <- count_tags(df)
    
    # Show ONLY the selected tags (if any are selected)
    sel <- input$tag_filter
    if (!is.null(sel) && length(sel) > 0) {
      tc <- dplyr::filter(tc, name_clean %in% sel)
    }
    
    validate(need(nrow(tc) > 0, "No data for current filters"))
    
    pal_use <- pal[tc$name_clean]; names(pal_use) <- tc$name_clean
    
    gg <- ggplot(tc,
                 aes(x = reorder(name, count), y = count,
                     fill = name_clean,
                     text = paste0(name, ": ", count))) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = count), hjust = -0.2, size = 3) +
      coord_flip() +
      scale_fill_manual(values = pal_use, na.translate = FALSE) +
      labs(x = NULL, y = "Response Count") +
      theme_minimal(base_size = 10) +
      theme(panel.grid.major.y = element_blank())
    
    ggplotly(gg, tooltip = "text")
  })
  
  #response grid
  output$responseGrid <- renderPlotly({
    df <- filtered_responses()
    validate(need(nrow(df) > 0, "No data for current filters"))
    
    # Tooltip text
    df$tag_string <- apply(df[, tag_cols], 1, function(row) {
      row <- row[!is.na(row) & row != ""]
      if (length(row) == 0) "" else paste(row, collapse = ", ")
    })
    df$tile_tag <- df$`tag.1`
    df$tile_tag[is.na(df$tile_tag) | df$tile_tag == ""] <- "other"
    if (!"other" %in% names(pal)) pal <- c(pal, other = "#CCCCCC")
    
    # --- build pips (cap at 6) laid out in a 3x2 grid in top-left corner
    pips <- df |>
      dplyr::mutate(n_pips = pmin(tag_count, 6)) |>
      tidyr::uncount(n_pips, .id = "pip_id") |>
      dplyr::mutate(
        # pip positions relative to the tile center
        # 3 columns (0,1,2), 2 rows (0,1)
        col = (pip_id - 1) %% 3,
        row = (pip_id - 1) %/% 3,
        px  = position_X - 0.37 + 0.18 * col,
        py  = position_Y - 0.37 + 0.18 * row
      )
    
    p <- ggplot(
      df,
      aes(position_X, position_Y,
          fill = tile_tag,
          text = paste0(
            "Tag count: ", tag_count, "\n",
            "Tags: ", tag_string, "\n\n",
            "Answer: ", answer_text
          ))
    ) +
      geom_tile(width = 0.98, height = 0.98, color = "white") +
      # pips: small white halo then solid dot for visibility
      geom_point(
        data = pips, inherit.aes = FALSE,
        aes(px, py),
        size = 2.6, shape = 21, stroke = 0, fill = "white"
      ) +
      geom_point(
        data = pips, inherit.aes = FALSE,
        aes(px, py),
        size = 1.6, shape = 21, stroke = 0, fill = "black"
      ) +
      scale_fill_manual(values = pal, na.translate = FALSE) +
      scale_y_reverse() + coord_fixed() +
      theme_void() + theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") |>
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        margin = list(t = 10, b = 0, l = 0, r = 0)
      )
  })
  
  #response grid with numbers
  output$responseGrid <- renderPlotly({
    df <- filtered_responses()
    validate(need(nrow(df) > 0, "No data for current filters"))
    
    # Tooltip text (same as before)
    df$tag_string <- apply(df[, tag_cols], 1, function(row) {
      row <- row[!is.na(row) & row != ""]
      if (length(row) == 0) "" else paste(row, collapse = ", ")
    })
    
    # Color by first tag (or change this to your category field)
    df$tile_tag <- df$`tag.1`
    df$tile_tag[is.na(df$tile_tag) | df$tile_tag == ""] <- "other"
    if (!"other" %in% names(pal)) pal <- c(pal, other = "#CCCCCC")
    
    p <- ggplot(
      df,
      aes(position_X, position_Y,
          fill = tile_tag,
          text = paste0(
            "Tag count: ", tag_count, "\n",
            "Tags: ", tag_string, "\n\n",
            "Answer: ", answer_text
          ))
    ) +
      geom_tile(width = 0.98, height = 0.98, color = "white") +
      # --- Add the count label on top of each tile ---
      geom_text(aes(label = tag_count), color = "white", size = 4, fontface = "bold") +  # halo
      geom_text(aes(label = tag_count), color = "black", size = 3, fontface = "bold") +  # text
      scale_fill_manual(values = pal, na.translate = FALSE) +
      scale_y_reverse() + coord_fixed() +
      theme_void() + theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        margin = list(t = 10, b = 0, l = 0, r = 0)
      )
  })
  
  
  
  
  
}

shinyApp(ui, server)
