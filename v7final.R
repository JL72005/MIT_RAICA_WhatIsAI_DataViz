library(shiny)
library(ggplot2)
library(plotly)
library(stringr)
library(dplyr)

#response data
df <- data.frame(
  post_gpt = c(
    0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
    0, 0, 1, 1, 1,
    0, 1, 1, 1,
    0, 0, 1, 1,
    0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1,
    0, 0, 0, 0, 1, 1,
    0, 0, 0, 1, 1, 1, 1
  ),
  
  admin_numbers = c(
    1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5,
    1, 2, 3, 3, 4,
    1, 2, 3, 3,
    1, 1, 2, 3,
    1, 1, 1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5,
    1, 2, 3, 3, 4, 4,
    1, 2, 2, 3, 3, 3, 3
  ),
  
  student_no = c(
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    3, 3, 3, 3, 3,
    4, 4, 4, 4,
    5, 5, 5, 5,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    7, 7, 7, 7, 7, 7,
    9, 9, 9, 9, 9, 9, 9
  ),
  raica_module = c(
    "IPA Mini", "RTWR", "RTWR", "D4D", "D4D", "D4D",
    "Pre - RL", "Pre - RL", "Pre - RL", "Post - RL", "Post - RL", "Post - RL",
    "IPA Mini", "RTWR", "Pre - RL", "Pre - RL", "Post - RL",
    "RTWR", "Pre - RL","Post - RL", "Post - RL",
    "RTWR", "RTWR", "Pre - RL", "Post - RL",
    "IPA Mini", "IPA Mini", "IPA Mini",
    "RTWR", "RTWR", "D4D", "D4D",
    "Pre - RL", "Pre - RL", "Pre - RL", "Post - RL", "Post - RL", "Post - RL",
    "IPA Mini", "RTWR", "D4D", "D4D",
    "Pre - RL", "Pre - RL",
    "IPA Mini", "RTWR", "RTWR", "Pre - RL", "Pre - RL", "Pre - RL", "Pre - RL"
  ),
  
  ai_responses <- c(
    "I think Ai means Robots.",
    "I believe AI means <b>Robots</b> and computer programs",
    "I believe AI means Robots <b>and computer programs</b>",
    
    "AI means a <b>computer or object</b> that cab learn and do tasks",
    "AI means a Computer or object that <b>cab learn</b> and do tasks",
    "AI means a Computer or object that cab learn <b>and do tasks</b>",
    
    "AI Is where a <b>computer</b> does tasks on its own such as setting an alarm and reminder and following though with the action when it wants to be done ( Like alexa and siri )",
    "AI Is where a computer <b>does tasks on its own</b> such as setting an alarm and reminder and following though with the action when it wants to be done ( Like alexa and siri )",
    "AI Is where a computer does tasks on its own <b>such as setting an alarm and reminder and following though with the action when it wants to be done ( Like alexa and siri )</b>",
    
    "Artificial Intelligence is  <b>Robots that either use Reinforcement learning<b> or rule based programming in AI can hekp with loads of things and can help improve everyone daily life .",
    "Artificial Intelligence is  Robots that either use Reinforcement learning or <b>rule based programming in AI can hekp with loads of things </b> and can help improve everyone daily life .",
    "Artificial Intelligence is  Robots that either use Reinforcement learning or rule based programming in AI can hekp with loads of things and can <b>help improve everyone daily life .</b>",
    
    ".",
    
    "to me it means <b>education and having fun online</b>",
    "I think it means A <b>type of robot/device</b> that can help you with different problems",
    "I think it means A type of robot/device that can <b>help you with different problems</b>",
    "I think it means a <b> mechanical being that can be controlled in a certain way depending on the code you give it to function with</b>",
    "Artificial means <b>man made intelligence is thinking.</b>",
    
    "In means a <b>person-made intelligence like a brain</b>",
    
    "<b>Its an intelligance that</b>  a person made and coded",
    "Its an intelligance that  <b>a person made and coded</b>",
    
    "It means <b>Technology Intelligence</b> but artificial means fake.",
    "It means Technology Intelligence but <b>artificial means fake</b>.",
    
    "I think that AI means that <b>you can create an Intelligent robot and it can have a mind of its own</b>.",
    
    "I think that it means that <b>people are making artificial thinking, like how humans think to robots and machines</b>.",
    
    "Artificial Intelligence is <b>based on Robotics</b> like the ones that help us or are programmed to help people in different situations like siri,alexa,cortana they are all AI.",
    "Artificial Intelligence is based on Robotics <b>like the ones that help us or are programmed</b> to help people in different situations like siri,alexa,cortana they are all AI.",
    "Artificial Intelligence is based on Robotics like the ones that help us or are programmed <b>to help people in different situations like siri,alexa,cortana they are all AI.</b>",
    
    "AI are <b>machines that are programmed</b> to types of jobs that they are programmed to <b>like for example elevator lifts are programmed to open and close by using a sensor to see if someone is there or not and if you press a the button with a number on it it will bring you there because the AI was programmed to that</b>.",
    "AI are machines that are programmed to <b>types of jobs that they are programmed</b> to like for example elevator lifts are programmed to open and close by using a sensor to see if someone is there or not and if you press a the button with a number on it it will bring you there because the AI was programmed to that.",
    
    "I think that artificial intelligence means an <b>object that is programmed</b> in a way that makes able to do things like open doors or sort things.",
    "I think that artificial intelligence means an object that is programmed <b>in a way that makes able to do things like open doors or sort things</b>.",
    
    "I think that AI is a machine that is <b>porgrammed</b> to follow what they coded it for and that it is also created for beneficial purposes for humans like cleaning AI.",
    "I think that AI is a machine that is porgrammed <b>to follow what they coded</b> it for and that it is also created for beneficial purposes for humans like cleaning AI.",
    "I think that AI is a machine that is porgrammed to follow what they coded it for and that it is also created for <b>beneficial purposes for humans like cleaning AI.</b>",
    
    "Artificial intelligence is <b>teaching machines complex or simple thing</b> such as working in a factory and organising data. Some AI's can interact with humans and can talk while others just do what they are told. It is basiclally humans using their information or inetlligence and putting into a machine.",
    "Artificial intelligence is teaching machines complex or simple thing <b>such as working in a factory and organising data</b>. Some AI's can interact with humans and can talk while others just do what they are told. It is basiclally humans using their information or inetlligence and putting into a machine.",
    "Artificial intelligence is teaching machines complex or simple thing such as working in a factory and organising data. <b>Some AI's can interact with humans and can talk while others just do what they are told. It is basiclally humans using their information or inetlligence and putting into a machine</b>.",
    
    "<b>computer intelligence</b>",
    
    ".",
    
    "it means <b>a robot or machine</b> that can predict what people are saying without having that word or sentance in their code or just being able to predict.",
    "it means a robot or machine that <b>can predict what people are saying without having that word or sentance in their code or just being able to predict</b>.",
    
    "it means  <b>a robot</b> that acts and looks like a human or animal",
    "it means  a robot that <b>acts and looks like a human or animal</b>",
    
    "Artificial intelligence",
    
    "<b>Fake inteligence not real</b>, just programmed",
    "Fake inteligence not real, <b>just programmed</b>",
    
    "AI is a <b>computer</b> or device that can think like a human. AI is used in Siri or Alexa. AI can be put into robots.",
    "AI is a computer or <b>device</b> that can think like a human. AI is used in Siri or Alexa. AI can be put into robots.",
    "AI is a computer or device that can <b>think like a human</b>. AI is used in Siri or Alexa. AI can be put into robots.",
    "AI is a computer or device that can think like a human. AI is <b>used in Siri or Alexa. AI can be put into robots</b>."
  ),
  
  conceptual_categories <- c(
    "AI is the same as automation and robotics",
    "AI is the same as automation and robotics",
    "Equating AI to computers",
    "Equating AI to computers",
    "Description of underlying technology",
    "Value/benefit to humans",
    "Equating AI to computers",
    "AI is created to be smart",
    "Equating AI to conversational agents",
    "AI is the same",
    "Value/benefit to humans",
    "Description of underlying technology",
    "No response",
    "Value/benefit to humans",
    "AI is the same as automation and robotics",
    "Value/benefit to humans",
    "AI is the same as automation and robotics",
    "AI is created to be smart",
    "AI is created to be smart",
    "AI is created to be smart",
    "AI is the same as automation and robotics",
    "AI is created to be smart",
    "Rephrasing with no interpretation",
    "AI is created to be smart",
    "AI is created to be smart",
    "AI is the same as automation and robotics",
    "Value/benefit to humans",
    "Equating AI to conversational agents",
    "AI is the same as automation and robotics",
    "Value/benefit to humans",
    "AI is the same as automation and robotics",
    "Value/benefit to humans",
    "AI is the same as automation and robotics",
    "Value/benefit to humans",
    "AI is the same as automation and robotics",
    "Description of underlying technology",
    "AI is the same as automation and robotics",
    "Anthropomorphic",
    "AI is created to be smart",
    "No response",
    "AI is the same as automation and robotics",
    "Description of underlying technology",
    "AI is the same as automation and robotics",
    "Anthropomorphic",
    "Rephrasing with no interpretation",
    "Rephrasing with no interpretation",
    "AI is the same as automation and robotics",
    "Equating AI to computers",
    "Anthropomorphic",
    "Equating AI to conversational agents",
    "AI is the same as automation and robotics"
  )
)

################## APP building

# UI
ui <- fluidPage(
  titlePanel("Student Responses to \"What is AI?\""),
  p(HTML('Seven middle schoolers from an international school responded to this question. Their answers are displayed.')),
  checkboxGroupInput(
    inputId = 'select_student',
    label = 'Select a Student to Highlight',
    choices = as.character(sort(unique(df$student_no))),
    selected = as.character(sort(unique(df$student_no))),
    inline = TRUE
  ),
  p(HTML('Each circle represents a student response tagged with a conceptual category. Hover to view full response.')),
  plotlyOutput("dot_plot", height = "300px"),
  hr(), #horizontal break
  plotlyOutput("heatmap_plot", height = "300px")
)


#server
server <- function(input, output, session) {
  filtered_data <- reactive({
    df %>%
      mutate(
        student_no_chr = as.character(student_no),
        highlight = if (is.null(input$select_student) || length(input$select_student) == 0) {
          TRUE
        } else {
          student_no_chr %in% input$select_student
        },
        highlight_alpha = ifelse(highlight, 1, 0.2),
        wrapped_response = str_wrap(ai_responses, width = 60) %>% str_replace_all("\n", "<br>"),
        admin_trial_factor = factor(admin_numbers, levels = c(1, 2, 3, 4, 5),
                                    labels = c("Admin 1", "Admin 2", "Admin 3", "Admin 4", "Admin 5")),
        student_no_factor = factor(student_no),
        conceptual_categories = factor(
          conceptual_categories,
          levels = names(sort(table(conceptual_categories), decreasing = TRUE)),
          labels = paste0(
            names(sort(table(conceptual_categories), decreasing = TRUE)),
            " (", as.vector(sort(table(conceptual_categories), decreasing = TRUE)), ")"
          )
        )
      )
  })
  
  output$dot_plot <- renderPlotly({
    df <- filtered_data() %>%
      group_by(admin_trial_factor, student_no_factor) %>%
      mutate(
        row_id = row_number(),
        n = n(),
        # Jitter x within ±0.4
        offset_x = ifelse(n == 1, 0, (row_id - (n + 1) / 2) * (0.8 / max(n, 1))),
        admin_x = as.numeric(admin_trial_factor) + offset_x,
        # Jitter y within ±0.4 (optional)
        #offset_y = ifelse(n == 1, 0, (row_id - (n + 1) / 2) * (0.8 / max(n, 1))),
        student_y = as.numeric(student_no_factor)
      ) %>%
      ungroup()
    
    
    admin_breaks <- seq(1.5, length(levels(df$admin_trial_factor)) - 0.5, by = 1)
    student_breaks <- seq(1.5, length(levels(df$student_no_factor)) - 0.5, by = 1)
    
    p <- ggplot(df, aes(
      x = admin_x,
      y = student_y,
      color = conceptual_categories,
      text = wrapped_response,
      alpha = highlight_alpha
    )) +
      geom_point(shape = 19, size = 5, stroke = 0.4, fill = "white") +
      scale_x_continuous(
        breaks = seq_along(levels(df$admin_trial_factor)),
        labels = levels(df$admin_trial_factor)
      )+
      scale_y_continuous(
        breaks = seq_along(levels(df$student_no_factor)),
        labels = levels(df$student_no_factor)
      ) + 
      scale_color_viridis_d(option = "turbo") +
      scale_alpha_continuous(range = c(0.2, 1), guide = 'none') +
      labs(
        x = "Admin Trial",
        y = "Student",
        color = "Conceptual Category"
      ) +
      geom_vline(xintercept = admin_breaks, color = "gray60", linetype = "dashed") +
      geom_hline(yintercept = student_breaks, color = "gray60", linetype = "dashed") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank()
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  
  output$heatmap_plot <- renderPlotly({
    print("Rendering heatmap...")
    df <- filtered_data() %>%
      filter(highlight)
    
    # Summarize counts per category and admin trial
    df_summary <- df %>%
      group_by(admin_trial = admin_trial_factor, category = as.character(conceptual_categories)) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(hover_text = paste0(admin_trial, "<br>Count: ", count))
    
    # If no data, don’t try to plot
    if (nrow(df_summary) == 0) return(NULL)
    
    # Create heatmap
    q <- ggplot(df_summary, aes(
      x = admin_trial,
      y = reorder(category, count),
      fill = count
    )) +
      geom_tile(aes(text = hover_text), color = "white") +
      scale_fill_distiller(palette = "Blues", direction = 1) +
      labs(
        x = "Admin Trial",
        y = "Conceptual Category",
        fill = "Response Count"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text.y = element_text(size = 10)
      )
    
    ggplotly(q, tooltip = "text")
  })
  
}

# Run app
shinyApp(ui, server)