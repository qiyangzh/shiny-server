library(shiny)
ui <- fluidPage(
    # *Input() functions
    # selectInput(inputId = "Evidence", 
    #             label = "Choose an evidence category", 
    #             choices = c("Evidence Present", "Evidence Absent")),
    selectInput(inputId = "Rating", 
                label = "Choose level of rating",
                choices = c("All", "Strong", "Promising", "Moderate", "No significance")),
    selectInput(inputId = "Topic",
                label = "Choose a topic",
                choices = c("All", "Math", "Reading")),
    selectInput(inputId = "Grade",
                label = "Choose a grade",
                choices = c("All", "Kindergarten", "Elementary", "Middle", "High")),
    fluidRow(
        column(width = 12,
               plotOutput("contents", hover = hoverOpts(id ="plot_hover"))
        )
    ),
    fluidRow(
        column(width = 12,
               verbatimTextOutput("hover_info")
        )
    ),
    # *Output() functions
    mainPanel(
        textOutput(outputId = "size"),
        uiOutput('map')
    )
)
