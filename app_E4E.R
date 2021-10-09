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

server <- function(input, output) {
    source("E4E_EGM.R")
    orig <- load_E4E()
    # output$size <- renderText({
    #     paste("size: ", height(), sep = "")
    # })
    
    df <- reactive({
        egm_df <- prep_egm(apply_choices(orig, input$Rating, input$Topic, input$Grade))
        make_egm(egm_df)   
    })
    
    height <- reactive({
        egm_df <- prep_egm(apply_choices(orig, input$Rating, input$Topic, input$Grade))
        # if(input$detail=="Programs"){
            length(unique(egm_df$name))*150
        # } else {
        #     length(unique(egm_df$Program))*600   # If not programs, then it's types of programs
        # }
    })

    output$contents <- renderPlot({
        df()
    })
    
    output$map <- renderUI({
        # fillPage(plotOutput("contents", height = height(), width = "100%"))
        #plotOutput("contents", height = "200", width = "100%")
        plotOutput("contents", height = height(), width = "100%")
    })
    
    output$hover_info <- renderPrint({
        if(!is.null(input$plot_hover)){
            plotdf <- prep_egm(apply_choices(orig, input$Rating, input$Topic, input$Grade))
            hover=input$plot_hover
            dist=sqrt((hover$x-plotdf$size)^2+(hover$y - plotdf$`Effect Size`)^2)
            #cat(" x =", hover$x, "y =", hover$y, "\n",
            #    "min dist =", min(dist), "\n",
            #    "which = ", which.min(dist), "\n",
            cat(paste(plotdf$name[which.min(dist)]), paste(plotdf$url[which.min(dist)]), sep="\n")
        }
        
        
    })
}

shinyApp(ui = ui, server = server)