library(shiny)
library(shinyjs)

#####
## UI
#####

fluidPage(
 useShinyjs(),
 titlePanel("Language game"),
  column(
    width = 12,
    "Set complexity level and click on the \"Play\" button to learn the rule.",
    br(),
    br(),
    fluidRow(
      column(
        width = 3,
        selectInput(
          inputId = "complexity",
          label = "Choose complexity",
          choices = c("Easy" = 1, "Difficult" = 2),
          width = '100%',
          selected = 1
        )
      ),
      column(
        width = 9,
        actionButton(
          inputId = "launchGame",
          label = "Play!",
          icon = icon("play"),
          width = '25%',
          style = "display:inline-block; margin-top: 25px"
        ),
        actionButton(
          inputId = "testMode",
          label = "Test yourself",
          icon = icon("check-square-o"),
          width = '25%',
          style = "display:inline-block; margin-top: 25px"
        )
      )
    ),

    hr(),

    sidebarLayout(
      sidebarPanel(
        uiOutput("communicate"),
        uiOutput("score"),
        uiOutput("moves")
      ),
      mainPanel(
        fluidRow(
          actionButton(
            inputId = "figLRC",
            label = " ",
            icon = icon("circle", "fa-4x"),
            width = '20%',
            style='height:80px; color: #ff0000; background-color: #fff'
          ),
          actionButton(
            inputId = "figSBC",
            label = " ",
            icon = icon("circle", "fa-2x"),
            width = '20%',
            style='height:80px; color: #0000ff; background-color: #fff'
          ),
          actionButton(
            inputId = "figSGC",
            label = " ",
            icon = icon("circle", "fa-2x"),
            width = '20%',
            style='height:80px; color: #00ff00; background-color: #fff'
          ),

          actionButton(
            inputId = "figLBC",
            label = " ",
            icon = icon("circle", "fa-4x"),
            width = '20%',
            style='height:80px; color: #0000ff; background-color: #fff'
          )
        ),
        br(),

        fluidRow(
          actionButton(
            inputId = "figLGC",
            label = " ",
            icon = icon("circle", "fa-4x"),
            width = '20%',
            style='height:80px; color: #00ff00; background-color: #fff'
          ),
          actionButton(
            inputId = "figSBS",
            label = " ",
            icon = icon("square", "fa-2x"),
            width = '20%',
            style='height:80px; color: #0000ff; background-color: #fff'
          ),
          actionButton(
            inputId = "figSRC",
            label = " ",
            icon = icon("circle", "fa-2x"),
            width = '20%',
            style='height:80px; color: #ff0000; background-color: #fff'
          ),
          actionButton(
            inputId = "figLRS",
            label = " ",
            icon = icon("square", "fa-4x"),
            width = '20%',
            style='height:80px; color: #ff0000; background-color: #fff'
          )
        ),
        
        br(),
        
        fluidRow(
          actionButton(
            inputId = "figLBS",
            label = " ",
            icon = icon("square", "fa-4x"),
            width = '20%',
            style='height:80px; color: #0000ff; background-color: #fff'
          ),
          actionButton(
            inputId = "figLGS",
            label = " ",
            icon = icon("square", "fa-4x"),
            width = '20%',
            style='height:80px; color: #00ff00; background-color: #fff'
          ),
          actionButton(
            inputId = "figSRS",
            label = " ",
            icon = icon("square", "fa-2x"),
            width = '20%',
            style='height:80px; color: #ff0000; background-color: #fff'
          ),
          actionButton(
            inputId = "figSGS",
            label = " ",
            icon = icon("square", "fa-2x"),
            width = '20%',
            style='height:80px; color: #00ff00; background-color: #fff'
          )
        )
      )
    )
  )
)
