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
    fluidRow(
      column(
        width = 12,
        "Click on the \"Play\" button to learn the rule.",
        br(),
        br(),
        actionButton(
          inputId = "launchGame",
          label = "Play!",
          icon = icon("play"),
          width = '15%',
          style = "display:inline-block"
        ),
        actionButton(
          inputId = "testMode",
          label = "Test yourself",
          icon = icon("check-square-o"),
          width = '15%',
          style = "display:inline-block"
        )
      )
    ),

    hr(),

    sidebarLayout(
      sidebarPanel(
        uiOutput("communicate"),
        uiOutput("score")
      ),
      mainPanel(
        fluidRow(
          actionButton(
            inputId = "figLRC",
            label = " ",
            icon = icon("circle", "fa-4x"),
            width = '20%',
            style='height:85px; color: #ff0000; background-color: #fff'
          ),
          actionButton(
            inputId = "figSBC",
            label = " ",
            icon = icon("circle", "fa-2x"),
            width = '20%',
            style='height:85px; color: #337ab7; background-color: #fff'
          ),
          actionButton(
            inputId = "figLRS",
            label = " ",
            icon = icon("square", "fa-4x"),
            width = '20%',
            style='height:85px; color: #ff0000; background-color: #fff'
          ),
          actionButton(
            inputId = "figLBC",
            label = " ",
            icon = icon("circle", "fa-4x"),
            width = '20%',
            style='height:85px; color: #337ab7; background-color: #fff'
          )
        ),
        br(),

        fluidRow(
          actionButton(
            inputId = "figLBS",
            label = " ",
            icon = icon("square", "fa-4x"),
            width = '20%',
            style='height:80px; color: #337ab7; background-color: #fff'
          ),
          actionButton(
            inputId = "figSBS",
            label = " ",
            icon = icon("square", "fa-2x"),
            width = '20%',
            style='height:80px; color: #337ab7; background-color: #fff'
          ),
          actionButton(
            inputId = "figSRC",
            label = " ",
            icon = icon("circle", "fa-2x"),
            width = '20%',
            style='height:80px; color: #ff0000; background-color: #fff'
          ),
          actionButton(
            inputId = "figSRS",
            label = " ",
            icon = icon("square", "fa-2x"),
            width = '20%',
            style='height:80px; color: #ff0000; background-color: #fff'
          )
        )
      )
    )
  )
)
