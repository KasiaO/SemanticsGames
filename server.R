source('functions.R')


#####
## server
#####

shinyServer(function(input, output) {
  
  observe({
    game <<- reactiveValues()
    game$comm <<- " "
    game$score <<- 0
    game$teacher <<- Agent$new()
    game$active <<- FALSE
    game$test <<- FALSE
    game$moves <<- 0
    disable("testMode")
  })
  
  observeEvent(input$launchGame, {
    
    figDims <- list(
      color = c("red", "blue", "green"),
      size = c("small", "large"),
      shape = c("circle", "square")
    )

    figures <- setEnv(figDims)
    game$teacher$accept <<- game$teacher$makeSplit(input$complexity, figures)
    game$score <<- 0
    game$active <<- TRUE
    game$test <<- FALSE
    game$comm <<- "Let's play!"
    game$moves <<- 0
    
    # enable all buttons
    enable("figLRC")
    enable("figLRS")
    enable("figSRC")
    enable("figSRS")
    enable("figLBC")
    enable("figLBS")
    enable("figSBC")
    enable("figSBS")
    enable("figLGC")
    enable("figLGS")
    enable("figSGC")
    enable("figSGS")
    enable("testMode")
  })

  observeEvent(input$figLRC, {

    req(length(game$teacher$accept) > 0)
    req(game$active)
    fig <- Figure$new(size = "large", color = "red", shape = "circle")
    point <- game$teacher$givePoint(fig)
    game$moves <<- game$moves + 1
    game$score <<- game$score + point
    if(point > 0) {
      game$comm <<- "I accept this card!"
    } else if(game$test) {
      game$score <<- -1000
    } else {
      game$comm <<- "Sorry, I do not accept this card!"
    }
    
    if(game$test) {
      disable("figLRC")
    }
  })
  
  observeEvent(input$figSBC, {
    req(length(game$teacher$accept) > 0)
    req(game$active)
    fig <- Figure$new(size = "small", color = "blue", shape = "circle")
    point <- game$teacher$givePoint(fig)
    game$moves <<- game$moves + 1
    game$score <<- game$score + point
    if(point > 0) {
      game$comm <<- "I accept this card!"
    } else if(game$test) {
      game$score <<- -1000
    } else {
      game$comm <<- "Sorry, I do not accept this card!"
    }
    
    if(game$test) {
      disable("figSBC")
    }
  })
  
  observeEvent(input$figLRS, {
    req(length(game$teacher$accept) > 0)
    req(game$active)
    fig <- Figure$new(size = "large", color = "red", shape = "square")
    point <- game$teacher$givePoint(fig)
    game$moves <<- game$moves + 1
    game$score <<- game$score + point
    if(point > 0) {
      game$comm <<- "I accept this card!"
    } else if(game$test) {
      game$score <<- -1000
    } else {
      game$comm <<- "Sorry, I do not accept this card!"
    }
    
    if(game$test) {
      disable("figLRS")
    }
  })
  
  observeEvent(input$figLBC, {
    req(length(game$teacher$accept) > 0)
    req(game$active)
    fig <- Figure$new(size = "large", color = "blue", shape = "circle")
    point <- game$teacher$givePoint(fig)
    game$moves <<- game$moves + 1
    game$score <<- game$score + point
    if(point > 0) {
      game$comm <<- "I accept this card!"
    } else if(game$test) {
      game$score <<- -1000
    } else {
      game$comm <<- "Sorry, I do not accept this card!"
    }
    
    if(game$test) {
      disable("figLBC")
    }
  })
 
  observeEvent(input$figLBS, {
    req(length(game$teacher$accept) > 0)
    req(game$active)
    fig <- Figure$new(size = "large", color = "blue", shape = "square")
    point <- game$teacher$givePoint(fig)
    game$moves <<- game$moves + 1
    game$score <<- game$score + point
    if(point > 0) {
      game$comm <<- "I accept this card!"
    } else if(game$test) {
      game$score <<- -1000
    } else {
      game$comm <<- "Sorry, I do not accept this card!"
    }
    
    if(game$test) {
      disable("figLBS")
    }
  })
  
  observeEvent(input$figSBS, {
    req(length(game$teacher$accept) > 0)
    req(game$active)
    fig <- Figure$new(size = "small", color = "blue", shape = "square")
    point <- game$teacher$givePoint(fig)
    game$moves <<- game$moves + 1
    game$score <<- game$score + point
    if(point > 0) {
      game$comm <<- "I accept this card!"
    } else if(game$test) {
      game$score <<- -1000
    } else {
      game$comm <<- "Sorry, I do not accept this card!"
    }
    
    if(game$test) {
      disable("figSBS")
    }
  })
  
  observeEvent(input$figSRC, {
    req(length(game$teacher$accept) > 0)
    req(game$active)
    fig <- Figure$new(size = "small", color = "red", shape = "circle")
    point <- game$teacher$givePoint(fig)
    game$moves <<- game$moves + 1
    game$score <<- game$score + point
    if(point > 0) {
      game$comm <<- "I accept this card!"
    } else if(game$test) {
      game$score <<- -1000
    } else {
      game$comm <<- "Sorry, I do not accept this card!"
    }
    
    if(game$test) {
      disable("figSRC")
    }
  })
  
  observeEvent(input$figSRS, {
    req(length(game$teacher$accept) > 0)
    req(game$active)
    fig <- Figure$new(size = "small", color = "red", shape = "square")
    point <- game$teacher$givePoint(fig)
    game$moves <<- game$moves + 1
    game$score <<- game$score + point
    if(point > 0) {
      game$comm <<- "I accept this card!"
    } else if(game$test) {
      game$score <<- -1000
    } else {
      game$comm <<- "Sorry, I do not accept this card!"
    }
    
    if(game$test) {
      disable("figSRS")
    }
  }) 
  
  observeEvent(input$figSGS, {
    req(length(game$teacher$accept) > 0)
    req(game$active)
    fig <- Figure$new(size = "small", color = "green", shape = "square")
    point <- game$teacher$givePoint(fig)
    game$moves <<- game$moves + 1
    game$score <<- game$score + point
    if(point > 0) {
      game$comm <<- "I accept this card!"
    } else if(game$test) {
      game$score <<- -1000
    } else {
      game$comm <<- "Sorry, I do not accept this card!"
    }
    
    if(game$test) {
      disable("figSGS")
    }
  }) 
  
  observeEvent(input$figLGS, {
    req(length(game$teacher$accept) > 0)
    req(game$active)
    fig <- Figure$new(size = "large", color = "green", shape = "square")
    point <- game$teacher$givePoint(fig)
    game$moves <<- game$moves + 1
    game$score <<- game$score + point
    if(point > 0) {
      game$comm <<- "I accept this card!"
    } else if(game$test) {
      game$score <<- -1000
    } else {
      game$comm <<- "Sorry, I do not accept this card!"
    }
    
    if(game$test) {
      disable("figLGS")
    }
  }) 
  
  observeEvent(input$figSGC, {
    req(length(game$teacher$accept) > 0)
    req(game$active)
    fig <- Figure$new(size = "small", color = "green", shape = "circle")
    point <- game$teacher$givePoint(fig)
    game$moves <<- game$moves + 1
    game$score <<- game$score + point
    if(point > 0) {
      game$comm <<- "I accept this card!"
    } else if(game$test) {
      game$score <<- -1000
    } else {
      game$comm <<- "Sorry, I do not accept this card!"
    }
    
    if(game$test) {
      disable("figSGC")
    }
  }) 
  
  observeEvent(input$figLGC, {
    req(length(game$teacher$accept) > 0)
    req(game$active)
    fig <- Figure$new(size = "large", color = "green", shape = "circle")
    point <- game$teacher$givePoint(fig)
    game$moves <<- game$moves + 1
    game$score <<- game$score + point
    if(point > 0) {
      game$comm <<- "I accept this card!"
    } else if(game$test) {
      game$score <<- -1000
    } else {
      game$comm <<- "Sorry, I do not accept this card!"
    }
    
    if(game$test) {
      disable("figLGC")
    }
  }) 
  
  observe({
    if(game$score == -1000) {
      game$comm <<- "Sorry, you lost! Click on the \"Play\" button to restart the game"
      game$active <<- FALSE
    }
  })

  output$communicate <- renderUI({
    req(game$comm)
    
    fluidRow(
      column(
        width = 12,
        game$comm
      )
    )
  })
  output$score <- renderUI({
    req(game$comm)
    req(game$active)
    req(game$test)
    fluidRow(
      column(
        width = 12,
        "Your current score is: ", game$score
      )
    )
  })
  
  output$moves <- renderUI({
    req(game$comm)
    req(game$active)
    req(!game$test)
    fluidRow(
      column(
        width = 12,
        "You have made ", game$moves, " moves."
      )
    )
  })
  
  observeEvent(input$testMode, {
    game$test <<- TRUE
    game$comm <<- "Let's test your knowledge! Make no mistake or you'll lose."
    game$score <<- 0
  })
  
  observe({
    req(game$test)
    if(game$score == length(game$teacher$accept)) {
      game$comm <<- "Congratulations, you have correctly learnt my rule. 
      Press \"Play\" to play again"
    }
  })
  
  # observe({
  #   req(!game$test)
  #   if(game$moves == 0) {
  #     game$comm <<- "You have no moves left. Now let's test your knowledge."
  #     game$test <<- TRUE
  #     game$score <<- 0
  #   }
  # })
})

