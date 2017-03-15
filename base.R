#####
## declaration of classes
#####

Figure <- setRefClass(
  "Figure",
  fields = list(color = "factor", size = "factor", shape = "factor")
)

# auxiliary function
isIn <- function(list, figure) {
  for(f in list) {
    if(identical(f, figure)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

Agent <- setRefClass(
  "Agent",
  
  fields = list(
    split = "list", 
    score = "numeric"
  ),
  
  methods = list(
    
    makeSplit = function(figures, dict){
      split <- list()
      # how many figures are there for splitting
      count <- length(figures)
      # how many labels are left - necessary to avoid degenerate/incomplete cases
      remain <- length(dict) - 1
      # random permutation
      figures <- sample(figures)
      for(s in dict) {
        if(remain > 0 & count > remain) {
          card <- sample.int(count - remain, 1)
        } else {
          card <- count
        }
        split[[s]] <- figures[1:card]
        figures <- figures[(card+1):length(figures)]
        count <- count - card
        remain <- remain - 1
      }
      
      return(split)
    },
    
    updateSplit = function(figure, communicate, point) {
      newSplit <- split
      if(!point) {
        while(TRUE) {
          newName <- sample(names(newSplit), 1)[[1]]
          if(newName != communicate) {
            break
          }
        }
        
        # remove figure from its previous category
        for(i in 1:length(newSplit[[communicate]])) {
          if(identical(newSplit[[communicate]][[i]], figure)) {
            newSplit[[communicate]][[i]] <- NULL
            break
          }
        }
        # add figure to a new category (may be the same as previously)
        newSplit[[newName]] <- c(newSplit[[newName]], figure)
      }
      return(newSplit)
    },
    
    sendCommunicate = function(figure) {
      for(i in 1:length(split)) {
        if(isIn(split[[i]], figure)) {
          return(names(split)[i])
        }
      }
      stop("I do not recognize the figure!")
    },
    
    sendFigure = function(communicate) {
      while (!(communicate %in% names(split))) {
        # choose randomly a figure
        figure <- sample(unlist(split))[[1]]
        # update split taking into account this figure and failure to communicate
        split <<- updateSplit(figure, communicate, 0)
      }
      return(sample(split[[communicate]], 1)[[1]])
    },
    
    givePoint = function(communicate, figure) {
      return(isIn(split[[communicate]], figure))
    },
    
    updateScore = function(point) {
      score <<- score + point
    }
  )
)


#####
## set-up simulation
#####

setEnvironment <- function(figDims, dict) {
  # input:
  # figDims - list - values for each dimension of the figure description (col, size, shape)
  
  # check configuration
  stopifnot(c("color", "size", "shape") %in% names(figDims))
  
# initialize figures
  combs <- expand.grid(figDims$color, figDims$size, figDims$shape)
  colnames(combs) <- c("color", "size", "shape")
  figures <- c()
  
  for(i in 1:nrow(combs)) {
    set <- combs[i,]
    newFig <- Figure$new(color = set$color, size = set$size, shape = set$shape)
    figures <- c(figures, newFig)
  }
  
  # initizalize agents
  player1 <- Agent$new(
    split = list(),
    score = 0
  )
  player1$split <- player1$makeSplit(figures, dict)
  
  player2 <- Agent$new(
    split = list(),
    score = 0
  )
  player2$split <- player2$makeSplit(figures, dict)
  
  env <- list()
  env$figures <- figures
  env$player1 <- player1
  env$player2 <- player2
  env$dict <- dict
  
  return(env)
}

drawFigure <- function(figures) {
  return(sample(figures, 1)[[1]])
}

oneRound <- function(env, isTwoWay) {
  
  # player1 starts as a sender (teacher)
  drawn1 <- drawFigure(env$figures)
  signal1 <- env$player1$sendCommunicate(drawn1)
  action1 <- env$player2$sendFigure(signal1)
  point1 <- env$player1$givePoint(signal1, action1)
  env$player2$updateScore(point1)
  
  if(isTwoWay) {
    # switch roles
    drawn2 <- drawFigure(env$figures)
    signal2 <- env$player2$sendCommunicate(drawn2)
    action2 <- env$player1$sendFigure(signal2)
    point2 <- env$player2$givePoint(signal2, action2)
    env$player1$updateScore(point2)
  }
  
  # update splits
  env$player2$split <- env$player2$updateSplit(action1, signal1, point1)
  if(isTwoWay) {
    env$player1$split <- env$player1$updateSplit(action1, signal1, point1)
    env$player1$split <- env$player1$updateSplit(action2, signal2, point2)
    env$player2$split <- env$player2$updateSplit(action2, signal2, point2)
  }
  return(env)
}

playGame <- function(n, figDims, dict, isTwoWay) {
  env <- setEnvironment(figDims, dict)
  avgScores <- list(player1 = c(), player2 = c())
  
  for(i in 1:n) {
    env <- oneRound(env, isTwoWay)
    avgScores$player1 <- c(avgScores$player1, env$player1$score/i)
    avgScores$player2 <- c(avgScores$player2, env$player2$score/i)
  }
  
  return(avgScores)
}

