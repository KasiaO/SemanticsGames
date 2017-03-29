# source main methods and functions
source('base.R')

#####
## learning agents - basic Herrnstein RL, full memory
#####

hrlLearner <- setRefClass(
  "hrlLearner",
  fields = list(
    urns = "list",
    split = "list",
    score = "numeric"
  ),
  contains = "Agent",
  methods = list(
    initUrns = function(figures, dict) {
      urns <- list()
      
      for(i in 1:length(figures)) {
        # initialize equal propensities
        # figure is stored in the first position of the list (cannot be a name)
        urns[[i]] <- c(figures[[i]], rep(1, length = length(dict)))
        names(urns[[i]]) <- c("figure", dict)
      }
      return(urns)
    },
    
    findUrn = function(figure) {
      for(i in 1:length(urns)) {
        if(identical(urns[[i]][[1]], figure)) {
          return(i)
        }
      }
    },
    
    # extractFigs = function(urns) {
    #   figures <- list()
    #   for(u in urns) {
    #     figures <- c(figures, u[[1]])
    #   }
    #   return(figures)
    # },
    
    updateUrns = function(figure, communicate, point) {
      # no penalty
      if(point) {
        urns[[findUrn(figure)]][[communicate]] <<- urns[[findUrn(figure)]][[communicate]] + 1
      }
    },
    
    updateSplit = function(figure, communicate, point) {
      updateUrns(figure, communicate, point)
      split <- list()
      
      for(i in 1:length(urns)) {
        urn <- unlist(urns[[i]][-1])
        figure <- urns[[i]][[1]]
        probs <- urn/sum(urn)
        drawn <- sample(x = dict, size = 1, prob = probs)[[1]]
        split[[drawn]] <- c(split[[drawn]], figure)
      }
      
      return(split)
    }
  )
)

# create player
initPlayer <- function() {
  player <- hrlLearner$new(
    split = list(),
    score = 0,
    urns = list()
  )
  return(player)
}

# override setEnvironment

setEnvironment <- function(figDims, dict, player1, player2) {
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
  
  #  configure players
  player1$split <- player1$makeSplit(figures, dict)
  player1$urns <- player1$initUrns(figures, dict)
  
  player2$split <- player2$makeSplit(figures, dict)
  player2$urns <- player2$initUrns(figures, dict)
  
  env <- list()
  env$figures <- figures
  env$player1 <- player1
  env$player2 <- player2
  env$dict <- dict
  env$log <- initLog()
  
  return(env)
}

#####
## run experiment
#####

figDims <- list(
  "color" = c("white", "red"),
  "size" = c("small", "big"),
  "shape" = c("square", "triangle")
)

dict <- c("A", "B")

for(twoWay in c(TRUE, FALSE)) {
  player1 <- initPlayer()
  player2 <- initPlayer()
  
  title <- if(twoWay) {
    paste0("Two-way Herrstein reinforcement learning with unlimited memory.")
  } else {
    paste0("One-way Herrstein reinforcement learning with unlimited memory.")
  }
  
  fileName <- if(twoWay) {
    'herrstein RL 2way unlim.png'
  } else {
    'herrstein RL 1way unlim.png'
  }
  
  res <- playGame(5000, figDims, dict, twoWay, player1, player2)
  print(plotRes(res, title))
  dev.copy(png, fileName)
  dev.off()
}


######
## run simulation
######

for(twoWay in c(TRUE, FALSE)) {
  title <- if(twoWay) {
    paste0("Average learning curve for two-way Herrstein reinforcement learning 
           with unlimited memory.")
  } else {
    paste0("Average learning curve for one-way Herrstein reinforcement learning 
           with unlimited memory.")
  }
  
  fileName <- if(twoWay) {
    'herrstein RL 2way unlim sim.png'
  } else {
    'herrstein RL 1way unlim sim.png'
  }
  
  sim <- runSimulation(20, 500, figDims, dict, twoWay)
  print(plotRes(sim, title))
  dev.copy(png, fileName)
  dev.off()
}
