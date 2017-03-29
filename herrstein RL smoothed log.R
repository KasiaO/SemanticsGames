# source main methods and functions
source('base.R')

#####
## learning agents - basic Herrnstein RL, smoothed memory
#####

hrlLearner <- setRefClass(
  "hrlLearner",
  fields = list(
    urns = "list",
    delta = "numeric",
    lambda = "numeric"
  ),
  contains = "Agent",
  methods = list(
    initUrns = function(figures, dict) {
      urns <- list()
      
      for(i in 1:length(figures)) {
        # initialize equal propensities
        # figure is stored in the first position of the list (cannot be a name)
        urns[[i]] <- c(figures[[i]], rep(1/length(dict), length = length(dict)))
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
    
    updateUrns = function(figure, communicate, point) {
      # no penalty
      urns[[findUrn(figure)]][[communicate]] <<- ({
        urns[[findUrn(figure)]][[communicate]]*(1-delta) + delta*point          
      })
    },
    
    calcProbs = function(urn) {
      es <- exp(lambda*urn)
      probs <- es/sum(es)
      return(probs)
    },
    
    updateSplit = function(figure, communicate, point) {
      updateUrns(figure, communicate, point)
      split <- list()
      
      for(i in 1:length(urns)) {
        urn <- unlist(urns[[i]][-1])
        figure <- urns[[i]][[1]]
        probs <- urn/sum(urn)
        drawn <- sample(x = dict, size = 1, prob = calcProbs(urn))[[1]]
        split[[drawn]] <- c(split[[drawn]], figure)
      }
      
      return(split)
    }
  )
)

# create player
initPlayer <- function(delta, lambda) {
  player <- hrlLearner$new(
    split = list(),
    score = 0,
    urns = list(),
    delta = delta,
    lambda = lambda
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
  
  # initizalize agents

  player1$split <- player1$makeSplit(figures, dict)
  player1$urns <- player1$initUrns(figures, dict)
  
  player2$split <- player2$makeSplit(figures, dict)
  player2$urns <- player2$initUrns(figures, dict)
  
  env <- list()
  env$figures <- figures
  env$player1 <- player1
  env$player2 <- player2
  env$dict <- dict
  
  return(env)
}

# override runSimulation
runSimulation <- function(iter, n, figDims, dict, isTwoWay, delta1, delta2, lambda1, lambda2) {
  res <- list()
  for(i in 1:iter) {
    player1 <- initPlayer(delta = delta1, lambda = lambda1)
    player2 <- initPlayer(delta = delta2, lambda = lambda2)
    part <- playGame(n, figDims, dict, isTwoWay, player1, player2)
    res$player1[[i]] <- part$player1
    res$player2[[i]] <- part$player2
  }
  agg <- list()
  agg$player1 <- aggRes(res$player1)
  agg$player2 <- aggRes(res$player2)
  return(agg)
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

twoWay <- TRUE
  for(delta1 in c(0.1, 0.9)) {
    for(delta2 in c(0.1, 0.9)) {
      for(lambda1 in c(5, 25)) {
        for(lambda2 in c(5,25)) {
          player1 <- initPlayer(delta = delta1, lambda = lambda1)
          player2 <- initPlayer(delta = delta2, lambda = lambda2)
          
          title <- paste0("Two-way Herrstein smoothed reinforcement learning (log) with 
                          delta ", 
                   delta1, " and lambda ", lambda1, " for Player 1 and delta ", 
                   delta2, " and lambda ", lambda2, " for Player 2.")
          
          fileName <- paste0('herrstein smoothed log RL 2way player1', delta1, " ", lambda1, 
          ' player2 ', delta2, " ", lambda2, '.png')
          res <- playGame(5000, figDims, dict, twoWay, player1, player2)
          print(plotRes(res, title))
          dev.copy(png, fileName)
          dev.off()
        }
      }
    }
  }

twoWay <- FALSE
for(delta2 in c(0.1, 0.9)) {
    for(lambda2 in c(5,25)) {
      # will not affect results
      delta1 <- 0
      lambda1 <- 0
      player1 <- initPlayer(delta = delta1, lambda = lambda1)
      player2 <- initPlayer(delta = delta2, lambda = lambda2)
      
      title <- paste0("One-way Herrstein smoothed reinforcement (log) learning with 
                      delta ", 
                      delta2, " and lambda ", lambda2, " for Player 2.")
      
      fileName <- paste0('herrstein smoothed RL log 1way player2 ', 
                         delta2, " ", lambda2, '.png')
      res <- playGame(5000, figDims, dict, twoWay, player1, player2)
      print(plotRes(res, title))
      dev.copy(png, fileName)
      dev.off()
    }
  }



######
## run simulation
######

twoWay <- TRUE
for(delta1 in c(0.1, 0.9)) {
  for(delta2 in c(0.1, 0.9)) {
    for(lambda1 in c(5, 25)) {
      for(lambda2 in c(5,25)) {
        player1 <- initPlayer(delta = delta1, lambda = lambda1)
        player2 <- initPlayer(delta = delta2, lambda = lambda2)
        
        title <- paste0("Average learning curve for two-way Herrstein smoothed reinforcement learning (log) with 
                        delta ", 
                        delta1, " and lambda ", lambda1, " for Player 1 and delta ", 
                        delta2, " and lambda ", lambda2, " for Player 2.")
        
        fileName <- paste0('herrstein smoothed RL log 2way player1', delta1, " ", lambda1, 
                           ' player2 ', delta2, " ", lambda2, ' sim.png')
        sim <- runSimulation(20, 500, figDims, dict, twoWay, delta1, delta2, lambda1, lambda2)
        print(plotRes(sim, title))
        dev.copy(png, fileName)
        dev.off()
      }
    }
  }
}

twoWay <- FALSE
for(delta2 in c(0.1, 0.9)) {
  for(lambda2 in c(5,25)) {
    # will not affect results
    delta1 <- 0
    lambda1 <- 1
    player1 <- initPlayer(delta = delta1, lambda = lambda1)
    player2 <- initPlayer(delta = delta2, lambda = lambda2)
    
    title <- paste0("Average learning curve for one-way Herrstein smoothed reinforcement learning (log) with 
                      delta ", 
                    delta2, " and lambda ", lambda2, " for Player 2.")
    
    fileName <- paste0('herrstein smoothed RL log 1way player2 ', 
                       delta2, " ", lambda2, ' sim.png')
    sim <- runSimulation(20, 500, figDims, dict, twoWay, delta1, delta2, lambda1, lambda2)
    print(plotRes(sim, title))
    dev.copy(png, fileName)
    dev.off()
  }
}
