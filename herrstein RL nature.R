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
      urns[[findUrn(figure)]][[communicate]] <<- urns[[findUrn(figure)]][[communicate]] + point
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

setEnvironment <- function(figDims, dict, player1, player2, wp, wn) {
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
  env$nature <- initNature()
  env$nature$split <- env$nature$makeSplit(figures, dict)
  env$wp <- wp
  env$wn <- wn
  
  return(env)
}

# override oneRound
oneRound <- function(env, isTwoWay) {
  
  # player1 starts as a sender (teacher)
  drawn1 <- drawFigure(env$figures)
  signal1 <- env$player1$sendCommunicate(drawn1)
  action1 <- env$player2$sendFigure(signal1)
  point1p <- env$player1$givePoint(signal1, action1)
  point1n <- env$nature$givePoint(signal1, action1)
  point1 <- env$wp*point1p + env$wn*point1n
  env$player2$updateScore(point1)
  
  if(isTwoWay) {
    # switch roles
    drawn2 <- drawFigure(env$figures)
    signal2 <- env$player2$sendCommunicate(drawn2)
    action2 <- env$player1$sendFigure(signal2)
    point2p <- env$player2$givePoint(signal2, action2)
    point2n <- env$nature$givePoint(signal2, action2)
    point2 <- env$wp*point2p + env$wn*point2n
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

# override playGame

playGame <- function(n, figDims, dict, isTwoWay, player1, player2, wp, wn) {
  env <- setEnvironment(figDims, dict, player1, player2, wp, wn)
  avgScores <- list(player1 = c(), player2 = c())
  
  for(i in 1:n) {
    env <- oneRound(env, isTwoWay)
    avgScores$player1 <- c(avgScores$player1, env$player1$score/i)
    avgScores$player2 <- c(avgScores$player2, env$player2$score/i)
  }
  
  #write.table(env$log, file = "log.csv", sep = ";")
  
  return(avgScores)
}

# override runSimulation
runSimulation <- function(iter, n, figDims, dict, isTwoWay, wp, wn) {
  res <- list()
  for(i in 1:iter) {
    player1 <- initPlayer()
    player2 <- initPlayer()
    part <- playGame(n, figDims, dict, isTwoWay, player1, player2, wp, wn)
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
for(wp in c(0.1, 0.5, 0.9)) {
  wn <- 1 - wp
  
  player1 <- initPlayer()
  player2 <- initPlayer()
  
  title <- paste0("Two-way Herrstein reinforcement learning 
                  with unlimited memory and external factor of weight ", wn)
  
  fileName <- paste0('herrstein RL 2way nature ', wn, '.png')
  res <- playGame(5000, figDims, dict, twoWay, player1, player2, wp, wn)
  print(plotRes(res, title))
  dev.copy(png, fileName, width = 500, height = 350)
  dev.off()
  
  
  ######
  ## run simulation
  ######
  
  title <- paste0("Average learning curve for two-way Herrstein reinforcement learning 
                  with unlimited memory and external factor of weight ", wn)
  
  fileName <- paste0('herrstein RL 2way nature ', wn, ' sim.png')
  
  sim <- runSimulation(20, 500, figDims, dict, twoWay, wp, wn)
  print(plotRes(sim, title))
  dev.copy(png, fileName, width = 500, height = 350)
  dev.off()
}

