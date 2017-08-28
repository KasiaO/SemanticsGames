#####
## learning agents - basic Herrnstein RL, smoothed memory
#####

srlLearner <- setRefClass(
  "srlLearner",
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
  player <- srlLearner$new(
    split = list(),
    score = 0,
    urns = list(),
    delta = delta,
    lambda = lambda
  )
  return(player)
}

# set envirnment
setEnvironment <- function(figDims, dict, player1, player2) {

  # initialize figures
  figures <- initFigs(figDims)
  
  # configure players
  player1$split <- player1$makeSplit(figures, dict)
  player1$urns <- player1$initUrns(figures, dict)
  
  player2$split <- player2$makeSplit(figures, dict)
  player2$urns <- player2$initUrns(figures, dict)
  
  # set environment
  env <- list()
  env$figures <- figures
  env$player1 <- player1
  env$player2 <- player2
  env$dict <- dict
  
  return(env)
}

# override runSimulation
runSimulation <- function(iter, n, figDims, dict, isTwoWay, delta, lambda, raw = FALSE) {
  res <- list()
  for(i in 1:iter) {
    player1 <- initPlayer(delta = delta, lambda = lambda)
    player2 <- initPlayer(delta = delta, lambda = lambda)
    part <- playGame(n, figDims, dict, isTwoWay, player1, player2)
    res$player1[[i]] <- part$player1
    res$player2[[i]] <- part$player2
  }
  agg <- list()
  agg$player1 <- aggRes(res$player1, raw)
  agg$player2 <- aggRes(res$player2, raw)
  return(agg)
}

# override runSimulationMeans
runSimulationMeans <- function(iter, iterSim, n, figDims, dict, isTwoWay, delta, lambda, raw = FALSE) {
  res <- list()
  for(i in 1:iter) {
    part <- runSimulation(iterSim, n, figDims, dict, isTwoWay, delta, lambda)
    res$player1[[i]] <- part$player1$mean
    res$player2[[i]] <- part$player2$mean
  }
  agg <- list()
  agg$player1 <- aggRes(res$player1, raw)
  agg$player2 <- aggRes(res$player2, raw)
  
  return(agg)
}
