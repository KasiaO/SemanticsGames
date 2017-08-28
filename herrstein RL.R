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
        # for each urn extract propensities
        urn <- unlist(urns[[i]][-1])
        # for each urn extract figure it corresponds to
        figure <- urns[[i]][[1]]
        # calculate probabilities linearly
        probs <- urn/sum(urn)
        # choose a name for this figure
        drawn <- sample(x = dict, size = 1, prob = probs)[[1]]
        # add figure to this part of the split
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

# set envirnment (agent type specific function)
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
