# source main methods and functions
source('base.R')

#####
## learning agents - basic Herrnstein RL, memory limited to M
#####

hrlLearner <- setRefClass(
  "hrlLearner",
  fields = list(
    urns = "list",
    m = "numeric"
  ),
  contains = "Agent",
  methods = list(
    initUrns = function(figures, dict) {
      urns <- list()
      
      for(i in 1:length(figures)) {
        # initialize equal propensities
        # initialize history randomly
        # figure is stored in the first position of the list (cannot be a name)
        urns[[i]] <- c(figures[[i]], sample(dict))
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
        ind <- findUrn(figure)
        urns[[ind]] <<- c(urns[[ind]], communicate)
        urns[[ind]] <<- c(urns[[ind]][1], tail(urns[[ind]], m))
      }
    },
    
    updateSplit = function(figure, communicate, point) {
      updateUrns(figure, communicate, point)
      split <- list()
      
      for(i in 1:length(urns)) {
        urn <- unlist(urns[[i]][-1])
        figure <- urns[[i]][[1]]
        probs <- c()
        for(d in dict) {
          probs[d] <- sum(urn == d)/length(urn)
        }
        drawn <- sample(x = dict, size = 1, prob = probs)[[1]]
        split[[drawn]] <- c(split[[drawn]], figure)
      }
      # co robi� je�li wszystko nazwiemy tak samo - ok? jak powinien si� zachowa� agent?
      
      return(split)
    }
  )
)

# override setEnvironment

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
  player1 <- hrlLearner$new(
    split = list(),
    score = 0,
    urns = list(),
    m = 5
  )
  player1$split <- player1$makeSplit(figures, dict)
  player1$urns <- player1$initUrns(figures, dict)
  
  player2 <- hrlLearner$new(
    split = list(),
    score = 0,
    urns = list(),
    m = 10
  )
  player2$split <- player2$makeSplit(figures, dict)
  player2$urns <- player2$initUrns(figures, dict)
  
  env <- list()
  env$figures <- figures
  env$player1 <- player1
  env$player2 <- player2
  env$dict <- dict
  
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

res <- playGame(5000, figDims, dict, 1)
plotRes(res)

######
## run simulation
######

sim <- runSimulation(10, 500, figDims, dict, 0)
plotRes(sim)
