#####
## learning agents - smoothed reinforcement learning
#####

srlLearnerNat <- setRefClass(
  "srlLearnerNat",
  fields = list(
    urns = "list",
    delta = "numeric",
    lambda = "numeric"
  ),
  contains = "srlLearner",
  methods = list(
    givePointNat = function(figureDrawn, figureRec) {
      for(cat in split) {
        if(isIn(cat, figureDrawn)) {
          return(isIn(cat, figureRec))
        }
      }
      return(FALSE)
    }
  )
)

# override initPlayer
initPlayer <- function(delta, lambda) {
  player <- srlLearnerNat$new(
    split = list(),
    score = 0,
    urns = list(),
    delta = delta,
    lambda = lambda
  )
  return(player)
}

# override runSimulation
runSimulation <- function(iter, n, figDims, dict, isTwoWay, delta, lambda, wp, wn, raw = FALSE) {
  res <- list()
  for(i in 1:iter) {
    player1 <- initPlayer(delta = delta, lambda = lambda)
    player2 <- initPlayer(delta = delta, lambda = lambda)
    part <- playGame(n, figDims, dict, isTwoWay, player1, player2, wp, wn)
    res$player1[[i]] <- part$player1
    res$player2[[i]] <- part$player2
  }
  agg <- list()
  agg$player1 <- aggRes(res$player1, raw)
  agg$player2 <- aggRes(res$player2, raw)
  return(agg)
}

# override runSimulationMeans
runSimulationMeans <- function(iter, iterSim, n, figDims, dict, isTwoWay, delta,
                               lambda, wp, wn, raw = FALSE) {
  res <- list()
  for(i in 1:iter) {
    print(i)
    part <- runSimulation(iterSim, n, figDims, dict, isTwoWay, delta, lambda, wp, wn, raw)
    res$player1[[i]] <- part$player1$mean
    res$player2[[i]] <- part$player2$mean
  }
  agg <- list()
  agg$player1 <- aggRes(res$player1, raw)
  agg$player2 <- aggRes(res$player2, raw)
  
  return(agg)
}
