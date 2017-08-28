#####
## learning agents - basic Herrnstein RL, full memory
#####

hrlLearnerNat <- setRefClass(
  "hrlLearnerNat",
  fields = list(
    urns = "list",
    split = "list",
    score = "numeric"
  ),
  contains = "hrlLearner",
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
initPlayer <- function() {
  player <- hrlLearnerNat$new(
    split = list(),
    score = 0,
    urns = list()
  )
  return(player)
}

# create Nature
initNature <- function() {
  nature <- hrlLearnerNat$new(
    split = list(),
    score = 0,
    urns = list()
  )
  return(nature)
}

# override setEnvironment
setEnvironment <- function(figDims, dict, player1, player2, wp, wn) {
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
  env$nature <- initNature()
  env$nature$split <- env$nature$makeSplit(figures, dict)
  env$wp <- wp
  env$wn <- wn
  
  return(env)
}

# override oneRound
oneRound <- function(env, isTwoWay) {
  
  # player1 starts as a sender
  drawn1 <- drawFigure(env$figures)
  signal1 <- env$player1$sendCommunicate(drawn1)
  action1 <- env$player2$sendFigure(signal1)
  point1p <- env$player1$givePoint(signal1, action1)
  point1n <- env$nature$givePointNat(drawn1, action1)
  point1 <- env$wp*point1p + env$wn*point1n
  env$player2$updateScore(point1)
  
  # switch roles
  drawn2 <- drawFigure(env$figures)
  signal2 <- env$player2$sendCommunicate(drawn2)
  action2 <- env$player1$sendFigure(signal2)
  point2p <- env$player2$givePoint(signal2, action2)
  point2n <- env$nature$givePointNat(drawn2, action2)
  point2 <- env$wp*point2p + env$wn*point2n
  env$player1$updateScore(point2)
  
  # update splits
  # first round - P1: sender, P2: receiver
  # sender learns by coordination
  env$player1$split <- env$player1$updateSplit(action1, signal1, point1p)
  # sender learns by nature
  env$player1$split <- env$player1$updateSplit(drawn1, signal1, point1n)
  # receiver learns by coordination
  env$player2$split <- env$player2$updateSplit(action1, signal1, point1p)
  
  # second round - P1: receiver, P2: sender
  # sender learns by coordination
  env$player2$split <- env$player2$updateSplit(action2, signal2, point2p)
  # sender learns by nature
  env$player2$split <- env$player2$updateSplit(drawn2, signal2, point2n)
  # receiver learns by coordination
  env$player1$split <- env$player1$updateSplit(action2, signal2, point2p)

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
  
  return(avgScores)
}

# override runSimulation
runSimulation <- function(iter, n, figDims, dict, isTwoWay, wp, wn, raw = FALSE) {
  res <- list()
  for(i in 1:iter) {
    player1 <- initPlayer()
    player2 <- initPlayer()
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
runSimulationMeans <- function(iter, iterSim, n, figDims, dict, isTwoWay, wp, wn, raw = FALSE) {
  res <- list()
  for(i in 1:iter) {
    part <- runSimulation(iterSim, n, figDims, dict, isTwoWay, wp, wn, raw)
    res$player1[[i]] <- part$player1$mean
    res$player2[[i]] <- part$player2$mean
  }
  agg <- list()
  agg$player1 <- aggRes(res$player1, raw)
  agg$player2 <- aggRes(res$player2, raw)
  
  return(agg)
}