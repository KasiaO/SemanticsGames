#####
## import
#####
library(ggplot2)


#####
## declaration of classes
#####

Figure <- setRefClass(
  "Figure",
  fields = list(color = "factor", size = "factor", shape = "factor")
)

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

    # updateSplit = function(figure, communicate, point) {
    #   newSplit <- split
    #   if(!point) {
    # 
    #     # remove figure from its previous category
    #     newSplit[[communicate]] <- lapply(newSplit[[communicate]], function(x) {
    #       if(identical(x, figure)) {
    #         return(NULL)
    #       } else {
    #         return(x)
    #       }
    #     })
    # 
    #     # add it to new category
    #     newName <- communicate
    #     while(newName == communicate) {
    #       newName <- sample(names(newSplit), 1)[[1]]
    #     }
    # 
    #     # add figure to a new category
    #     newSplit[[newName]] <- c(newSplit[[newName]], figure)
    #    }
    #   return(newSplit)
    # },
    
    sendCommunicate = function(figure) {
      for(i in 1:length(split)) {
        if(isIn(split[[i]], figure)) {
          return(names(split)[i])
        }
      }
      stop("I do not recognize the figure!")
    },
    
    sendFigure = function(communicate) {
      while (length(split[[communicate]]) == 0) {
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
## functions
#####

isIn <- function(list, figure) {
  for(f in list) {
    if(identical(f, figure)) {
      return(TRUE)
    }
  }
  return(FALSE)
}


testMeans <- function(res1, res2) {
  pvalues <- c()
  for(i in 1:length(res1)) {
    pvalues[i] <- t.test(res1[[i]], res2[[i]])$p.value
  }
  return(pvalues <= 0.05)
}

drawFigure <- function(figures) {
  return(sample(figures, 1)[[1]])
}

oneRound <- function(env, isTwoWay) {
  # player1 starts as a sender
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

playGame <- function(n, figDims, dict, isTwoWay, player1, player2) {
  env <- setEnvironment(figDims, dict, player1, player2)
  avgScores <- list(player1 = c(), player2 = c())
  
  for(i in 1:n) {
    env <- oneRound(env, isTwoWay)
    avgScores$player1 <- c(avgScores$player1, env$player1$score/i)
    avgScores$player2 <- c(avgScores$player2, env$player2$score/i)
  }
  
  return(avgScores)
}

initFigs <- function(figDims) {
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
  return(figures)
}

plotRes <- function(res, title) {
  res <- as.data.frame(res)
  if(any(res$player1) && any(res$player2)) {
    g <- ggplot(res, aes(1:nrow(res), player1, player2)) + 
      geom_line(aes(y = player1, colour = "Player 1")) + 
      geom_line(aes(y = player2, colour = "Player 2")) +
      xlab("Iteration") + scale_y_continuous(limits = c(0, 1)) +
      ggtitle(title) + theme(plot.title = element_text(lineheight = 0.8, size = '10')) +
      theme(legend.position="bottom", legend.title=element_blank())
  } else if(any(res$player1)) {
    g <- ggplot(res, aes(1:nrow(res), player1)) + 
      geom_line(aes(y = player1, colour = "Player 1")) +
      xlab("Iteration") + scale_y_continuous(limits = c(0, 1)) +
      ggtitle(title) + theme(plot.title = element_text(lineheight = 0.8, size = '10')) +
      theme(legend.position="bottom", legend.title=element_blank())
  } else {
    g <- ggplot(res, aes(1:nrow(res), player2)) + 
      geom_line(aes(y = player2, colour = "Player 2")) +
      xlab("Iteration") + scale_y_continuous(limits = c(0, 1)) +
      ggtitle(title) + theme(plot.title = element_text(lineheight = 0.8, size = '10')) +
      theme(legend.position="bottom", legend.title=element_blank())
  }
  
  return(g)
}

aggRes <- function(playerRes, raw) {
  agg <- list()
  for(i in 1:length(playerRes[[1]])) {
    part <- unlist(lapply(playerRes, function(x) x[i]))
    n <- length(part)
    avg <- mean(part)
    stdev <- sd(part)
    agg$stdev[[i]] <- stdev
    agg$mean[[i]] <- avg
    agg$low[[i]] <- quantile(part, 0.025)
    agg$hi[[i]] <- quantile(part, 0.975)
    if(raw) {
      agg$raw[[i]] <- part
    }
  }
  return(agg)
}

#####
## simulation
#####

runSimulation <- function(iter, n, figDims, dict, isTwoWay, raw = FALSE) {
  res <- list()
  for(i in 1:iter) {
    player1 <- initPlayer()
    player2 <- initPlayer()
    part <- playGame(n, figDims, dict, isTwoWay, player1, player2)
    res$player1[[i]] <- part$player1
    res$player2[[i]] <- part$player2
  }
  agg <- list()
  agg$player1 <- aggRes(res$player1, raw)
  agg$player2 <- aggRes(res$player2, raw)
  
  return(agg)
}

runSimulationMeans <- function(iter, iterSim, n, figDims, dict, isTwoWay, raw = FALSE) {
  res <- list()
  for(i in 1:iter) {
    part <- runSimulation(iterSim, n, figDims, dict, isTwoWay)
    res$player1[[i]] <- part$player1$mean
    res$player2[[i]] <- part$player2$mean
  }
  agg <- list()
  agg$player1 <- aggRes(res$player1, raw)
  agg$player2 <- aggRes(res$player2, raw)
  
  return(agg)
}

plotResSim <- function(res, title) {
  res <- as.data.frame(cbind(
    res$player1$mean,
    res$player1$low,
    res$player1$hi, 
    res$player2$mean,
    res$player2$low,
    res$player2$hi)
  )
  colnames(res) <- c("player1", "hi1", "low1", "player2", "hi2", "low2")
  if(any(res$player1) && any(res$player2)) {
    g <- ggplot(res, aes(1:nrow(res), player1, player2)) + 
      geom_ribbon(aes(ymin = res$low1, ymax = res$hi1), fill = "mistyrose2") +
      geom_ribbon(aes(ymin = res$low2, ymax = res$hi2), fill = "lightskyblue1", alpha = '0.4') +
      geom_line(aes(y = player1, colour = "Player 1")) + 
      geom_line(aes(y = player2, colour = "Player 2"))
  } else if(any(res$player1)) {
    g <- ggplot(res, aes(1:nrow(res), player1)) + 
      geom_ribbon(aes(ymin = res$low1, ymax = res$hi1), fill = "lightskyblue1", alpha = '0.4') +
      geom_line(aes(y = player1, colour = "Player 1"))
  } else {
    g <- ggplot(res, aes(1:nrow(res), player2)) + 
      geom_ribbon(aes(ymin = res$low2, ymax = res$hi2), fill = "mistyrose2") +
      geom_line(aes(y = player2, colour = "Player 2"))
  }
  
  g <- g + xlab("Round") + scale_y_continuous(limits = c(0, 1)) + ylab("Success rate") +
    ggtitle(title) + theme(plot.title = element_text(lineheight = 0.8, size = '10')) +
    theme(legend.position="bottom", legend.title=element_blank())
  return(g)
}

plotResComp <- function(res1, res2, alg1, alg2, title) {
  res1 <- as.data.frame(cbind(
    res1$player1$mean,
    res1$player1$low,
    res1$player1$hi, 
    res1$player2$mean,
    res1$player2$low,
    res1$player2$hi)
  )
  colnames(res1) <- c("player1", "hi1", "low1", "player2", "hi2", "low2")
  g <- ggplot(res1, aes(1:nrow(res1), player2)) +
    geom_ribbon(aes(ymin = res1$low2, ymax = res1$hi2), fill = '#F8766D',
                alpha = '0.1') +
    geom_line(aes(y = player2, colour = alg1)) + ylab("Mean success rate") +
    xlab("Round") + scale_y_continuous(limits = c(0, 1)) +
    ggtitle(title) + theme(plot.title = element_text(lineheight = 0.8, size = '10'))
  
  # add res2 using the same logic
  res2 <- as.data.frame(cbind(
    res2$player1$mean,
    res2$player1$low,
    res2$player1$hi, 
    res2$player2$mean,
    res2$player2$low,
    res2$player2$hi)
  )
  colnames(res2) <- c("player1", "hi1", "low1", "player2", "hi2", "low2")
  g <- g + geom_ribbon(aes(ymin = res2$low2, ymax = res2$hi2), fill = '#00BFC4', 
                       alpha = '0.1') +
    geom_line(aes(y = player2, colour = alg2), res2) +
    theme(legend.position="bottom", legend.title=element_blank())
  return(g)
}


#####
## logging functions (deprecated)
#####

logFig <- function(figure) {
  figString <- paste(
      as.character(figure$color),
      as.character(figure$size),
      as.character(figure$shape)
    )
  return(figString)
}

logSplit <- function(split) {
  string <- c()
  for(d in names(split)) {
    string <- paste0(string, d, ":")
    for(fig in split[[d]]) {
      string <- paste(string, logFig(fig), ", ")
    }
  }
  return(string)
}

logEx <- function(split1, split2, signal, action, point) {
  log <- data.frame(
    split1 = logSplit(split1),
    split2 = logSplit(split2),
    signal,
    #action = logFig(action),
    action = action,
    point
  )
  return(log)
}

initLog <- function(){
  log <- data.frame(
    split1 = character(),
    split2 = character(),
    signal = character(),
    action = character(),
    point = logical()
  )
  return(log)
}

addLogEntry <- function(log, split1, split2, signal, action, point) {
  return(rbind(log, logEx(split1, split2, signal, action, point)))
}

