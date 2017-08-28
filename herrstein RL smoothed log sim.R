#####
## import
#####
# source main methods and functions
source('base.R')
source('herrstein RL smoothed log.R')

#####
## run experiment
#####

figDims <- list(
  "color" = c("white", "red"),
  "size" = c("small", "big"),
  "shape" = c("square", "triangle")
)

dict <- c("A", "B")

# two-way
twoWay <- TRUE
delta1 <- 0.1
delta2 <- 0.9
lambda1 <- 5
lambda2 <- 25

title1 <- paste0("Average learning curve for two-way smoothed reinforcement learning with
                 delta ", delta1, " and lambda ", lambda1, ".")

fileName1 <- paste0('herrstein smoothed RL log 2way ', delta1, " ", lambda1,
                    '.png')
sim1_smooth <- runSimulation(100, 2000, figDims, dict, twoWay, delta1, lambda1)
print(plotResSim(sim1_smooth, title1))
dev.copy(png, fileName1, width = 500, height = 350)
dev.off()

title2 <- paste0("Average learning curve for two-way smoothed reinforcement learning with
                 delta ", delta1, " and lambda ", lambda2, ".")

fileName2 <- paste0('herrstein smoothed RL log 2way ', delta1, " ", lambda2,
                    '.png')
sim2_smooth <- runSimulation(100, 2000, figDims, dict, twoWay, delta1, lambda2)
print(plotResSim(sim2_smooth, title2))
dev.copy(png, fileName2, width = 500, height = 350)
dev.off()

title3 <- paste0("Average learning curve for two-way smoothed reinforcement learning with
                 delta ", delta2, " and lambda ", lambda1, ".")

fileName3 <- paste0('herrstein smoothed RL log 2way ', delta2, " ", lambda1,
                    '.png')
sim3_smooth <- runSimulation(100, 2000, figDims, dict, twoWay, delta2, lambda1)
print(plotResSim(sim3_smooth, title3))
dev.copy(png, fileName3, width = 500, height = 350)
dev.off()

title4 <- paste0("Average learning curve for two-way smoothed reinforcement learning with
                 delta ", delta2, " and lambda ", lambda2, ".")

fileName4 <- paste0('herrstein smoothed RL log 2way ', delta2, " ", lambda2,
                    '.png')
sim4_smooth <- runSimulation(100, 2000, figDims, dict, twoWay, delta2, lambda2)
print(plotResSim(sim4_smooth, title4))
dev.copy(png, fileName4, width = 500, height = 350)
dev.off()

# one-way
twoWay <- FALSE
for(delta in c(0.1, 0.9)) {
  for(lambda in c(5,25)) {

    title <- paste0("Average learning curve for one-way Herrstein smoothed reinforcement 
                    learning (log) with delta ",
                    delta, " and lambda ", lambda, " for Player 2.")

    fileName <- paste0('herrstein smoothed RL log 1way player2 ',
                       delta, " ", lambda, ' sim.png')
    sim <- runSimulation(100, 2000, figDims, dict, twoWay, delta, lambda)
    print(plotResSim(sim, title))
    dev.copy(png, fileName, width = 500, height = 350)
    dev.off()
  }
}