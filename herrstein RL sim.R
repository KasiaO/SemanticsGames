#####
## import
#####
source('herrstein RL.R')


######
## run simulation
######

figDims <- list(
  "color" = c("white", "red"),
  "size" = c("small", "big"),
  "shape" = c("square", "triangle")
)

dict <- c("A", "B")

# one-way
twoWay <- FALSE
title <- paste0("Average learning curve for one-way Herrstein reinforcement learning ",
                "with unlimited memory.")

fileName <- 'herrstein RL 1way unlim.png'

sim1_hrl <- runSimulation(100, 2000, figDims, dict, twoWay)
plotResSim(sim1_hrl, title)
dev.copy(png, fileName, width = 500, height = 350)
dev.off()


# two-way
twoWay <- TRUE
title <- paste0("Average learning curve for two-way Herrstein reinforcement learning ",
                "with unlimited memory.")

fileName <- 'herrstein RL 2way unlim.png'

sim2_hrl <- runSimulation(100, 2000, figDims, dict, twoWay)
plotResSim(sim2_hrl, title)
dev.copy(png, fileName, width = 500, height = 350)
dev.off()

#####
## means
#####

# one-way
twoWay <- FALSE
title <- paste0("Distribution of the mean for one-way Herrstein reinforcement learning ",
                "with unlimited memory.")

fileName <- 'herrstein RL 1way unlim sim.png'

sim1_hrl_avg <- runSimulationMeans(100, 20, 100, figDims, dict, twoWay)
plotResSim(sim1_hrl_avg, title)
dev.copy(png, fileName, width = 500, height = 350)
dev.off()


# two-way
twoWay <- TRUE
title <- paste0("Distribution of the mean for two-way Herrstein reinforcement learning ",
                "with unlimited memory.")

fileName <- 'herrstein RL 2way unlim sim.png'

sim2_hrl_avg <- runSimulationMeans(100, 20, 100, figDims, dict, twoWay)
plotResSim(sim2_hrl_avg, title)
dev.copy(png, fileName, width = 500, height = 350)
dev.off()