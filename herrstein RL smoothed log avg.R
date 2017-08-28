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

title1 <- paste0("Distribution of the mean for two-way smoothed reinforcement learning with ",
                 "delta ",
                 delta1, " and lambda ", lambda1, ".")

fileName1 <- paste0('herrstein smoothed RL log 2way ', delta1, " ", lambda1,
                    ' sim.png')
sim1_smooth_avg <- runSimulationMeans(100, 20, 100, figDims, dict, twoWay, delta1, lambda1)
print(plotResSim(sim1_smooth_avg, title1))
dev.copy(png, fileName1, width = 500, height = 350)
dev.off()


title2 <- paste0("Distribution of the mean for two-way smoothed reinforcement learning with ",
                 "delta ",
                 delta2, " and lambda ", lambda2, ".")

fileName2 <- paste0('herrstein smoothed RL log 2way ', delta2, " ", lambda2,
                    ' sim.png')
sim2_smooth_avg <- runSimulationMeans(100, 20, 100, figDims, dict, twoWay, delta2, lambda2)
print(plotResSim(sim2_smooth_avg, title2))
dev.copy(png, fileName2, width = 500, height = 350)
dev.off()

# one-way
twoWay <- FALSE

title3 <- paste0("Distribution of the mean for two-way smoothed reinforcement learning with ",
                 "delta ",
                 delta1, " and lambda ", lambda1, ".")

fileName3 <- paste0('herrstein smoothed RL log 1way ', delta1, " ", lambda1,
                    ' sim.png')
sim3_smooth_avg <- runSimulationMeans(100, 20, 100, figDims, dict, twoWay, delta1, lambda1)
print(plotResSim(sim3_smooth_avg, title3))
dev.copy(png, fileName3, width = 500, height = 350)
dev.off()


title4 <- paste0("Distribution of the mean for two-way smoothed reinforcement learning with ",
                 "delta ",
                 delta2, " and lambda ", lambda2, ".")

fileName4 <- paste0('herrstein smoothed RL log 1way ', delta2, " ", lambda2,
                    ' sim.png')
sim4_smooth_avg <- runSimulationMeans(100, 20, 100, figDims, dict, twoWay, delta2, lambda2)
print(plotResSim(sim4_smooth_avg, title4))
dev.copy(png, fileName4, width = 500, height = 350)
dev.off()