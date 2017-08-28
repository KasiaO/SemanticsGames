#####
## import
#####
# source main methods and functions
source('base.R')
source('herrstein RL smoothed log.R')
source('herrstein RL nature mod.R')
source('herrstein RL smoothed log nat.R')

######
## run simulation
######

wp1 <- 0.1
wp2 <- 0.5
wp3 <- 0.9

wn1 <- 1 - wp1
wn2 <- 1 - wp2
wn3 <- 1 - wp3
# two-way
twoWay <- TRUE
delta <- 0.9
lambda <- 25
  
title1 <- paste0("Average learning curve for two-way smoothed reinforcement learning with
                 delta ",
                 delta, " and lambda ", lambda, " and external factor of weight ", wn1, ".")

fileName1 <- paste0('herrstein smoothed RL log 2way ', delta, " ", lambda,
                    ' nat ', wn1, '.png')
sim1_smooth_nat <- runSimulation(100, 2000, figDims, dict, twoWay, delta, lambda, wp1, wn1)
print(plotResSim(sim1_smooth_nat, title1))
dev.copy(png, fileName1, width = 500, height = 350)
dev.off()

title2 <- paste0("Average learning curve for two-way smoothed reinforcement learning with
                 delta ",
                delta, " and lambda ", lambda, " and external factor of weight ", wn2, ".")

fileName2 <- paste0('herrstein smoothed RL log 2way ', delta, " ", lambda,
                   ' nat ', wn2, '.png')
sim2_smooth_nat <- runSimulation(100, 2000, figDims, dict, twoWay, delta, lambda, wp2, wn2)
print(plotResSim(sim2_smooth_nat, title2))
dev.copy(png, fileName2, width = 500, height = 350)
dev.off()

title3 <- paste0("Average learning curve for two-way smoothed reinforcement learning with
                 delta ",
                delta, " and lambda ", lambda, " and external factor of weight ", wn3, ".")

fileName3 <- paste0('herrstein smoothed RL log 2way ', delta, " ", lambda,
                   ' nat ', wn3, '.png')
sim3_smooth_nat <- runSimulation(100, 2000, figDims, dict, twoWay, delta, lambda, wp3, wn3)
print(plotResSim(sim3_smooth_nat, title3))
dev.copy(png, fileName3, width = 500, height = 350)
dev.off()