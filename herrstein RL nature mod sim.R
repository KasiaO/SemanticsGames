#####
## import
#####
# source main methods and functions
source('base.R')
source('herrstein RL.R')
source('herrstein RL nature mod.R')

######
## run simulation
######

wp1 <- 0.1
wp2 <- 0.5
wp3 <- 0.9

wn1 <- 1 - wp1
wn2 <- 1 - wp2
wn3 <- 1 - wp3

title1 <- paste0("Average learning curve for two-way Herrstein reinforcement learning 
                 with unlimited memory and external factor of weight ", wn1)

fileName1 <- paste0('herrstein RL 2way nature  ', wn1, ' mod.png')

sim1_nat <- runSimulation(100, 2000, figDims, dict, twoWay, wp1, wn1)
plotResSim(sim1_nat, title1)
dev.copy(png, fileName1, width = 500, height = 350)
dev.off()

title2 <- paste0("Average learning curve for two-way Herrstein reinforcement learning 
                 with unlimited memory and external factor of weight ", wn2)

fileName2 <- paste0('herrstein RL 2way nature  ', wn2, ' mod.png')

sim2_nat <- runSimulation(100, 2000, figDims, dict, twoWay, wp2, wn2)
print(plotResSim(sim2_nat, title2))
dev.copy(png, fileName2, width = 500, height = 350)
dev.off()

title3 <- paste0("Average learning curve for two-way Herrstein reinforcement learning 
                 with unlimited memory and external factor of weight ", wn3)

fileName3 <- paste0('herrstein RL 2way nature  ', wn3, ' mod.png')

sim3_nat <- runSimulation(100, 2000, figDims, dict, twoWay, wp3, wn3)
print(plotResSim(sim3_nat, title3))
dev.copy(png, fileName3, width = 500, height = 350)
dev.off()