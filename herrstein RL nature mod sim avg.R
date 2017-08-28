#####
## import
#####
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

twoWay <- TRUE

title1 <- paste0("Distribution of the mean for two-way Herrstein reinforcement learning 
                 with unlimited memory and external factor of weight ", wn1)

fileName1 <- paste0('herrstein RL 2way nature  ', wn1, ' mod sim.png')

sim1_nat_avg <- runSimulationMeans(100, 20, 100, figDims, dict, twoWay, wp1, wn1)
plotResSim(sim1_nat_avg, title1)
dev.copy(png, fileName1, width = 500, height = 350)
dev.off()

title2 <- paste0("Distribution of the mean for two-way Herrstein reinforcement learning 
                 with unlimited memory and external factor of weight ", wn2)

fileName2 <- paste0('herrstein RL 2way nature  ', wn2, ' mod sim.png')

sim2_nat_avg <- runSimulationMeans(100, 20, 100, figDims, dict, twoWay, wp2, wn2)
print(plotResSim(sim2_nat_avg, title2))
dev.copy(png, fileName2, width = 500, height = 350)
dev.off()

title3 <- paste0("Distribution of the mean for two-way Herrstein reinforcement learning 
                 with unlimited memory and external factor of weight ", wn3)

fileName3 <- paste0('herrstein RL 2way nature  ', wn3, ' mod sim.png')

sim3_nat_avg <- runSimulationMeans(100, 20, 100, figDims, dict, twoWay, wp3, wn3)
print(plotResSim(sim3_nat_avg, title3))
dev.copy(png, fileName3, width = 500, height = 350)
dev.off()