test1 <- testMeans(sim1_hrl$player2$raw, sim2_hrl$player2$raw)
sum(test1)
test1

test2 <- testMeans(sim1_hrl$player2$raw, sim1_smooth$player2$raw)
sum(test2)
test2

test3 <- testMeans(sim1_hrl$player2$raw, sim2_smooth$player2$raw)
sum(test3)
test3

test4 <- testMeans(sim1_hrl$player2$raw, sim3_smooth$player2$raw)
sum(test4)
test4

test5 <- testMeans(sim1_hrl$player2$raw, sim4_smooth$player2$raw)
sum(test5)
test5

test6 <- testMeans(sim2_smooth$player2$raw, sim3_smooth$player2$raw)
sum(test6)