source('base.R')
#####
## run experiment
#####

figDims <- list(
  "color" = c("white", "red"),
  "size" = c("small", "big"),
  "shape" = c("square", "triangle")
)

dict <- c("A", "B")

res <- playGame(1000, figDims, dict, 0)
par(mfrow = c(1,2))
plot(res$player1)
plot(res$player2)
