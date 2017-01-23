#####
## declaration of classes and functions
#####

Figure <- setRefClass(
  "Figure",
  fields = list(color = "character", size = "character", shape = "character")
)

# auxiliary function
isIn <- function(list, figure) {
  for(f in list) {
    if(f$color == figure$color && f$shape == figure$shape && f$size == figure$size) {
      return(TRUE)
    }
  }
  return(FALSE)
}

Agent <- setRefClass(
  "Agent",
  
  fields = list(
    accept = "list"
  ),
  
  methods = list(
    
    makeSplit = function(complexity, figures){
      accept <- list()
      if(complexity == 1) {
        dim <- sample(c("color", "shape", "size"), 1)[[1]]
        vals <- c()
        for(f in figures) {
          vals <- unique(c(vals, f[[dim]]))
        }
        val <- sample(vals, 1)[[1]]
        for(f in figures) {
          if(f[[dim]] == val) {
            accept <- c(accept, f)
          }
        }
      }
      return(accept)
    },
    
    givePoint = function(figure) {
      if(isIn(accept, figure)) {
        return(1)
      }
      return(-1)
    }
  )
)

setEnv <- function(figDims) {
  # input:
  # figDims - list - values for each dimension of the figure description (col, size, shape)
  
  # check configuration
  stopifnot(c("color", "size", "shape") %in% names(figDims))
  
  # initialize figures
  combs <- expand.grid(figDims$color, figDims$size, figDims$shape)
  colnames(combs) <- c("color", "size", "shape")
  figures <- c()
  
  for(i in 1:nrow(combs)) {
    set <- combs[i,]
    newFig <- Figure$new(color = as.character(set$color), 
                         size = as.character(set$size), 
                         shape = as.character(set$shape)
                         )
    figures <- c(figures, newFig)
  }
  
  return(figures)
}