num <- c("1", "2", "3")
shape <- c("Round", "Diamond", "Squiggle")
color <- c("Green", "Red", "Purple")
pattern <- c("Blank", "Striped", "Solid")

createDeck <- function(){
  deck <- list(array(c(num[1],shape[1], color[1], pattern[1])))
  count <- 1
  for (i in num) {
    for (j in shape) {
      for (k in color) {
        for (l in pattern) {
          
          deck[[count]] <- array(c(i, j, k, l))
          count <- count + 1
        }
      }
    }
  }
  
  return(deck)
}

d <- createDeck()

set.seed(1)

order <- sample(1:81)
for (i in order) {
  print(d[[i]])
}
sDeck <- d[order]
head(sDeck)


