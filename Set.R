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



#Shuffle deck
shuffleDeck <- function(d) {
  order <- sample(1:length(d))
  return(d[order])
}

#Check for set
checkSet <- function(l) {
  set <- FALSE
  for (i in 1:(length(l)-2)) {
    for (j in (i+1):(length(l)-1)) {
      for (k in (j+1):length(l)) {
        val <- TRUE
        c1 <- l[[i]]
        c2 <- l[[j]]
        c3 <- l[[k]]
        for (n in 1:4){
          if(c1[n] == c2[n] && c1[n] == c3[n]) {
            val <- val && TRUE
          } else if (c1[n] != c2[n] && c2[n] != c3[n] && c1[n] != c3[n] ) {
            val <- val && TRUE
          } else {
            val <- val && FALSE
          }
        }
        if(val) {
          set <- TRUE
          break
        }
      }
    }
  }
  return(set)  
}



#Initialize deck
d <- createDeck()

set.seed(1)
#Check if first layout is set for lots of decks
count <- 0
for (i in 1:100000) {
  sDeck <- shuffleDeck(d)
  if(checkSet(sDeck[1:16]) == TRUE) {
    count <- count + 1
  } 
}
count
# For 100,000 deals of 12 cards, get 96.81%
# For 100,000 deals of 16 cards, get 99.992%

