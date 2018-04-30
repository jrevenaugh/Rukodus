# Block indices: take (row,column) produce vector index
bIndex <- array(0, dim = c(3,3,9))
for (i in 1:3) {
  for (j in 1:3) {
    bIndex[i,j,] <- c(1:3, 10:12, 19:21) + (i - 1) * 3 + (j - 1) * 27
  }
}

# Vector of grid filling numbers
N <- 1:9

# Attempted fills (equal to fill number if tried, 0 else)
Tried <- array(0, dim = c(9, 9, 9))

# Convert grid number to (row,column)
rowCol <- function(iGrid) {
  iGrid <- iGrid - 1
  return(c(iGrid %% 9 + 1, iGrid %/% 9 + 1))
}

solvePuzzle <- function(Puzzle) {
  openCells <- which(Puzzle == 0, arr.ind = TRUE)
  nOpen <- nrow(openCells)
  iGrid <- 1
  while (iGrid) {
    rc <- openCells[iGrid,]
    candidates <- N[!(N %in% neighbors(rc, Puzzle) | N %in% Tried[rc[1],rc[2],])]
    nC <- length(candidates)
    if (nC == 0) {
      iGrid <- iGrid - 1
      rc <- openCells[iGrid,]
      Puzzle[rc[1],rc[2]] <- 0
      iS <- min(iGrid + 1, nOpen)
      for (i in iS:nOpen) {
        nrc <- openCells[i,]
        Tried[nrc[1], nrc[2],] <- rep(0, 9)
      }
      next
    }
    if (nC == 1) j <- candidates
    if (nC > 1) j <- sample(candidates, 1)
    Puzzle[rc[1],rc[2]] <- j
    Tried[rc[1],rc[2],j] <- j
    iGrid <- iGrid + 1
    if (iGrid == (nOpen + 1)) break
  }
  if (iGrid == 0) return(FALSE)
  return(Puzzle)
}

neighbors <- function(rc, Puzzle) {
  nR <- rc[1]
  nC <- rc[2]
  bR <- (nR - 1) %/% 3 + 1
  bC <- (nC - 1) %/% 3 + 1
  neighbors <- c(Puzzle[nR,], Puzzle[,nC], Puzzle[bIndex[bR,bC,]])
  return(unique(neighbors))
}
