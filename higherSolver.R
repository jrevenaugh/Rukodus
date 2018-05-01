# higherSolver
#
# Sudoku solver using order search algorithms:
# 1. neighbors (row, column, block)
# 2. next...
# 3. next...

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
Avail <- array(FALSE, dim = c(9, 9, 9))

# Convert grid number to (row,column)
rowCol <- function(iGrid) {
  iGrid <- iGrid - 1
  return(c(iGrid %% 9 + 1, iGrid %/% 9 + 1))
}

neighbors <- function(nR, nC, Puzzle) {
  bR <- (nR - 1) %/% 3 + 1
  bC <- (nC - 1) %/% 3 + 1
  neighbors <- c(Puzzle[nR,], Puzzle[,nC], Puzzle[bIndex[bR,bC,]])
  return(unique(neighbors))
}

whatsAvailable <- function(Puzzle, Avail, openCells) {
  Avail[,,] <- FALSE
  nOpen <- nrow(openCells)
  for (i in 1:nOpen) {
    rc <- openCells[i,]
    iR <- rc[1]
    iC <- rc[2]
    Avail[iR,iC,] <- !(N %in% neighbors(iR, iC, Puzzle))
  }
  return(Avail)
}

soleNeighbor <- function(Puzzle, Avail, openCells) {
  nOpen <- nrow(openCells)
  nFilled <- 0
  for (i in 1:nOpen) {
    rc <- openCells[i,]
    iR <- rc[1]
    iC <- rc[2]
    Avail[iR,iC,] <- !(N %in% neighbors(iR, iC, Puzzle))
    onlyOne <- sum(Avail[iR,iC,])
    if (onlyOne == 1) {
      Puzzle[iR,iC] <- which(Avail[iR,iC,])
      nFilled <- nFilled + 1
    }
  }
  return(list(p = Puzzle, n = nFilled))
}

uniqueRow <- function(Puzzle, Avail) {
  nFilled <- 0
  for (iR in 1:9) {
    for (n in N) {
      nSum <- sum(Avail[iR,,n])
      if (nSum == 1) {
        iC <- which(Avail[iR,,n] == 1)
        Puzzle[iR,iC] <- n
        nFilled <- nFilled + 1
      }
    }
  }
  return(list(p = Puzzle, n = nFilled))
}

uniqueCol <- function(Puzzle, Avail) {
  nFilled <- 0
  for (iC in 1:9) {
    for (n in N) {
      nSum <- sum(Avail[,iC,n])
      if (nSum == 1) {
        iR <- which(Avail[,iC,n] == 1)
        Puzzle[iR,iC] <- n
        nFilled <- nFilled + 1
      }
    }
  }
  return(list(p = Puzzle, n = nFilled))
}

uniqueBox <- function(Puzzle, Avail) {
  nFilled <- 0
  for (n in 1:9) {
    B <- as.vector(Avail[,,n])
    for (bR in 1:3) {
      for (bC in 1:3) {
        nSum <- sum(B[bIndex[bR,bC,]])
        if (nSum == 1) {
          nB <- which(B[bIndex[bR,bC,]] == TRUE)
          nC <- (nB - 1) %/% 3 + 1
          nR <- nB - (nC - 1) * 3 + (bR - 1) * 3
          nC <- nC + (bC - 1) * 3
          Puzzle[nR,nC] <- n
          nFilled <- nFilled + 1
        }
      }
    }
  }
  return(list(p = Puzzle, n = nFilled))
}

higherSolver <- function(Puzzle) {
  while (1) {
    nFilledAll <- 0

    # Simple sole candidate elimination
    openCells <- which(Puzzle == 0, arr.ind = TRUE)
    SN <- soleNeighbor(Puzzle, Avail, openCells)
    Puzzle <- SN$p
    nFilled <- SN$n

    # Unique in row elimination
    if (nFilled > 0) {
      openCells <- which(Puzzle == 0, arr.ind = TRUE)
      if (nrow(openCells) == 0) return(Puzzle)
      Avail <- whatsAvailable(Puzzle, Avail, openCells)
    }
    UR <- uniqueRow(Puzzle, Avail)
    Puzzle <- UR$p
    nFilled <- UR$n

    # Unique in column elimination
    if (nFilled > 0) {
      openCells <- which(Puzzle == 0, arr.ind = TRUE)
      if (nrow(openCells) == 0) return(Puzzle)
      Avail <- whatsAvailable(Puzzle, Avail, openCells)
    }
    UC <- uniqueCol(Puzzle, Avail)
    Puzzle <- UC$p
    nFilled <- UC$n

    # Unique in block elimination
    if (nFilled > 0) {
      openCells <- which(Puzzle == 0, arr.ind = TRUE)
      if (nrow(openCells) == 0) return(Puzzle)
      Avail <- whatsAvailable(Puzzle, Avail, openCells)
    }
    UB <- uniqueBox(Puzzle, Avail)
    Puzzle <- UB$p
    nFilled <- UB$n

    if (SN$n + UR$n + UC$n + UB$n == 0) {
      print("Failed")
      return(Puzzle)
    }
    openCells <- which(Puzzle == 0, arr.ind = TRUE)
    nOpen <- nrow(openCells)
    if (nOpen == 0) return(Puzzle)
  }
}
