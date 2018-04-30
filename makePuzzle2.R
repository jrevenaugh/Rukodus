# makePuzzle
#
# Begin with filled puzzle (buildPuzzle), remove mirror pairs of entries if
# symmetry is TRUE, otherwise remove singletons.  Because removed cells are
# required to have unique values (only one value can fill the cell), there is
# no reason to check puzzle uniqueness after removal.
#
# Continue removing pairs until puzzle becomes non-unique or minimum filled
# cell count is reached.  If the former, program backtracks and
# continues.  If maxIter is reached on a puzzle and it dead-ends, a new puzzle
# is generated and the search continues anew.
#
# This is a more complex algorithm than makePuzzle.  My guess, and that's all it
# is, is that this will be faster for difficult puzzles and slower for all
# others.  Definitely slower at 27 filled cells.

source("buildPuzzle.R")
source("hasSolution.R")
source("isUnique.R")

# Block indices: take (row,column) produce vector index
bIndex <- array(0, dim = c(3,3,9))
for (i in 1:3) {
  for (j in 1:3) {
    bIndex[i,j,] <- c(1:3, 10:12, 19:21) + (i - 1) * 3 + (j - 1) * 27
  }
}

makePuzzle2 <- function(minFilled = 41, maxIter = 1000, symmetric = TRUE, verbose = FALSE) {
  # Indices for the lower triangle and diagonal of puzzle.  Third column is
  # 1 for cell remains filled, 0 for empty.  If symmetric = FALSE
  # Lower is all cells and diagonal is always ignored.

  # Get a starting puzzle
  nMax <- ifelse(symmetric, 45, 81)
  Puzzle <- buildPuzzle()
  sPuzzle <- array(Puzzle, dim = c(9, 9, nMax))
  cellCount <- sum(Puzzle != 0)
  iStep <- 1
  nIter <- 1

  # Set up cell indexing
  Lower <- array(0, dim = c(nMax, 3, nMax))
  diagonal <- rep(1, 9)
  onDiag <- rep(FALSE, nMax)
  nFac <- ifelse(symmetric, 2, 1)
  yesNo <- rep(TRUE, 81)
  nDiag <- 0
  k <- 1
  for (i in 1:9) {
    jLimit <- ifelse(symmetric, i, 9)
    for (j in 1:jLimit) {
      Lower[k,,1:nMax] <- c(i, j, 1)
      if (i == j) {
        diagonal[i] <- k
        onDiag[k] <- TRUE
      }
      k <- k + 1
    }
  }

  while (cellCount > minFilled) {
    while (1) {
      nIter <- nIter + 1
      availCell <- which(Lower[,3,iStep] == 1)
      n <- length(availCell)
      for (j in 1:n) {
        rc <- Lower[availCell[j],1:2,iStep]
        yesNo[j] <- onlyOne(rc, Puzzle, symmetric)
      }
      availCell <- availCell[yesNo[1:n]]
      nAvail <- length(availCell)
      if (symmetric) nDiag <- length(onDiag[availCell])
      nRemaining <- (nFac * nAvail - nDiag)

      # Dead-end? (No cells left to remove or too few to reach target)
      # Back solution up one removal step (leaving current removed cell(s) as taboo)
      if (nAvail == 0 | (cellCount - minFilled - nRemaining > 0)) {
        iStep <- iStep - 1
        Puzzle <- sPuzzle[,,iStep]
        cellCount <- sum(Puzzle != 0)
        if (iStep) Lower[,3,iStep:nMax] <- Lower[,3,iStep]

        # Out of iterations for this puzzle.  Reboot the process
        if (iStep == 0 | nIter > maxIter) {
          if (verbose) print(paste("Reset at", cellCount,
                                   "with", nRemaining,
                                   "possible cells remaining."))
          Puzzle <- buildPuzzle()
          cellCount <- sum(Puzzle != 0)
          Lower[,3,1:nMax] <- 1
          iStep <- 1
          sPuzzle[,,1:nMax] <- Puzzle
          nIter <- 1
        }
        next
      }

      # Remove a cell (or pair if symmetric)
      else if (nAvail == 1) nCell <- availCell
      else nCell <- sample(availCell, 1)
      nRow <- Lower[nCell,1,iStep:nMax]
      nCol <- Lower[nCell,2,iStep:nMax]
      tPuzzle <- Puzzle
      tPuzzle[nRow,nCol] <- 0
      if (symmetric) tPuzzle[nCol,nRow] <- 0
      Lower[nCell,3,iStep:nMax] <- 0
      Puzzle <- tPuzzle
      break
    }
    # Removed cell(s) produced unique puzzle, iterate removal process
    cellCount <- sum(Puzzle != 0)
    iStep <- iStep + 1
    sPuzzle[,,iStep:nMax] <- Puzzle
  }
  return(Puzzle)
}

# Return list of (unique) sudoku neighbors for cell at rc (row,column)
neighbors <- function(rc, Puzzle) {
  nR <- rc[1]
  nC <- rc[2]
  bR <- (nR - 1) %/% 3 + 1
  bC <- (nC - 1) %/% 3 + 1
  neighbors <- c(Puzzle[nR,], Puzzle[,nC], Puzzle[bIndex[bR,bC,]])
  return(unique(neighbors))
}

onlyOne <- function(rc, Puzzle, symmetric) {
  if (symmetric) {
    takenLL <- neighbors(rc, Puzzle)
    if (sum(takenLL) == 45) {
      takenUR <- neighbors(rev(rc), Puzzle)
      if (sum(takenUR) == 45) return(TRUE)
    }
  }
  else {
    taken <- neighbors(rc, Puzzle)
    if (sum(taken) == 45) return(TRUE)
  }
  return(FALSE)
}
