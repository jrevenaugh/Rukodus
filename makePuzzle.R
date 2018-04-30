# makePuzzle
#
# Begin with filled puzzle (sudokuBuild), remove mirror pairs of entries if
# symmetry is TRUE, otherwise remove singletons.
# After each removal, check that puzzle retains a unique solution (inUnique).
# Continue removing pairs until puzzle becomes non-unique or minimum filled
# cell count is reached.  If the former, program resets puzzle and starts
# anew.

source("buildPuzzle.R")
source("hasSolution.R")
source("isUnique.R")

makePuzzle <- function(minFilled = 41, symmetric = TRUE, verbose = FALSE) {
  # Indices for the lower triangle and diagonal of puzzle.  Third column is
  # 1 for cell remains filled, 0 for empty.  If symmetric = FALSE
  # Lower is all cells and diagonal is always ignored.
  n <- ifelse(symmetric, 45, 81)
  Lower <- matrix(0, nrow = n, ncol = 3)
  diagonal <- rep(1, 9)
  nFac <- ifelse(symmetric, 2, 1)
  k <- 1
  for (i in 1:9) {
    jLimit <- ifelse(symmetric, i, 9)
    for (j in 1:jLimit) {
      Lower[k,] <- c(i, j, 1)
      if (i == j) diagonal[i] <- k
      k <- k + 1
    }
  }
  Puzzle <- buildPuzzle()
  cellCount <- sum(Puzzle != 0)
  while (cellCount > minFilled) {
    while (1) {
      availCell <- which(Lower[,3] == 1)
      nAvail <- length(availCell)
      nRemaining <- (nFac * nAvail + symmetric * sum(Lower[diagonal,3]))
      if (nAvail == 0 | (cellCount - minFilled - nRemaining > 0)) {
        if (verbose) print(paste("Reset at", cellCount,
                                 "with", nRemaining,
                                 "possible cells remaining."))
        Puzzle <- buildPuzzle()
        cellCount <- sum(Puzzle != 0)
        Lower[,3] <- 1
        next
      }
      else if (nAvail == 1) nCell <- availCell
      else nCell <- sample(availCell, 1)
      nRow <- Lower[nCell,1]
      nCol <- Lower[nCell,2]
      tPuzzle <- Puzzle
      tPuzzle[nRow,nCol] <- 0
      if (symmetric) tPuzzle[nCol,nRow] <- 0
      Lower[nCell,3] <- 0
      if (!isUnique(tPuzzle)) next
      Puzzle <- tPuzzle
      break
    }
    cellCount <- sum(Puzzle != 0)
  }
  return(Puzzle)
}
