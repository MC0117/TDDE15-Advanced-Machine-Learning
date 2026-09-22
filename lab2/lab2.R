library(HMM)


################################## QUESTION 1

states <- 1:10
symbols <- 1:10

transProbs <- matrix(0,10,10)
emissionProbs <- matrix(0,10,10)

for(i in 1:10) {
  transProbs[i,i] <- 0.5
  transProbs[i, (i%%10) + 1] <- 0.5
  for(k in -2:2){
    emissionProbs[i, ((i+k-1) %% 10) + 1] <- 0.2
  }
}
################################## QUESTION 2
hmm <- initHMM(States = states, Symbols = symbols, transProbs = transProbs, emissionProbs = emissionProbs, startProbs = rep(0.1, 10))
simulation <- simHMM(hmm, 100)

################################## QUESTION 3

observations <- simulation$observation

filtered <- rep(0, 100)
smoothed <- rep(0, 100)

logAlpha <- forward(hmm, observations)
logBeta <- backward(hmm, observations)

# filtered
filtered <- apply(logAlpha, 2, function(col){
  col <- exp(col - max(col))
  col/sum(col)
})

#smoothed
smoothed <- apply(logAlpha + logBeta, 2, function(col){
  col <- exp(col - max(col))
  col/sum(col)
})

#probable path
probablePath <- viterbi(hmm, observations)


#prints
print(filtered)
print(smoothed)
print(probablePath)

################################## QUESTION 4

# accuracy filtered
filteredStates <- apply(filtered, 2, which.max)
filteredAcc <- mean(filteredStates == simulation$states)
print(filteredAcc)

#accuracy smoothed
smoothedStates <- apply(smoothed, 2, which.max)
smoothedAcc <- mean(smoothedStates == simulation$states)
print(smoothedAcc)

#accuracy probable path
probablePathAcc <- mean(probablePath == simulation$states)
print(probablePathAcc)

################################## QUESTION 5
set.seed(222222)
simulation <- simHMM(hmm, 100)


filtered <- rep(0, 100)
smoothed <- rep(0, 100)

logAlpha <- forward(hmm, observations)
logBeta <- backward(hmm, observations)

# filtered
filtered <- apply(logAlpha, 2, function(col){
  col <- exp(col - max(col))
  col/sum(col)
})

#smoothed
smoothed <- apply(logAlpha + logBeta, 2, function(col){
  col <- exp(col - max(col))
  col/sum(col)
})

#probable path
probablePath <- viterbi(hmm, observations)


#prints
print(filtered)
print(smoothed)
print(probablePath)

# accuracy filtered
filteredStates <- apply(filtered, 2, which.max)
filteredAcc <- mean(filteredStates == simulation$states)
print(filteredAcc)

#accuracy smoothed
smoothedStates <- apply(smoothed, 2, which.max)
smoothedAcc <- mean(smoothedStates == simulation$states)
print(smoothedAcc)

#accuracy probable path
probablePathAcc <- mean(probablePath == simulation$states)
print(probablePathAcc)

### ANSWER: The smoothing is in general better that filtered since smoothing has
#access to the entire state sequence as opposed to filtered. most prob path
# will in general be outperformed by smoothing because smooting is maximizing
# P(zt | x0:T) separately for every t. Viterbi does it as a whole, viterbi needs
# to respect the internally consistency of the transiotion model and cannot jump
# around implausibly among the states.

################################## QUESTION 6

################################## QUESTION 7





 