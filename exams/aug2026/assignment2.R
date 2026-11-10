

# --- Build HMM matrices: robot on a ring of 10 sectors ---
# Hidden states = true sector, symbols = sensor reading (both 1..10)

set.seed(12345)

states <- c("Background", "CG-rich")
symbols <- c("A","C","G","T")
# transProbs[from, to]: rows = current state z_t, cols = next state z_t+1
# (each row sums to 1)
# emissionProbs[state, symbol]: rows = hidden state, cols = observed symbol
# (each row sums to 1)

initProbs <- c(0.6,0.4)

transProbs <- matrix(0,2,2) 
transProbs[1,] <-c(0.85,0.15)
transProbs[2,] <-c(0.25,0.75)

emissionProbs <- matrix(0,2,4)
emissionProbs[1,] <-c(0.30,0.20,0.20,0.30)
emissionProbs[2,] <-c(0.10,0.40,0.40,0.10)

observations <- c("A", "C", "G", "C", "G", "T", "C", "G", "A", "C", "G")

# --- Create HMM object and simulate ---
# initHMM(States, Symbols, startProbs, transProbs, emissionProbs);
# startProbs = uniform over the 10 sectors
hmm <- initHMM(States = states, Symbols = symbols, transProbs = transProbs,
               emissionProbs = emissionProbs, startProbs = initProbs)
1# simHMM returns a list: $states (true hidden path), $observation (readings)
#simulation <- simHMM(hmm, 100)

logAlpha <- forward(hmm, observations)

alpha <- exp(logAlpha)

filtered <- apply(logAlpha, 2, function(col){
  col <- exp(col - max(col))
  col/sum(col)
})

# filtered is now alpha i.e the probability of the observation sequence
# condtioned on the hidden state S
# I order to compute the probability of the observations P(O^1:11) we need
# to marginalize on the hidden state

transProb11 <- exp(11*log(transProbs))
stateProb11 <- t(as.matrix(initProbs))%*%transProb11

simulateTransitions <- function(transMat, steps, initDist){
  product <- transMat
  for (i in 2:steps) {
    product <- product%*%transMat
  }
  return(as.vector(initDist%*%product))
}


stateProb11 <- simulateTransitions(transProbs, 11, t(as.matrix(initProbs)))

prob_observations_seq <- 0 #initialize

for (i in 1:length(states)){
  state <- states[i]
  prob_observations_seq <- prob_observations_seq + exp(logAlpha[state, 11]-log(stateProb11[i]))
}

prob_observations_seq

# very small probability however, its a very low probability given 11 steps
# the number of possible emission sequences of the an highly exponential order 
# example number of possible state sequences is 2^11 = 2048. all these sequences
# has their own space of possible observation sequences which is 4^11
# since all emission probs are greater than 0. 
# note that different state sequences may have the same obsevation seq, but
# it gives an idea over the magnitude of the possible outcomes.

# This questions begs the follow up question of what is meant by effect?
# For a very long state sequence and if the transition matrix has an asymptotic distribution
# and i.e the chain closed, aperiodic and has irreducible (which this case satisfies) 
# then it will converge to its stationary distribution. However this stationary distribution
# may not be the same as the initial distribution. Given that it is not then i would
# say that the initial probability distribution will decay as the t increases.
# the observation sequence, that is merely an effect of the state sequence
# will also experience this effect. 
# A second argument for this conclusion is also that both states can result 
# in all types of emissions with a probability in a rather even spectrum (0.10-0.40)

# However: different initial distributions will of course result in different results naturally
