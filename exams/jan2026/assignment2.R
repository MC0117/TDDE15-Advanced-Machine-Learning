# Shared Q-learning machinery used by environments A, B and C.
# State = (x, y) with x = row (1..H, plotted upwards), y = column (1..W).
# Q-table layout: q_table[x, y, action], an H x W x 4 array.
# Action indices: 1 = up, 2 = right, 3 = down, 4 = left (clockwise order,
# so action +/- 1 (mod 4) is a perpendicular action).
set.seed(12345)
library(ggplot2)
# Arrow symbols and (dx, dy) moves, indexed by action number.
arrows <- c("ˆ", ">", "v", "<")
action_deltas <- list(c(1,0), # up
                      c(0,1), # right
                      c(-1,0), # down
                      c(0,-1)) # left
# Plot the grid (given code): the four Q-values per non-terminal cell

# (top/right/bottom/left = up/right/down/left), the greedy action as an
# arrow, and fill colour = max_a Q (red NA cells = negative rewards).
# Uses globals H, W, reward_map and q_table.
vis_environment_double <- function(iterations=0, epsilon = 0.5, alpha = 0.1, gamma = 0.95, beta = 0){
  # expand.grid: data frame with one row per (x, y) cell (x varies fastest).
  df <- expand.grid(x=1:H,y=1:W)
  # mapply applies f(x, y) over the paired df$x, df$y columns; NA = terminal.
  foo <- mapply(function(x,y) ifelse(reward_map[x,y] == 0,q_table_1[x,y,1]+q_table_2[x,y,1],NA),df$x,df$y)
  df$val1 <- as.vector(round(foo, 2))
  foo <- mapply(function(x,y) ifelse(reward_map[x,y] == 0,q_table_1[x,y,2]+q_table_2[x,y,2],NA),df$x,df$y)
  df$val2 <- as.vector(round(foo, 2))
  foo <- mapply(function(x,y) ifelse(reward_map[x,y] == 0,q_table_1[x,y,3]+q_table_2[x,y,3],NA),df$x,df$y)
  df$val3 <- as.vector(round(foo, 2))
  foo <- mapply(function(x,y) ifelse(reward_map[x,y] == 0,q_table_1[x,y,4]+q_table_2[x,y,4],NA),df$x,df$y)
  df$val4 <- as.vector(round(foo, 2))
  foo <- mapply(function(x,y)
    ifelse(reward_map[x,y] == 0,arrows[GreedyPolicyDoubleQ(x,y)],reward_map[x,y]),df$x,df$y)
  df$val5 <- as.vector(foo)
  foo <- mapply(function(x,y) ifelse(reward_map[x,y] == 0,max(q_table_1[x,y,]+q_table_2[x,y,]),
                                     ifelse(reward_map[x,y]<0,NA,reward_map[x,y])),df$ x,df$y)
  df$val6 <- as.vector(foo)

  # print() needed: ggplot objects are not auto-drawn inside a function/loop.
  # aes maps column y to the horizontal and row x to the vertical axis.
  print(ggplot(df,aes(x = y,y = x)) +
          # Continuous fill white -> green; NA cells drawn red; no legend title.
          scale_fill_gradient(low = "white", high = "green", na.value = "red", name = "")
        +
          # One filled square per cell, coloured by val6 (max Q / reward).
          geom_tile(aes(fill=val6)) +
          # nudge_x/y shift labels towards a cell edge; na.rm drops NA labels.
          geom_text(aes(label = val1),size = 3,nudge_y = .35,na.rm = TRUE) +
          geom_text(aes(label = val2),size = 3,nudge_x = .35,na.rm = TRUE) +
          geom_text(aes(label = val3),size = 3,nudge_y = -.35,na.rm = TRUE) +
          geom_text(aes(label = val4),size = 3,nudge_x = -.35,na.rm = TRUE) +
          geom_text(aes(label = val5),size = 8) +
          # Transparent tiles with black borders draw the grid lines.
          geom_tile(fill = 'transparent', colour = 'black') +
          ggtitle(paste("Double-Q-table after ",iterations," iterations\n",
                        "(epsilon = ",epsilon,", alpha = ",alpha,"gamma = ",gamma,", beta = ",beta,")")) +
          # hjust = 0.5 centres the title.
          theme(plot.title = element_text(hjust = 0.5)) +
          # Integer tick marks at every column / row.
          scale_x_continuous(breaks = c(1:W),labels = c(1:W)) +
          scale_y_continuous(breaks = c(1:H),labels = c(1:H)))
}

vis_environment <- function(iterations=0, epsilon = 0.5, alpha = 0.1, gamma = 0.95, beta = 0){
  # expand.grid: data frame with one row per (x, y) cell (x varies fastest).
  df <- expand.grid(x=1:H,y=1:W)
  # mapply applies f(x, y) over the paired df$x, df$y columns; NA = terminal.
  foo <- mapply(function(x,y) ifelse(reward_map[x,y] == 0,q_table[x,y,1],NA),df$x,df$y)
  df$val1 <- as.vector(round(foo, 2))
  foo <- mapply(function(x,y) ifelse(reward_map[x,y] == 0,q_table[x,y,2],NA),df$x,df$y)
  df$val2 <- as.vector(round(foo, 2))
  foo <- mapply(function(x,y) ifelse(reward_map[x,y] == 0,q_table[x,y,3],NA),df$x,df$y)
  df$val3 <- as.vector(round(foo, 2))
  foo <- mapply(function(x,y) ifelse(reward_map[x,y] == 0,q_table[x,y,4],NA),df$x,df$y)
  df$val4 <- as.vector(round(foo, 2))
  foo <- mapply(function(x,y)
    ifelse(reward_map[x,y] == 0,arrows[GreedyPolicy(x,y)],reward_map[x,y]),df$x,df$y)
  df$val5 <- as.vector(foo)
  foo <- mapply(function(x,y) ifelse(reward_map[x,y] == 0,max(q_table[x,y,]),
                                     ifelse(reward_map[x,y]<0,NA,reward_map[x,y])),df$ x,df$y)
  df$val6 <- as.vector(foo)
  
  # print() needed: ggplot objects are not auto-drawn inside a function/loop.
  # aes maps column y to the horizontal and row x to the vertical axis.
  print(ggplot(df,aes(x = y,y = x)) +
          # Continuous fill white -> green; NA cells drawn red; no legend title.
          scale_fill_gradient(low = "white", high = "green", na.value = "red", name = "")
        +
          # One filled square per cell, coloured by val6 (max Q / reward).
          geom_tile(aes(fill=val6)) +
          # nudge_x/y shift labels towards a cell edge; na.rm drops NA labels.
          geom_text(aes(label = val1),size = 3,nudge_y = .35,na.rm = TRUE) +
          geom_text(aes(label = val2),size = 3,nudge_x = .35,na.rm = TRUE) +
          geom_text(aes(label = val3),size = 3,nudge_y = -.35,na.rm = TRUE) +
          geom_text(aes(label = val4),size = 3,nudge_x = -.35,na.rm = TRUE) +
          geom_text(aes(label = val5),size = 8) +
          # Transparent tiles with black borders draw the grid lines.
          geom_tile(fill = 'transparent', colour = 'black') +
          ggtitle(paste("Q-table after ",iterations," iterations\n",
                        "(epsilon = ",epsilon,", alpha = ",alpha,"gamma = ",gamma,", beta = ",beta,")")) +
          # hjust = 0.5 centres the title.
          theme(plot.title = element_text(hjust = 0.5)) +
          # Integer tick marks at every column / row.
          scale_x_continuous(breaks = c(1:W),labels = c(1:W)) +
          scale_y_continuous(breaks = c(1:H),labels = c(1:H)))
}

  
  # Greedy policy: return argmax_a Q(x, y, a).
GreedyPolicy <- function(x, y){
  # Break ties at random among the actions with maximal Q-value.
  # (which.max would always pick the first, e.g. always "up" while all
  # Q-values are still 0, biasing the agent.)
  q <- q_table[x,y,]
  best <- which(q == max(q))
  return(best[sample(length(best), 1)])
}

GreedyPolicyDoubleQ <- function(x, y){
  # Break ties at random among the actions with maximal Q-value.
  # (which.max would always pick the first, e.g. always "up" while all
  # Q-values are still 0, biasing the agent.)
  q <- q_table_1[x,y,] + q_table_2[x,y,]
  best <- which(q == max(q))
  return(best[sample(length(best), 1)])
}

# Epsilon-greedy policy: with probability epsilon explore (uniformly random
# action, which may also be the greedy one), otherwise exploit (greedy).
EpsilonGreedyPolicy <- function(x, y, epsilon){
  if(runif(1) < epsilon){
    return(sample(1:4,1))
  } else{
    return(GreedyPolicy(x,y))
  }
}

EpsilonGreedyPolicyDoubleQ <- function(x, y, epsilon){
  if(runif(1) < epsilon){
    return(sample(1:4,1))
  } else{
    return(GreedyPolicyDoubleQ(x,y))
  }
}

# Environment dynamics. beta = slip probability: with prob 1 - beta the
# intended action is executed, with prob beta/2 each the agent slips to
# one of the two perpendicular actions. Returns the new state (x, y).
transition_model <- function(x, y, action, beta){
  # delta = -1 (slip counter-clockwise), 0 (no slip), +1 (clockwise).
  delta <- sample(-1:1, size = 1, prob = c(0.5*beta,1-beta,0.5*beta))
  # Rotate the action by delta, wrapping around within 1..4.
  final_action <- ((action + delta + 3) %% 4) + 1
  # [ ] on a list returns a sub-list; unlist turns it into the c(dx, dy).
  foo <- c(x,y) + unlist(action_deltas[final_action])
  # Clamp to the grid: moving into a wall leaves the agent in place.
  foo <- pmax(c(1,1),pmin(foo,c(H,W)))
  return (foo)
}

# Run one episode of Q-learning from start_state, updating the global
# q_table in place (<<-). Behaviour policy: epsilon-greedy; target: greedy
# (max over next actions), i.e. off-policy. alpha = learning rate,
# gamma = discount, beta = slip probability.
# Returns c(reward of the episode, sum of TD errors).
q_learning <- function(start_state, epsilon = 0.5, alpha = 0.1, gamma = 0.95,
                       beta = 0){
  current_state <- start_state
  # Sum of TD errors over the episode: how much the Q-table was corrected.
  # Tends to 0 as the Q-values converge.
  episode_correction <- 0
  repeat{
    # Follow policy, execute action, get reward.
    next_action <- EpsilonGreedyPolicy(current_state[1], current_state[2], epsilon)
    next_state <- transition_model(current_state[1], current_state[2], next_action, beta)
    q_old <- q_table[current_state[1], current_state[2], next_action]
    reward <- reward_map[next_state[1], next_state[2]]
    # Q-table update (TD update):
    # Q(s,a) <- Q(s,a) + alpha * (r + gamma * max_a' Q(s',a') - Q(s,a)).
    # Terminal states have Q = 0 (never updated), so target = r there.
    target <- reward + gamma * max(q_table[next_state[1], next_state[2],])
    q_table[current_state[1], current_state[2], next_action] <<- q_old + alpha * (target-q_old)
    episode_correction <- episode_correction + (target - q_old)
    current_state <- next_state

  # Any non-zero reward (positive goal or negative state) is terminal.
  if(reward!=0)
    # End episode.
    return (c(reward,episode_correction))
  }
}

double_q_learning <- function(start_state, epsilon = 0.5, alpha = 0.1, gamma = 0.95,
                       beta = 0){
  current_state <- start_state
  # Sum of TD errors over the episode: how much the Q-table was corrected.
  # Tends to 0 as the Q-values converge.
  episode_correction <- 0
  repeat{
    # Follow policy, execute action, get reward.
    next_action <- EpsilonGreedyPolicyDoubleQ(current_state[1], current_state[2], epsilon)
    next_state <- transition_model(current_state[1], current_state[2], next_action, beta)
    q_old <- 0
    use_q1 <- runif(1) < 0.5
    if(use_q1){
      q_old <- q_table_1[current_state[1], current_state[2], next_action]
    }
    else{
      q_old <- q_table_2[current_state[1], current_state[2], next_action]
    }
    reward <- reward_map[next_state[1], next_state[2]]
    # Q-table update (TD update):
    # Q(s,a) <- Q(s,a) + alpha * (r + gamma * max_a' Q(s',a') - Q(s,a)).
    # Terminal states have Q = 0 (never updated), so target = r there.
    if(use_q1){
      target <- reward + gamma * q_table_2[next_state[1], next_state[2], which.max(q_table_1[next_state[1], next_state[2],])]
      q_table_1[current_state[1], current_state[2], next_action] <<- q_old + alpha * (target - q_old)
      episode_correction <- episode_correction + (target - q_old)
    }
    else{
      target <- reward + gamma * q_table_1[next_state[1], next_state[2], which.max(q_table_2[next_state[1], next_state[2],])]
      q_table_2[current_state[1], current_state[2], next_action] <<- q_old + alpha * (target - q_old)
      episode_correction <- episode_correction + (target - q_old)
    }
    
    current_state <- next_state
    
    # Any non-zero reward (positive goal or negative state) is terminal.
    if(reward!=0)
      # End episode.
      return (c(reward,episode_correction))
  }
}

# Moving average over windows of n episodes (via cumulative sums), used to
# smooth the reward and correction curves.
MovingAverage <- function(x, n){
  cx <- c(0,cumsum(x))
  # Window sum = S[i+n] - S[i]; returns length(x) - n + 1 averages.
  rsum <- (cx[(n+1):length(cx)] - cx[1:(length(cx) - n)]) / n
  return (rsum)
}
# Environment A: 5 x 7 grid, goal +10 at (3,6), wall of -1 at (2:4,3).
# Default parameters (epsilon 0.5, alpha 0.1, gamma 0.95, beta 0).
H <- 5
W <- 7
reward_map <- matrix(0, nrow = H, ncol = W)
reward_map[3,6] <- 10
reward_map[2:4,3] <- -1
# Q-table starts at 0 for all states and actions.
#REPLACE
q_table <- array(0,dim = c(H,W,4))
q_table_1 <- array(0,dim = c(H,W,4))
q_table_2 <- array(0,dim = c(H,W,4))

reward_double <- numeric(10000)
correction_double <- numeric(10000) 
reward <- numeric(10000)
correction <- numeric(10000) 

# 10000 episodes from (3,1); plot the Q-table at 10, 100, 1000, 10000.
for(i in 1:10000){
  foo <- double_q_learning(start_state = c(3,1))
  reward_double[i] <- foo[1]
  correction_double[i] <- foo[2]
  
  foo <- q_learning(start_state = c(3,1))
  reward[i] <- foo[1]
  correction[i] <- foo[2]
  
  if(any(i==c(10,100,1000,10000))){
    vis_environment_double(i)
    readline(prompt="press enter to continue")
    vis_environment(i)
    readline(prompt="press enter to continue")
  }
}
setting <- paste("epsilon ", 0.5, ", gamma ", 0.95)
plot(MovingAverage(reward,100), type = "l", main = paste("Reward Comparison,", setting),
     xlab = "Episode", ylab = "Moving Avg reward (last 100)", cex.main = 0.8, col="blue", ylim = c(2,max(reward_double)))
lines(MovingAverage(reward_double,100), type = "l", col="red")
legend("bottomleft", legend = c("QL", "DQL"),
       col = c("blue", "red"), lwd = 2)

plot(MovingAverage(correction,100), type = "l", main = paste("Correction,", setting),
     xlab = "Episode", ylab = "Avg correction (last 100) Q-Learning", cex.main = 0.8)
plot(MovingAverage(correction_double,100), type = "l", main = paste("Correction,", setting),
     xlab = "Episode", ylab = "Avg correction (last 100) 2xQ-Learning", cex.main = 0.8)

# Environment B: 7 x 8 grid, -1 in top and bottom rows, +5 at (4,5)
# (close) and +10 at (4,8) (far, behind the 5). Start in (4,1).
H <- 7
W <- 8
reward_map <- matrix(0, nrow = H, ncol = W)
reward_map[1,] <- -1
reward_map[7,] <- -1
reward_map[4,5] <- 5
reward_map[4,8] <- 10
# Grid search: epsilon (exploration) x gamma (discount); fresh Q-table
# and 30000 episodes per setting.
for(eps in c(0.5, 0.1)){
  for(j in c(0.5,0.75,0.95)){
    #q_table <- array(0,dim = c(H,W,4))
    q_table_1 <- array(0,dim = c(H,W,4))
    q_table_2 <- array(0,dim = c(H,W,4))
    # Per-episode reward and correction (sum of TD errors).
    reward <- numeric(30000)
    correction <- numeric(30000)
    for(i in 1:30000){
      #Replace
      #foo <- q_learning(epsilon = eps, gamma = j, start_state = c(4,1))
      foo <- double_q_learning(epsilon = eps, gamma = j, start_state = c(4,1))
      reward[i] <- foo[1]
      correction[i] <- foo[2]
    }
    setting <- paste("epsilon =", eps, ", gamma =", j)
    vis_environment(i, epsilon = eps, gamma = j)
    # Moving averages (window 100): reward shows which goal is reached,
    # correction shows when the Q-table stops changing.
    # par(mfrow): 1 x 2 panel layout for the two base plots (reset below).
    par(mfrow = c(1,2))
    # type = "l": line plot; cex.main shrinks the title.
    plot(MovingAverage(reward,100), type = "l", main = paste("Reward,", setting),
         xlab = "Episode", ylab = "Avg reward (last 100)", cex.main = 0.8)
    plot(MovingAverage(correction,100), type = "l", main = paste("Correction,", setting),
         xlab = "Episode", ylab = "Avg correction (last 100)", cex.main = 0.8)
    par(mfrow = c(1,1))
  }
}
# Environment C: 3 x 6 grid, -1 at (1,2:5) (a "cliff"), goal +10 at
# (1,6). Start in (1,1), epsilon 0.5, gamma 0.6.
H <- 3
W <- 6
reward_map <- matrix(0, nrow = H, ncol = W)
reward_map[1,2:5] <- -1
reward_map[1,6] <- 10
# Vary beta (slip probability); fresh Q-table, 10000 episodes each.
for(j in c(0,0.2,0.4,0.66)){
  q_table <- array(0,dim = c(H,W,4))
  for(i in 1:10000)
    foo <- q_learning(gamma = 0.6, beta = j, start_state = c(1,1))
  vis_environment(i, gamma = 0.6, beta = j)
}