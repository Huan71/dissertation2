# dissertation
#The software used is Rstudio
#This is the section on model comparisons in section 3
library(sand)
library(igraph)
library(deSolve)

#The functions constructed below are for the 
#stochastic SIS model, the SIS node-level model and the SIS ODE model respectively

#stochastic SIS model
ex_SIS_model1 <- function(X, A, beta, gamma, day) {
  
  status_matrix <- matrix(0, nrow = day, ncol = length(X))
  colnames(status_matrix) <- 1:length(X)
  status_matrix[1, ] <- X
  
  num_infected <- sum(X == 1)
  num_susceptible <- sum(X == 0)
  
  for (t in 2:day) {
    
    prob_i <- beta * (A %*% X)
    prob_i <- pmin(1, pmax(0, prob_i))
    
    prob_s <- gamma * X  
    prob_s <- pmin(1, pmax(0, prob_s))
    
    X_new1 <- rbinom(length(X), 1, prob_i)
    X_new2 <- rbinom(length(X), 1, prob_s)
    
    for (i in 1:length(X)) {
      if (X[i] == 1 && X_new2[i] == 1) {
        X[i] <- 0
      }
      if (X[i] == 0 && X_new1[i] == 1) {
        X[i] <- 1
      }
    }
  
    status_matrix[t, ] <- X
    num_infected[t] <- sum(X == 1)
    num_susceptible[t] <- sum(X == 0)
  }
  return(list(num_infected = num_infected, num_susceptible = num_susceptible))
}

#SIS node-level model
SIS_net_ODEMODEL <- function(time, state, parameters, A) {
  
  I <- state
  beta <- parameters[1]
  gamma <- parameters[2]
  N <- length(I)
  dI <- numeric(N)
  new_I <- numeric(N)
  l <- I
  for (i in 1:N){
    
    dI[i] <- beta *(1 - I[i])* sum(A[i,] *I) - gamma * I[i]
    l[i] <- I[i]
    new_I[i] = I[i] + dI[i]
    I[i]<-new_I[i]
    
  }
  return(list(c(new_I - l)))
  
}

#SIS ODE model
SIS_ODEMODEL <- function(time, state, parameters,connect_probability) {
  S <- state[1]
  I <- state[2]
  
  
  beta <- parameters$beta
  gamma <- parameters$gamma
  
  dS <- gamma * I - beta * S * I*connect_probability
  dI <- beta * S * I*connect_probability  - gamma * I
  
  
  return(list(c(dS, dI)))
}

#small network with Different connect probability
#when \( N = 50 \) (number of vertices)
#\item \( \beta = 0.15 \) (infection rate)
 #\item \( \gamma = 0.15 \) (recovery rate)
 #\item day = 100 (the length of time points)
 #\item The initial number of infected people is roughly \(0.2\times N\)
#the figure 4
num_note<-50
day <- 100
beta <- 0.15
gamma <- 0.15
X <- rbinom(num_note, 1, 0.2)
N <- length(X)
state <- c(S =(num_note - sum(X)), I = sum(X))
times <- 1:day
parameters <- c(beta, gamma)
par(mfrow = c(1, 2))


plot(1, 1, type="n", xlim = c(0,day+10), ylim=c(0, num_note+10), xlab="Time", ylab="Number of I")
title(main="SIS: stochastic and node-level model with different connect probability")
plot(1, 1, type="n", xlim = c(0,day+10), ylim=c(0, num_note+10), xlab="Time", ylab="Number of I")
title(main="SIS: stochastic and OED model with different connect probability")
colors <- rainbow(length(seq(0.001, 0.1, by=0.01)))
color_index <- 1
for(connect_probability in seq(0.001, 0.1, by=0.01)){
  
  G <- erdos.renyi.game(num_note, connect_probability)
  layout <- layout_with_fr(G)
  A <- get.adjacency(G, sparse = FALSE)
  
  result<-ex_SIS_model1(X, A, beta, gamma, day)
  num_infected<-result$num_infected
  num_susceptible<-result$num_susceptible
  print(c("1",num_infected[length(num_infected)]))
  
  out <- ode(y = X, times = times, func = SIS_net_ODEMODEL, parms = parameters, A = A)
  out <- as.data.frame(out)
  
  sumsI <- rowSums(out[,-1])
  
  out1 <- out1 <- ode(y = state, times = times, func = SIS_ODEMODEL, parms = list(beta =beta, gamma=gamma),connect_probability=connect_probability)
  out1 <- as.data.frame(out1)
  
  S_pred1 <- out1[ , "S"]
  I_pred1 <- out1[ , "I"]
  print(c("3",I_pred1[length(I_pred1)]))
  par(mfg = c(1, 1))
  lines(times, num_infected, lty = 1, col = colors[color_index])
  text(x = 100, y = num_infected[length(num_infected)]+1, labels = connect_probability, cex = 0.7, pos = 4, col = 'black')
  lines(times, sumsI, lty = 2, col = colors[color_index])
  # lines(times, I_pred, lty = 2, col = colors[color_index])
  text(x = 80, y = sumsI[length(sumsI)]+1, labels = connect_probability, cex = 0.7, pos = 4, col = 'black')
  
  par(mfg = c(1, 2))
  lines(times, num_infected, lty = 1, col = colors[color_index])
  text(x = 100, y = num_infected[length(num_infected)]+1, labels = connect_probability, cex = 0.7, pos = 4, col = 'black')
  lines(times, I_pred1, lty = 2, col = colors[color_index])
  text(x = 90, y = I_pred1[length(I_pred1)]+1, labels = connect_probability, cex = 0.7, pos = 4, col = 'black')
  
  color_index <- color_index + 1
  
}
par(mfg = c(1, 1))
legend("topright", legend = c("stochastic model", "node-level model"), lty = c(1, 2), col = c("blue", "red"))
par(mfg = c(1, 2))
legend("topright", legend = c("stochastic model", "OED model"), lty = c(1, 2), col = c("blue", "red"))


#small network with Different recovery rate
#when \( N = 50 \) (number of vertices)
#\item \( \beta = 0.15 \) (infection rate)
 #\item \( \eta = 0.01 \) (connection rate in the homogeneous mixture network)
 #\item day = 100 (the length of time points)
 #\item The initial number of infected people is roughly \(0.2\times N\)
#the figure 5
# For large network just change num_note to 1000, connect_probability to 0.001
#the figure 7


num_note<-50
day <- 100
connect_probability<-0.01
beta <- 0.15
X <- rbinom(num_note, 1, 0.2)
state <- c(S =(num_note - sum(X)), I = sum(X))
times <- 1:day
G <- erdos.renyi.game(num_note, connect_probability)
layout <- layout_with_fr(G)
A <- get.adjacency(G, sparse = TRUE)
par(mfrow = c(1, 2))

plot(1, 1, type="n", xlim = c(0,day+10), ylim=c(0, num_note), xlab="Time", ylab="Number of I")
title(main="SIS: stochastic and node-level model with different recover rate")
plot(1, 1, type="n", xlim = c(0,day+10), ylim=c(0, num_note), xlab="Time", ylab="Number of I")
title(main="SIS: stochastic and OED model with different recover rate")
colors <- rainbow(length(seq(0.05, 0.95, by=0.05)))
color_index <- 1
for(gamma in seq(0.05, 0.95, by=0.05)){
  parameters <- c(beta, gamma)
  
  result<-ex_SIS_model1(X, A, beta, gamma, day)
  num_infected<-result$num_infected
  num_susceptible<-result$num_susceptible
  
  out <- ode(y = X, times = times, func = SIS_net_ODEMODEL, parms = parameters, A = A)
  out <- as.data.frame(out)
  
  
  sumsI <- rowSums(out[,-1])
  
  out1 <- ode(y = state, times = times, func = SIS_ODEMODEL, parms = list(beta =beta, gamma=gamma),connect_probability=connect_probability, hmax = 0.1)
  out1 <- as.data.frame(out1)
  
  S_pred1 <- out1[ , "S"]
  I_pred1 <- out1[ , "I"]
  
  par(mfg = c(1, 1))
  lines(times, num_infected, lty = 1, col = colors[color_index])
  text(x = 100, y = num_infected[length(num_infected)]+1, labels = gamma, cex = 0.7, pos = 4, col = 'black')
  lines(times, sumsI, lty = 2, col = colors[color_index])
  text(x = 80, y = sumsI[length(sumsI)]+1, labels = gamma, cex = 0.7, pos = 4, col = 'black')
  
  par(mfg = c(1, 2))
  lines(times, num_infected, lty = 1, col = colors[color_index])
  text(x = 100, y = num_infected[length(num_infected)]+1, labels = gamma, cex = 0.7, pos = 4, col = 'black')
  lines(times, I_pred1, lty = 2, col = colors[color_index])
  text(x = 90, y = I_pred1[length(I_pred1)]+1, labels = gamma, cex = 0.7, pos = 4, col = 'black')

  color_index <- color_index + 1
  
}
par(mfg = c(1, 1))
legend("topright", legend = c("stochastic model", "node-level model"), lty = c(1, 2), col = c("black", "black"))
par(mfg = c(1, 2))
legend("topright", legend = c("stochastic model", "OED model"), lty = c(1, 2), col = c("black", "black"))



#small network with Different infection rate
#when \( N = 50 \) (number of vertices)
 #\item \( \gamma = 0.15 \) (recovery rate)
 #\item \( \eta = 0.01 \) (connection rate in the homogeneous mixture network)
 #\item day = 100 (the length of time points)
 #\item The initial number of infected people is roughly \(0.2\times N\)
#the figure 6
# For large network just change num_note to 1000, connect_probability to 0.001
#the figure 8


num_note<-50
day <- 100
connect_probability<-0.01
beta <- 0.15
gamma <- 0.15
X <- rbinom(num_note, 1, 0.2)
state <- c(S =(num_note - sum(X)), I = sum(X))
times <- 1:day
#connect_probability<-0.001
G <- erdos.renyi.game(num_note, connect_probability)
layout <- layout_with_fr(G)
#plot(G, layout = layout, vertex.size = 5, vertex.label = NA, edge.arrow.size = 0.5)
A <- get.adjacency(G, sparse = FALSE)
par(mfrow = c(1, 2))


plot(1, 1, type="n", xlim = c(0,day+10), ylim=c(0, num_note), xlab="Time", ylab="Number of I")
title(main="SIS: stochastic and node-level model with different infected rate")
plot(1, 1, type="n", xlim = c(0,day+10), ylim=c(0, num_note), xlab="Time", ylab="Number of I")
title(main="SIS: stochastic and OED model with different infected rate")
#colors <- rainbow(length(seq(0.001, 0.1, by=0.05)))
colors <- rainbow(length(seq(0.05, 0.95, by=0.05)))
color_index <- 1

for(beta in seq(0.05, 0.95, by=0.05)){
  parameters <- c(beta, gamma)
  print(c('0',beta,(max(eig_results$values)*beta-gamma)/gamma))
  
  result<-ex_SIS_model1(X, A, beta, gamma, day)
  num_infected<-result$num_infected
  num_susceptible<-result$num_susceptible
  
  out <- ode(y = X, times = times, func = SIS_net_ODEMODEL, parms = parameters, A = A)
  out <- as.data.frame(out)

  sumsI <- rowSums(out[,-1])
  
  out1 <- ode(y = state, times = times, func = SIS_ODEMODEL, parms = list(beta =beta, gamma=gamma),connect_probability=connect_probability, hmax = 0.1)
  out1 <- as.data.frame(out1)
  
  S_pred1 <- out1[ , "S"]
  I_pred1 <- out1[ , "I"]
  
  par(mfg = c(1, 1))
  lines(times, num_infected, lty = 1, col = colors[color_index])
  text(x = 100, y = num_infected[length(num_infected)]+1, labels = beta, cex = 0.7, pos = 4, col = 'black')
  lines(times, sumsI, lty = 2, col = colors[color_index])
  text(x = 80, y = sumsI[length(sumsI)]+1, labels = beta, cex = 0.7, pos = 4, col = 'black')
  par(mfg = c(1, 2))
  lines(times, num_infected, lty = 1, col = colors[color_index])
  text(x = 100, y = num_infected[length(num_infected)]+1, labels = beta, cex = 0.7, pos = 4, col = 'black')
  lines(times, I_pred1, lty = 2, col = colors[color_index])
  text(x = 90, y = I_pred1[length(I_pred1)]+1, labels = beta, cex = 0.7, pos = 4, col = 'black')
  
  color_index <- color_index + 1
  
}

par(mfg = c(1, 1))
legend("topright", legend = c("stochastic model", "node-level model"), lty = c(1, 2), col = c("black", "black"))
par(mfg = c(1, 2))
legend("topright", legend = c("stochastic model", "OED model"), lty = c(1, 2), col = c("black", "black"))


