#All the code for chapter4 will be shown here

#chapter 4 
# 4.2 Raw data and data screening
#Download 100 FTSE 100 shares from Yahoo
#Data screening was performed to obtain data on 33 stocks


if (!require(quantmod)) {
  install.packages("quantmod")
  library(quantmod)
}

fush100_name <- c("III", "ABDN", "ADM", "AAF", "AAL", "ANTO", "AHT", "ABF",
                  "AZN", "AUTO", "AV", "BME", "BP", "BA", "BARC", "BDEV",
                  "BEZ", "BKG", "BT-A", "BATS", "BNZL", "BRBY", "CNA","CCH",
                  "CPG", "CTEC", "CRH", "CRDA", "DCC", "DGE", "EDV", "ENT",
                  "EXPN", "FCIT", "FLTR", "FRAS", "FRES", "GLEN", "GSK", "HSBA",
                  "HLN", "HLMA", "HL", "HSX", "IMI", "IMB", "INF", "IHG", "IAG",
                  "ITRK", "JD", "JMAT", "KGF", "LAND", "LGEN", "LLOY", "LSEG",
                  "MNG", "MRO", "MNDI", "NG", "NWG", "NXT", "OCDO", "PSON",
                  "PSH", "PSN", "PHNX", "PRU", "RKT", "REL", "RTO", "RMV",
                  "RIO", "RS1", "SGE", "SBRY", "SDR", "SMT", "SGRO",
                  "SVT", "SHEL", "SN", "SMDS", "SMIN", "SKG","SPX", "SSE",
                  "STJ", "STAN", "TW", "TSCO", "ULVR", "UTG", "UU", "VOD",
                  "WEIR", "WTB", "WPP")

symbols <- paste0(fush100_name, ".L")
start_date <- as.Date("2020-5-01")  # start at 2020-5-01
end_date <- as.Date("2021-11-01") # end at 2021-11-01

for (symbol in symbols) {
  tryCatch({
    symbol_data <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE) 
    
    # Creates a dataframe that contains the date and closing price, but does not remove the rows containing the missing values
    symbol_data <- data.frame(Date=index(symbol_data),
                              Open=as.vector(Op(symbol_data)), 
                              High = as.vector(Hi(symbol_data)), 
                              Low = as.vector(Lo(symbol_data)), 
                              Close=as.vector(Cl(symbol_data)), 
                              Volume=as.vector(Vo(symbol_data)), 
                              Adjusted=as.vector(Ad(symbol_data)))
    # Calculation of average price
    symbol_data$Avg_Price <- rowMeans(symbol_data[, c("High", "Low")])
    assign(gsub("\\.", "_", symbol), symbol_data, envir = .GlobalEnv)
  }, error = function(e) {
    message(paste("Unable to import", symbol, ":", e$message))
  })
}

first_symbol <- gsub("\\.", "_", symbols[1])
fush100_price <- get(first_symbol)[, c("Date", "Avg_Price")]
colnames(fush100_price)[2] <- first_symbol

for (symbol in symbols[-1]) {  
  symbol_data <- get(gsub("\\.", "_", symbol))
  symbol_data <- symbol_data[, c("Date", "Avg_Price")]
  colnames(symbol_data)[2] <- gsub("\\.", "_", symbol)
  
  if (gsub("\\.", "_", symbol) %in% colnames(fush100_price)) {
    fush100_price[gsub("\\.", "_", symbol)] <- symbol_data[, 2]
  } else {
    fush100_price <- merge(fush100_price, symbol_data, by = "Date", all = TRUE)
  }
}


#Screening for stocks that did not contain vacancies during the study period
if (!require(zoo)) {
  install.packages("zoo")
}
library(zoo)
if (any(is.na(fush100_price[, -1]))) {
  fush100_price <- fush100_price[, c(TRUE, colSums(is.na(fush100_price[, -1])) == 0)]
}
company_name <- colnames(fush100_price[, -1])
length(company_name)






#3.2 Data procession，Three diagrams
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

#Figure 9: Time plot of stock prices between May 2020 and May 2021
fush100_price_long <- tidyr::pivot_longer(fush100_price, -Date, names_to = "Symbol", values_to = "Avg_Price")
ggplot(fush100_price_long, aes(x = Date, y = Avg_Price, color = Symbol)) +
  geom_line(alpha = 0.8) +
  labs(x = "Date", y = "Average Price", color = "Symbol") +
  theme(legend.position = "none")




#Figure 10: Time plot of stock prices log-return between May 2020 and May 2021
log_fush100_price <- fush100_price
log_fush100_price[,-1] <- lapply(log_fush100_price[,-1], function(x) c(diff(log(x)), NA))
log_fush100_price <- log_fush100_price[-nrow(log_fush100_price), ]
log_fush100_price_long <- tidyr::pivot_longer(log_fush100_price, -Date, names_to = "Symbol", values_to = "return")
ggplot(log_fush100_price_long, aes(x = Date, y = return, color = Symbol)) +
  geom_line(alpha = 0.8) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(x = "Date", y = "log return", color = "Symbol") +
  theme(legend.position = "none")



#Figure 11: Log return plot for the 33 stocks during first 30 day                  
log_fush100_price2 <- log_fush100_price[0:29, ]
log_fush100_price_long2 <- tidyr::pivot_longer(log_fush100_price2, -Date, names_to = "Symbol", values_to = "return")
ggplot(log_fush100_price_long2, aes(x = Date, y = return, color = Symbol)) +
  geom_line(alpha = 0.8) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(x = "Date", y = "log return", color = "Symbol") +
  theme(legend.position = "none")









#4.3 Network Construction


library(igraph)

#Deleted due to 27th stock data not fitting the model                 
log_fush100_price <- log_fush100_price[,-27]
company_name<-colnames(log_fush100_price[,-1])
length(company_name)

library(moments)

#Calculate the associated kurtosis and skewness
skewness <- skewness(log_fush100_price[,-1])
kurtosis <- kurtosis(log_fush100_price[,-1])
print(paste("Skewness of returns: ", skewness))
print(paste("Kurtosis of returns: ", kurtosis))

#Table 7: The summary of the skewness and kurtosis for the 32 stocks
summary(skewness)
summary(kurtosis)

library(rugarch)

#4.3.2 fit AR(1)-EGARCH(1,1)-t model
residuals_list <- list()
conditional_volatility_list<- list()
error_columns <- c()
standardized_residuals_list <- list()

for (i in 2:ncol(log_fush100_price)) {
  AR.GARCH.spec <- ugarchspec(variance.model = list(model = "eGARCH", 
                                                    garchOrder = c(1, 1)), 
                              mean.model = list(armaOrder = c(1, 0), 
                                                include.mean = TRUE), 
                              distribution.model = "std")
  fit <- tryCatch(
    ugarchfit(spec = AR.GARCH.spec, data = log_fush100_price[,i]),
    error = function(e) {
      message(paste("Error in column", i, ":", e$message))
      error_columns <- c(error_columns, i)
      return(NULL)
    }
  )
  
  if (!is.null(fit)) {
    residuals_list[[i-1]] <- residuals(fit)
    conditional_volatility_list[[i-1]] <- sigma(fit)
    standardized_residuals_list[[i-1]] <- residuals_list[[i-1]]/conditional_volatility_list[[i-1]]
    
  }
}

#Figure 12: ACF plot for the last stock
plot(fit , which = 6)
#Figure 13: Standardized Residual Squared ACF for the last stock                                 
plot(fit , which = 11)


residuals_list <- lapply(residuals_list, as.numeric)
conditional_volatility_list<- lapply(conditional_volatility_list, as.numeric)
standardized_residuals_list<- lapply(standardized_residuals_list, as.numeric)

#copula model is employed to capture the de-
#pendency structure of residuals.
ecdf_func_list <- lapply(residuals_list, ecdf)
uniform_data <- mapply(function(x, f) f(x), residuals_list, ecdf_func_list)
summary(uniform_data)

if (!require(copula)) {
  install.packages("copula")
}
library(copula)
library(rugarch)

#Compute the upper and lower tail matrices
upper_tail_dep_mat <- matrix(0, ncol = 32, nrow = 32)
lower_tail_dep_mat <- matrix(0, ncol = 32, nrow = 32)

for (i in 1:32) {
  for (j in 1:32) {
    # Skip if i and j are equal
    if (i == j) {
      next
    }
    data_mat_ij <- as.data.frame(uniform_data[, c(i, j)])
    if (any(is.na(data_mat_ij)) || any(apply(data_mat_ij, 2, is.nan)) || any(apply(data_mat_ij, 2, is.infinite))) {
      data_mat_ij <- na.omit(data_mat_ij)
    }
    if (length(unique(data_mat_ij)) < 2) {
      next
    }
    copula <- claytonCopula()
    fit <- fitCopula(copula, as.matrix(data_mat_ij))
    kendall_iTau <- cor(data_mat_ij, method = "kendall")[1,2]
    spearman_rho <- cor(data_mat_ij, method = "spearman")[1,2]
    # Get tail dependencies  
    deps <- lambda(fit@copula)
    # Lower tail dependence
    lower_dep <- deps[1]  
    # Populate matrix
    lower_tail_dep_mat[i,j] <- lower_dep
    upper_tail_dep_mat[i, j] <- 1 - (1 - spearman_rho)^(1/log(2))
  }
}

#Output upper and lower tail dependency matrices
print(lower_tail_dep_mat)
print(upper_tail_dep_mat)

#Convert the upper and lower tail dependency matrices into upper and lower tail distance matrices
dist_mat1 <- sqrt(1 - lower_tail_dep_mat)
dist_mat2 <- sqrt(1 - upper_tail_dep_mat)
dist_mat1
dist_mat2
#Building and mapping networks                       
library(igraph)
G1 <- graph_from_adjacency_matrix(dist_mat1, mode = "undirected", weighted = TRUE, diag = FALSE)
G2 <- graph_from_adjacency_matrix(dist_mat2, mode = "undirected", weighted = TRUE, diag = FALSE)
length(V(G2))
V(G1)$name <- company_name
V(G2)$name <- company_name


par(mfrow = c(1, 2))
#Figure 14: Networks constructed from lower tail distance matrices
l <- layout_with_drl(G1)
igraph_options(vertex.size=10)
plot(G1, layout=l, vertex.label=V(G1)$name,vertex.label.size=0.5,
     vertex.label.color="blue", vertex.color="#8ECFC9", main="Network graph constructed from the lower tail distance matrix")

#Figure 14: Networks constructed from upper tail distance matrices
l <- layout_with_drl(G2)
igraph_options(vertex.size=10)
plot(G2, layout=l, vertex.label=V(G2)$name,vertex.label.size=0.5,
     vertex.label.color="blue", vertex.color="#8ECFC9", main="Network graph constructed from the upper tail distance matrix")

par(mfrow = c(1, 1))


#Determine the value of the 75 per cent quantile as the threshold value
p_75_values <- numeric()
for (i in 1:length(conditional_volatility_list)) {
  print(summary(conditional_volatility_list[[i]]))
  p_75 <- quantile(conditional_volatility_list[[i]], 0.75)
  p_75_values <- c(p_75_values, p_75)
}
p_75_value <- mean(p_75_values)
p_75_value

#The stock market is only open for 5 days in a week, 
#and values exceeding the threshold for 3 or more of the 5 days 
#are considered high risk stocks and are recorded as 1, otherwise they are 0                      
get_data <- function(conditional_volatility_list) {
  day <- length(conditional_volatility_list[[1]])
  week <- day %/% 5
  matrix_A <- matrix(0, nrow = length(conditional_volatility_list), ncol = week)
  for (j in 1:length(conditional_volatility_list)) {
    for (i in 1:week) {
      count <- sum(conditional_volatility_list[[j]][(i*5-4):(i*5)] > p_75_value)
      if (count >= 3) {
        matrix_A[j, i] <- 1
      } else {
        matrix_A[j, i] <- 0
      }
    }
  }
  return(matrix_A)
}

#Storing data for state in X
X <- get_data(conditional_volatility_list)

#Draw a dot plot of the total number of high-risk and non-high-risk stocks per week                       
t <- ncol(X)
S_observed <- numeric(ncol(X))
I_observed <- numeric(ncol(X))
for(i in 1:ncol(X)){
  for (j in 1:length(conditional_volatility_list)) {
    if(X[j, i]==1){
      I_observed[i] <- I_observed[i]+1
    }else{
      S_observed[i] <- S_observed[i]+1
    }
  }
}
S_observed
I_observed

#Calculate the total number of weeks            
week <- 1:ncol(X)

#Figure 15: Scatter plot of the number of high-risk and non-high-risk stocks in the real world over 75 weeks
plot(week,S_observed,type = "p",xlab = "Time",ylab = "Number",col = "blue",
     ylim = c(min(S_observed,I_observed), max(S_observed,I_observed)))
points(week, I_observed, col = "red")
legend("topright", legend = c("S observed", "I observed"), 
       col = c("blue", "red"), pch = c( 1, 1), bg = "transparent")
par(mfrow = c(1, 1))

#Constructing unit matrices
#Change the diagonal value in the distance matrix to 1                       
E <- diag(length(conditional_volatility_list))
A1 <- dist_mat1 - E
A2 <- dist_mat2 - E
library(deSolve)

########################################
#Split the data into 2 parts, the first being from 1 May 2020 to 1 May 2021 for fitting the model sign as pre
#The second is the half-yearly data after 1 May 2021, which is used to compare the forecast results sign as fut

S_observed_all <- S_observed
I_observed_all <- I_observed
X_all <- X

# Number of days for prediction
n_pre <- 50

# Split S_observed and I_observed
S_observed_pre <- S_observed_all[1:n_pre]
I_observed_pre <- I_observed_all[1:n_pre]

# Use the rest for prediction
S_observed_fut <-
  S_observed_all[(n_pre + 1):length(S_observed_all)]
I_observed_fut <-
  I_observed_all[(n_pre + 1):length(I_observed_all)]

# Also split the matrix X
X_pre <- X_all[, 1:n_pre]
X_fut <- X_all[, (n_pre + 1):ncol(X_all)]

# Estimate parameters using prediction data
# Define SIS model function
SISmodel <- function(t, y, parms, A, X) {
  S <- y[1]
  I <- y[2]
  lambda <- parms[1]
  gamma <- parms[2]
  dS <- -lambda * sum(A %*% X[,t+1]) + gamma * I
  dI <- lambda * sum(A %*% X[,t+1]) - gamma * I
  return(list(c(dS = dS, dI = dI)))
}

# New error function
error_func <- function(par, S_observed, I_observed, day, A, X) {
  lambda <- par[1]
  gamma <- par[2]
  IC <- c(S = S_observed[1], I = I_observed[1])  # assuming R0 = 0
  parms <- c(lambda, gamma) 
  t <- seq(0, day-1, 1)  # as time starts from 0 in the ode function  
  # Solve ODE
  out <- ode(y = IC, times = t, func = SISmodel, parms = parms, A = A, X = X)
  # Extract S and I
  S <- out[ , "S"]
  I <- out[ , "I"]
  
  # Calculate error
  error_S <- sum((S - S_observed)^2)
  error_I <- sum((I - I_observed)^2)
  error <- error_S + error_I
  return(error)
} 
par <- c(0.1, 0.1)                       
result <-optim(par,error_func,S_observed = S_observed_pre,
               I_observed = I_observed_pre,day = n_pre,A = A1,X = X_pre)

# Estimated parameters from model prediction
lambda_hat <- result$par[1]
lambda_hat
gamma_hat <- result$par[2]
gamma_hat

# Define time points for pre and fut set
t_pre <-seq(0, length(S_observed_pre) - 1, 1)  
t_fut <- seq(0, length(S_observed_fut) - 1, 1)

# Use estimated parameters to predict the pre and fut set
IC_pre <-c(S = S_observed_pre[1], I = I_observed_pre[1])
out_pre <-ode(y = IC_pre,times = t_pre,func = SISmodel,
              parms = c(lambda_hat, gamma_hat),A = A1,X = X_pre)

IC_fut <-c(S = S_observed_pre[n_pre], I = I_observed_pre[n_pre])
out_fut <-ode(y = IC_fut,times = t_fut,func = SISmodel,
              parms = c(lambda_hat, gamma_hat),A = A1,X = X_fut)

# Extract S and I from solution
S_pred_pre <- out_pre[, "S"]
I_pred_pre <- out_pre[, "I"]

S_pred_fut <- out_fut[, "S"]
I_pred_fut <- out_fut[, "I"]

#Figure 16: Comparative Graph of Fitted Curve and Actual Scatter Plot Over 50 Weeks                       
par(mfrow = c(1, 1))

# Plot observed and predicted S for the prediction set
plot(t_pre,S_observed_pre,type = "p",xlab = "Time",ylab = "Number",col = "blue",
     ylim = c(min(S_observed_pre,I_observed_pre), max(S_observed_pre,I_observed_pre)))
lines(t_pre,S_pred_pre,col = "lightblue",lwd = 3)
# Add observed and predicted I to the plot for the prediction set
points(t_pre, I_observed_pre, col = "red")
lines(t_pre, I_pred_pre, col = "pink", lwd = 3)
# Add legend
legend("topright",legend = c("S observed", "S predicted", "I observed", "I predicted"),
       col = c("blue", "lightblue", "red", "pink"),lty = c(0, 1,0,1),pch = c(1, NA, 1, NA), bg = "transparent")

#Figure 17: Comparative Graph of Predicted Curve and Actual Scatter Plot Over 25 Weeks                       
# Plot observed and predicted S for the fut set
plot(t_fut,S_observed_fut,type = "p",xlab = "Time",ylab = "Number",col = "blue",
     ylim = c(min(S_observed_pre-5,I_observed_pre-5), max(S_observed_pre+5,I_observed_pre+5)))
lines(t_fut, S_pred_fut, col = "lightblue", lwd = 3)
# Add observed and predicted I to the plot for the fut set
points(t_fut, I_observed_fut, col = "red")
lines(t_fut, I_pred_fut, col = "pink", lwd = 3)
# Add legend
legend("topright",legend = c("S observed", "S predicted", "I observed", "I predicted"),
       col = c("blue", "lightblue", "red", "pink"),lty = c(0, 1,0,1),pch = c(1, NA, 1, NA), bg = "transparent")

#Figure 18: Comparative Graph of the Predicted Curve and Actual Scatter Plot for High-Risk Stocks Over 25 Weeks
# Plot observed and predicted S for the fut set
plot(t_fut,I_observed_fut,type = "p",xlab = "Time",
     ylab = "Number of I",col = "red")
lines(t_fut, I_pred_fut, col = "blue")
legend("topright",legend = c("I observed", "I predicted"),
       col = c("red","blue"),lty = c(0, 1),pch = c(1, NA),bg = "transparent")


##########################
#Consider different rates of infection and infected under each stock                       
#error function
error_func2 <- function(par, S_observed, I_observed, day, learning_rate, A, X) {
  lambda <- par[1:32]
  gamma <- par[33:64]
  
  t <- day
  S <- numeric(t)
  I <- numeric(t)
  S[1] <- S_observed[1]
  I[1] <- I_observed[1]
  E1 <- rep(1,32)
  
  b <- matrix(nrow = 32, ncol = t)  # Initialize b
  h <- matrix(nrow = 32, ncol = t)  # Initialize h
  
  for (i in 2:t) {
    for(j in 1:32){
      b[j,i-1] <- lambda[j] * X[j,i-1]
      h[j,i-1] <- gamma[j] * (E1[j] - X[j,i-1])
    }
    dS <-  (sum(h[,i-1]) - sum(A %*% b[,i-1]))
    dI <- (sum(A %*% b[,i-1]) - sum(h[,i-1]))
    
    S[i] <- S[i-1] + learning_rate * dS
    I[i] <- I[i-1] + learning_rate * dI
  }
  
  error_S <- sum((S - S_observed)^2)
  error_I <- sum((I - I_observed)^2)
  error <- error_S + error_I
  
  return(error)
}

# starting parameter
learning_rate <-0.05
beta0 <- rep(0.00654, 32)
gamma0 <- rep(0.191, 32)
params0 <- c(beta0, gamma0)

# Optimisation by least squares
result2 <- optim(params0, error_func2, S_observed=S_observed, I_observed=I_observed, 
                 day=ncol(X), learning_rate=learning_rate, A=A1, X=X)

# Print the optimal parameters                 
print(result2$par)

#Getting the optimal parameters                       
#table9 and table 10
lambda_opt <- result2$par[1:32]
gamma_opt <- result2$par[33:64]

#Perform model fitting                       
t <- ncol(X)
S <- numeric(t)
I <- numeric(t)
S[1] <- S_observed[1]
I[1] <- I_observed[1]
E1 <- rep(1,32)

b <- matrix(nrow = 32, ncol = t)  # Initialize b
h <- matrix(nrow = 32, ncol = t)  # Initialize h

#Generating estimates of S and I using optimal parameters 
for (i in 2:t) {
  for(j in 1:32){
    b[j,i-1] <- lambda_opt[j] * X[j,i-1]
    h[j,i-1] <- gamma_opt[j] * (E1[j] - X[j,i-1])
  }
  dS <-  (-sum(A1 %*% b[,i-1]) + sum(h[,i-1]))
  dI <- (sum(A1 %*% b[,i-1]) - sum(h[,i-1]))
  
  S[i] <- S[i-1] + learning_rate * dS
  I[i] <- I[i-1] + learning_rate * dI
}
S
I

min_time <- 1
max_time <- length(S_observed)
min_count <- min(c(S_observed, I_observed, S, I))
max_count <- max(c(S_observed, I_observed, S, I))
length_x <- 1:length(S_observed)

#Figure 19: The comparison of model curves with actual data points, when each stock has
different recovery and infection rates
plot(length_x,S_observed, col = "blue",xlim = c(min_time, max_time), ylim = c(min_count, max_count))
points(length_x,I_observed, col = "red")
lines(length_x,S, col = "blue")
lines(length_x,I, col = "red")
legend("topright", 
       legend=c("S_observed", "I_observed", "S_predicted", "I_predicted"), 
       col=c("blue", "red", "blue", "red"), 
       lty = c(0, 0,1,1),
       pch = c(1, 1,NA, NA),
       bg = 'transparent')
