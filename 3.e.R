S <- 100 
T <- 1/12 
sigma <- 20 


priceBach <- function(K, sigma, T){
  d1 <- (S-K)/(sigma*sqrt(T))
  (S-K)*pnorm(d1) + sigma*sqrt(T)*dnorm(d1)
}


priceBS <- function(K, sigma, T){
  d1 <- (log(S/K) + T*sigma^2/2)/(sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  S*pnorm(d1) - K * pnorm(d2)
}


IV <- function(obsprice, K){
  difference <- function(sigmaBS){
    abs(priceBS(K, sigmaBS, T) - obsprice)
  }
  optimize(difference, c(10^(-6), 10), maximum = FALSE)$minimum 
}


BM_func <- function(K){
  sigma/K
}


approx1_func <- function(K){
  sqrt(((sigma/S)^2 + (sigma/K)^2)/2)
}


Lamperti_inside <- function(u){
  1/(u*(sigma/u))
}


approx2_func <- function(K){
  result <- integrate(Lamperti_inside, K, S)
  if (result$message != "OK") {
    return(NA) 
  }
  return (log(S/K)/result$value)
}


strikes <- c(50:150)
trueIV <- rep(0, length(strikes))
BM <- rep(0, length(strikes))
approx1 <- rep(0, length(strikes))
approx2 <- rep(0, length(strikes))


for (i in 1:length(strikes)){
  K <- i+min(strikes)-1
  price <- priceBach(K, sigma, T)
  trueIV[i] <- IV(price, K)
  BM[i] <- BM_func(K)
  approx1[i] <- approx1_func(K)
  approx2[i] <- approx2_func(K)
}

