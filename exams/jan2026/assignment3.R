set.seed(111)
data <- read.csv("https://github.com/STIMALiU/AdvMLCourse/raw/master/GaussianProcess/Code/TempTullinge.csv", header=TRUE, sep=";")

time <- 1:2190
day <- rep(1:365, 6)
query_points <- 1:2190
time <- seq(1, 2190, by=5)

idx <- seq(1, 2190, by = 5)
time_sample <- sample(time, 100); day <- day[idx]; temp <- data$temp[idx]
# kernlab-style SE kernel: outer function fixes the hyperparam


SEKernel <- function(x1,x2, sigmaF, l){
  n1 <- length(x1)
  n2 <- length(x2)
  K <- matrix(NA,n1,n2)
  for (i in 1:n2){
    K[,i] <- (sigmaF^2)*exp(-0.5*( (x1-x2[i])/l)^2 )
  }
  return(K)
}

# Your code here.

# Kxs <- SEKernel(x,xs)
# Kss <- SEKernel(xs,xs)
# Kxz <- SEKernel(x,z)
# Kzz <- SEKernel(z,z)

matrix_inv <- function(A_inv, U, C_inv, V){
  #assuming A is sigmanoise_2 * I
  #A_inv <- solve(A, diag(dim(A)[1]), tol=0)
  #C_inv <- solve(C, diag(dim(C)[1]), tol=0)
  CVAU_inv <- solve(C_inv + V%*%A_inv%*%U, diag(dim(C_inv)[1]), tol=0)
  return(A_inv - A_inv%*%U%*%CVAU_inv%*%V%*%A_inv)
}

# calculates posterior mean and variance based on inducing points Z
posteriorGP_approx <- function(X, y, XStar, sigmaNoise, k, Z) {
  K <- k(X, X)
  K_star <- k(X, XStar)
  K_star_star <- k(XStar, XStar)

  K_xz <- k(X, Z)
  K_zz <- k(Z, Z)
  K_zx <- k(Z, X)
  jitter <- 1e-3
  nystrom_approx <- matrix_inv(A_inv=diag(nrow(K))*1/sigmaNoise**2, U=K_xz, C_inv=K_zz + jitter * diag(nrow(K_zz)), V=K_zx)
  # Posterior mean
  f_star_mean <- t(K_star)%*%nystrom_approx%*%y
  
  # Posterior variance: diag(K**) - diag(vˆT v) (= colSums(vˆ2), faster)
  f_star_var <- diag(K_star_star - t(K_star)%*%nystrom_approx%*%K_star)
  #f_star_var <- diag(k(XStar, XStar)) - diag(t(v) %*% v)
  return(list(mean = f_star_mean, var = f_star_var, n=nystrom_approx))
}

k <- function(x1, x2) SEKernel(x1, x2, sigmaF = 20, l = 100)

fit <- lm(temp ~ time + I(time**2))
sigmaN <- sqrt(var(fit$residuals))

res <- posteriorGP_approx(time, temp, query_points, sigmaNoise = sigmaN, k, time_sample)

plotGP <- function(X, y, XStar, res, main = "", ylim = c(-2.5, 2.5),
                   xlab = "time", ylab = "temp", ptcol = "grey", ptcex = 0.5) {
  sd <- sqrt(res$var)
  plot(XStar, res$mean, type = "l", lwd = 2, ylim = ylim,
       xlab = xlab, ylab = ylab, main = main)
  lines(XStar, res$mean + 1.96 * sd, col = "blue", lty = 2)
  lines(XStar, res$mean - 1.96 * sd, col = "blue", lty = 2)
  points(X, y, pch = 19, col = ptcol, cex = ptcex)
}


plotGP(time, temp, query_points, res, main = "using approx", ylim=range(temp))
res_old <- posteriorGP_old(time, temp, query_points, sigmaNoise = sigmaN, k)

plotGP(time, temp, query_points, res_old, main = "old one", ylim=range(temp))












posteriorGP_old <- function(X, y, XStar, sigmaNoise, k) {
  K <- k(X, X)
  # chol() returns upper triangular R (K = RˆT R), so t() gives lower L
  L <- t(chol(K + sigmaNoise**2 * diag(nrow(K))))
  # alpha = (K + sigmaNˆ2 I)ˆ-1 y = LˆT \ (L \ y)
  alpha <- solve(t(L), solve(L, y))
  # K* = K(X, XStar), n x n*
  k_star <- k(X, XStar)
  # Posterior mean: K*ˆT alpha
  f_star_mean <- t(k_star) %*% alpha
  # v = L \ K*, so vˆT v = K*ˆT (K + sigmaNˆ2 I)ˆ-1 K*
  v <- solve(L, k_star)
  # Posterior variance: diag(K**) - diag(vˆT v) (= colSums(vˆ2), faster)
  f_star_var <- diag(k(XStar, XStar)) - diag(t(v) %*% v)
  return(list(mean = f_star_mean, var = f_star_var))
}