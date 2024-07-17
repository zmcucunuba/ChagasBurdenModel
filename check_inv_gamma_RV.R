set.seed(1)
mu <- 1
cv <- 5

param <- epitrix::gamma_mucv2shapescale(mu = mu,cv = cv)

x1 <- rgamma(n = 1e4, shape = param$shape, scale = param$scale)
hist(x1)

x2 <- 1/x1
hist(x2)

x3 <- r
c(mean(x1),sd(x1)/mean(x1))
c(mean(x2),sd(x2)/mean(x2))

# x1 -> gamma(shape = k, scale = theta)
# mean = k*theta
# sd = sqrt(k*theta^2)
# cv = 1/sqrt(k)

# x2 = 1/x1 -> inv-gamma(shape = k, scale = theta)
# mean = theta / (k-1)    # defined only for k>1
# sd = sqrt( theta^2 / (k-1)^2 / (k-2)  )    # defined only for k>2
# cv = sd/mean
