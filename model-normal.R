## model
modelString = "
  model{
    for(i in 1:n){
      Y[i] ~ dnorm(mu[i],tau.Y)
      mu[i] <- a[id[i]]+inprod(beta,X[i,])
    }
    tau.Y <- 1/pow(YSD,2)
    YSD ~ dgamma(sh.Y,ra.Y)
    # dgamma parameterized by mode (m) and standard deviation
    sh.Y <- 1+m.Y*ra.Y
    ra.Y <- (m.Y+sqrt(m.Y^2+4*sd.Y^2))/(2*sd.Y^2)
    m.Y ~ dunif(0,100)
    sd.Y ~ dunif(0,100)

    for(k in 1:nEff){
      beta[k] ~ dnorm(0,0.0001)
    }
    
    for(s in 1:S){
      a[s] ~ dnorm(a.hat[s],aTau)
      a.hat[s] <- g.0+g.1*VID[s]
    }
    
    aTau <- 1/pow(aSD,2)
    aSD ~ dgamma(sh,ra)
    # dgamma parameterized by mode (m) and standard deviation
    sh <- 1+m*ra
    ra <- (m+sqrt(m^2+4*sd^2))/(2*sd^2)
    m ~ dunif(0,100)
    sd ~ dunif(0,100)
    g.0 ~ dnorm(0,0.0001)
    g.1 ~ dnorm(0,0.0001)
    }
"

## parameters
params = c('g.0','g.1','beta')
