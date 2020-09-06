library(tidyverse)
library(rjags)

# Read in the big data set
final.data.VID <- read.csv("/Volumes/PAY/Political Science/Liz 2020/Data/final-data-VID.csv")

# Create variables to be used as the X in the model
VID.data <- final.data.VID %>%
  mutate(logit.past.rep=log(rep.past/dem.past),
         ALR.white=log((white_pct+0.001)/(100-white_pct-black_pct-hispanic_pct+0.001)),
         ALR.black=log((black_pct+0.001)/(100-white_pct-black_pct-hispanic_pct+0.001)),
         ALR.hispanic=log((hispanic_pct+0.001)/(100-white_pct-black_pct-hispanic_pct+0.001)),
         logit.female=log(female_pct/(100-female_pct)),
         ALR.age29andunder=log(age29andunder_pct/(100-age29andunder_pct-age65andolder_pct)),
         ALR.age65andolder=log(age65andolder_pct/(100-age65andolder_pct-age29andunder_pct)),
         median_hh_inc.st=(median_hh_inc-mean(median_hh_inc))/sd(median_hh_inc),
         logit.unemploy=log((clf_unemploy_pct+0.001)/(100.001-clf_unemploy_pct)),
         logit.lesshs=log(lesshs_pct/(100-lesshs_pct)),
         logit.rural=log((rural_pct+0.001)/(100.001-rural_pct)))
X <- VID.data %>%
  select(logit.past.rep:logit.rural)

voterID.state <- final.data.VID %>%
  group_by(state_code) %>%
  summarize(voterID.st=mean(voterID))

# Read in the model
#source("/Volumes/PAY/Political Science/Liz 2020/Programs/model-normal.R")
#source("/Volumes/PAY/Political Science/Liz 2020/Programs/model-cauchy.R")


# put data in a list
datal <- list(Y=VID.data$Y,
              X=X,
              n=nrow(X),
              nEff=ncol(X),
              id=VID.data$state_code,
              S=length(unique(VID.data$state_code)),
              VID=voterID.state$voterID.st)

# settings
nBurn <- 1000
nChains <- 4
nSave <- 1000
# nThin <- 200 - for normal and cauchy
nThin <- 500
nIter <- ceiling((nSave*nThin)/nChains)

# Initial guess
#library(lme4)

#mle.init <- lmer(Y~(1|state_code)+(voterID|state_code)+logit.past.rep+ALR.white
                # +ALR.black+ALR.hispanic+logit.female+ALR.age29andunder
                # +ALR.age65andolder+median_hh_inc.st+logit.unemploy+logit.lesshs
                # +logit.rural,data = VID.data) 
#b <- coefficients(mle.init)
#initial.parm.guess <- b$state_code %>%
#  summarize_if(is.numeric,mean)
mle.test <- lm(Y~voterID+logit.past.rep+ALR.white+ALR.black+ALR.hispanic
               +logit.female+ALR.age29andunder+ALR.age65andolder+median_hh_inc.st
               +logit.unemploy+logit.lesshs+logit.rural,data = VID.data)
initial.parm.guess <- unname(coefficients(mle.test))
initial.list <- list(g.0=initial.parm.guess[1],
                     g.1=initial.parm.guess[2],
                     beta=c(initial.parm.guess[3],initial.parm.guess[4],initial.parm.guess[5],
                            initial.parm.guess[6],initial.parm.guess[7],initial.parm.guess[8],
                            initial.parm.guess[9],initial.parm.guess[10],initial.parm.guess[11],
                            initial.parm.guess[12],initial.parm.guess[13]),
                     nu=4)
  
# create JAGS model
mod <- jags.model(textConnection(modelString),data=datal,inits=initial.list,
                  n.chains=nChains)

# burn in
update(mod,n.iter=nBurn)

# MCMC samples
samp <- coda.samples(model=mod,variable.names=params,n.iter=nIter,thin=nThin)

# Model Deviance
dic.check <- dic.samples(mod,1000)
dic.check

# check posterior
gelman.diag(samp)
effectiveSize(samp)
source("/Volumes/PAY/Courses/MAT350/Doing Bayesian Data Analysis/Programs/DBDA2E-utilities.R")
diagMCMC(codaObject=samp,parName="g.0") 
diagMCMC(codaObject=samp,parName="g.1") 
diagMCMC(codaObject=samp,parName="beta[1]") 
diagMCMC(codaObject=samp,parName="beta[2]") 
diagMCMC(codaObject=samp,parName="beta[3]") 
diagMCMC(codaObject=samp,parName="beta[4]") 
diagMCMC(codaObject=samp,parName="beta[5]") 
diagMCMC(codaObject=samp,parName="beta[6]") 
diagMCMC(codaObject=samp,parName="beta[7]") 
diagMCMC(codaObject=samp,parName="beta[8]") 
diagMCMC(codaObject=samp,parName="beta[9]") 
diagMCMC(codaObject=samp,parName="beta[10]") 
diagMCMC(codaObject=samp,parName="beta[11]") 

# summary of posterior
summary(samp)

# extract to matrix
mcmc <- as.matrix(samp,chains=T)

# Create vote share for GOP
mean.X <- X %>%
  summarize_if(is.numeric,mean)
  
mu.VID <- mcmc[,13]+mcmc[,14]
for(i in 1:11){
  mu.VID <- mu.VID+(mcmc[1,i+1]*mean.X[1,i])
}

mu.noVID <- mcmc[,13]
for(i in 1:11){
  mu.noVID <- mu.noVID+(mcmc[1,i+1]*mean.X[1,i])
}

# Expected vote share for GOP when VID is present
p.VID <- exp(mu.VID)/(1+exp(mu.VID))

# Expected vote share for GOP when VID is no present
p.noVID <- exp(mu.noVID)/(1+exp(mu.noVID))

# Expected vote share for GOP due to VID
GOP.share.VID <- p.VID-p.noVID

summary(p.VID)
sd(p.VID)
quantile(p.VID,c(0.025,0.975))
summary(p.noVID)
sd(p.noVID)
quantile(p.noVID,c(0.025,0.975))
summary(GOP.share.VID)
sd(GOP.share.VID)
quantile(GOP.share.VID,c(0.025,0.975))
sum(GOP.share.VID>0)/length(GOP.share.VID)

# Output results to CSV file
write.csv(mcmc,"/Volumes/PAY/Political Science/Liz 2020/Programs/Results/mcmc-cauchy.csv",row.names=FALSE)
write.csv(GOP.share.VID,"/Volumes/PAY/Political Science/Liz 2020/Programs/Results/GOP-share-cauchy.csv",row.names=FALSE)