# rm(list=ls())
library(data.table)
library(lubridate)
library(car)
library(jagsUI)
library(lme4)

dat <- fread('var_comp_data.csv')
dat[, .N]
head(dat)
summary(dat)

dat$hu4_zoneid <- as.factor(dat$hu4_zoneid)

# Subset out just response of interest
dat <- dat[!is.na(median_tp),]
dat[, .N]
summary(dat)

# Remove observations with NA covariates
dat <- dat[!is.na(iws_urban)]
dat <- dat[!is.na(hu12_dep_totaln_1990_mean)]

# Remove outliers
dat <- dat[median_tp > 0.015,]

# hist(log(dat$median_tp))

# log_transform response
dat[, log_y:= log(median_tp)]

# Create new variable that is lake and year combined
# dat[, lakeyear:= paste0(lagoslakeid,sampleyear)]

dat[,.N]
length(unique(dat$lagoslakeid))
length(unique(dat$sampleyear))
length(unique(dat$lakeyear))
length(unique(dat$hu4_zoneid))

# fit model using lmer
m1 <- lmer(log_y ~ 1 + (1|lagoslakeid) +(1|sampleyear) + (1|lagoslakeid:sampleyear) + (1|hu4_zoneid), data=dat)
summary(m1)

write.csv(dat,"TP_dat.csv",row.names = F)

#################################################################
########## BUGS CODE ############################################
#################################################################

# Define the model in the BUGS language and write a text file
sink("vc1.txt")
cat("
    model {
    
    # Likelihood: 
    # Level-1 of the model
    for (i in 1:n){ 
      y[i] ~ dnorm(mu[i], tau)               
      mu[i] <- beta0 + u1[lake[i]] + u2[year[i]] + u3[lake[i],year[i]] + u4[huc[i]]          
    } 
    
    # Random lake effect
    for(j in 1:J){
    u1[j] ~ dnorm(0,tau.lake)
    }

    # Random HU4 effect
    for(h in 1:H){
    u4[h] ~ dnorm(0,tau.huc)
    }
    
    # Random year effect
    for(y in 1:Y){
    u2[y] ~ dnorm(0,tau.year)
    }

    # loop over lakes and years - random ephemeral temporal effect
    for (j in 1:J){
	    for (y in 1:Y){
		    u3[j, y] ~ dnorm(0.0, tau.lake.year) 
        } 
      } 
    
    # Priors
    beta0 ~ dnorm(0, 0.0001)
    sigma.e ~ dunif(0, 5)
    sigma.lake ~ dunif(0,5)
    sigma.huc ~ dunif(0,5)
    sigma.year ~ dunif(0,5)
    sigma.lake.year ~ dunif(0,5)
    
    # Derived quantities
    tau <- pow(sigma.e,-2) 
    tau.lake <- pow(sigma.lake,-2) 
    tau.huc <- pow(sigma.huc,-2) 
    tau.year <- pow(sigma.year,-2) 
    tau.lake.year <- pow(sigma.lake.year,-2)

    
    } # end model
    ",fill = TRUE)
sink()


head(dat)

lake <- as.numeric(as.factor(as.numeric(dat$lagoslakeid)))
year <- as.numeric(as.factor(as.numeric(dat$sampleyear)))
huc <- as.numeric(as.factor(as.numeric(dat$hu4_zoneid)))
J <- length(unique(lake))
H <- length(unique(huc))
Y <- length(unique(year))

# Load data
data <- list(y = dat$log_y, lake = lake, year = year, huc = huc, n = dim(dat)[1], J = J, H = H, Y = Y )


# Initial values
inits <- function (){
  list (beta0 = rnorm(1), sigma.e=runif(1), sigma.lake=runif(1),sigma.year=runif(1), 
        sigma.lake.year=runif(1), sigma.huc = runif(1) )
}


# Parameters monitored
parameters <- c("beta0","sigma.e","sigma.lake", "sigma.year", "sigma.lake.year", "sigma.huc")


# MCMC settings
ni <- 9000
nt <- 1
nb <- 5000
nc <- 3

start.time = Sys.time()         # Set timer 
# Call JAGS from R 
out <- jags(data, inits, parameters, "vc1.txt", n.chains = nc, 
            n.thin = nt, n.iter = ni, n.burnin = nb,  parallel=T)
end.time = Sys.time()
elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
cat('Posterior computed in ', elapsed.time, ' minutes\n\n', sep='') 
# Calculate computation time


# Summarize posteriors
print(out, dig = 3)

### Sometimes you have many, many parameters to examine:
# Find which parameters, if any, have Rhat > 1.1
which(out$summary[, c("Rhat")] > 1.1)

# Or see what max Rhat value is
max(out$summary[, c("Rhat")])

# outExp <- out$summary
# write.csv(outExp, "TP_ModelSummary.csv", row.names = T)
mcmcOut <- out$sims.list
saveRDS(mcmcOut, file="TP_mcmc_out_vc1.rds")

