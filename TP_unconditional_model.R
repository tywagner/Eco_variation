# rm(list=ls())
library(jagsUI)



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

# Read in data
dat <- readRDS('TP_unconditional_data.rds')
str(dat)
attach(dat)


# Initial values
inits <- function (){
  list (beta0 = rnorm(1), sigma.e=runif(1), sigma.lake=runif(1),sigma.year=runif(1), 
        sigma.lake.year=runif(1), sigma.huc = runif(1) )
}


# Parameters monitored
parameters <- c("beta0","sigma.e","sigma.lake", "sigma.year", "sigma.lake.year", "sigma.huc")


# MCMC settings
ni <- 50000
nt <- 1
nb <- 20000
nc <- 3

start.time = Sys.time()         # Set timer 
# Call JAGS from R 
out <- jags(dat, inits, parameters, "vc1.txt", n.chains = nc, 
            n.thin = nt, n.iter = ni, n.burnin = nb,  parallel=T)
end.time = Sys.time()
elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
cat('Posterior computed in ', elapsed.time, ' minutes\n\n', sep='') 
# Calculate computation time


# Summarize posteriors
print(out, dig = 3)


