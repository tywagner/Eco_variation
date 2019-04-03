# rm(list=ls())

library(jagsUI)


#################################################################
########## BUGS CODE ############################################
#################################################################

# Define the model in the BUGS language and write a text file
sink("vc1_horse.txt")
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
    u1[j] ~ dnorm(lake.hat[j],tau.lake)
      lake.hat[j] <- a[1] * x1[j] + a[2] * x2[j] + a[3] * x3[j]+a[4] * x4[j]+a[5] * x5[j]+a[6] * x6[j]+a[7] * x7[j]+a[8] * x8[j]+
                     a[9] * x9[j]+a[10] * x10[j]+a[11] * x11[j]+a[12] * x12[j]+a[13] * x13[j]+a[14] * x14[j]+a[15] * x15[j]  
                      
    }
    
# Formulation of the horseshoe prior
 for(i in 1:39) {
    a[i] ~ dnorm(0,prec.b[i])
    prec.b[i] <- 1/(s.b[i]*s.b[i]) 
    s.b[i] <- global.b*local.b[i]
    local.b[i] ~ dnorm(0,v.b[i])T(0,)
    v.b[i] ~ dgamma(0.5,0.5)
    }
    global.b ~ dnorm(0,gamma)T(0,)
    gamma ~ dgamma(0.5,0.5)


    # Random HU4 effect
    for(h in 1:H){
    u4[h] ~ dnorm(hu4.hat[h],tau.huc)
      hu4.hat[h] <- a[16] * hx1[h] + a[17] * hx2[h] + a[18] * hx3[h]+ a[19] * hx4[h]+ a[20] * hx5[h]+ a[21] * hx6[h]+ 
          a[22] * hx7[h]+ a[23] * hx8[h] + a[24] * hx9[h] + a[25] * hx10[h] + a[26] * hx11[h] + a[27] * hx12[h] + a[28] * hx13[h]
    }
    

    # Random year effect
    for(y in 1:Y){
      u2[y] ~ dnorm(y.hat[y],tau.year)
        y.hat[y] <- a[37] * y1[y] + a[38] * y2[y] + a[39] * y3[y] 
    }
    
    # loop over lakes and years - random ephemeral temporal effect
    for (j in 1:J){
      for (y in 1:Y){
        u3[j, y] ~ dnorm(ly.hat[j,y], tau.lake.year) 
          ly.hat[j,y] <- a[29]*lyx1[j,y]+a[30]*lyx2[j,y]+a[31]*lyx3[j,y]+a[32]*lyx4[j,y]+a[33]*lyx5[j,y]+a[34]*lyx6[j,y]+
                        a[35]*lyx7[j,y]+a[36]*lyx8[j,y]
      } 
    } 
    

    # Priors
    beta0 ~ dnorm(0, 0.0001)
    sigma.e ~ dunif(0, 1)
    sigma.lake ~ dunif(0,2)
    sigma.huc ~ dunif(0,2)
    sigma.year ~ dunif(0,1)
    sigma.lake.year ~ dunif(0,1)
    
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
dat <- readRDS('TP_full_data.rds')
str(dat)
attach(dat)



# Initial values
inits <- function (){
  list (beta0 = rnorm(1), sigma.e=runif(1), sigma.lake=runif(1),sigma.year=runif(1), 
        sigma.lake.year=runif(1), sigma.huc = runif(1))
}


# Parameters monitored
parameters <- c("beta0","sigma.e","sigma.lake", "sigma.year", "sigma.lake.year", "sigma.huc",
                "local.b","global.b","a")


# MCMC settings
ni <- 100000
nt <- 1
nb <- 50000
nc <- 3

start.time = Sys.time()         # Set timer 
# Call JAGS from R 
out <- jags(dat, inits, parameters, "vc1_horse.txt", n.chains = nc, 
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
saveRDS(mcmcOut, file="TP_mcmc_out_cond_exp_pred_HORSE.rds")

library(ggmcmc)
library(gridExtra)
library(ggthemes)
library(coda)
out.mcmc <- as.mcmc(out)
S <- ggs(out.mcmc$samples)
# ggs_traceplot(S)
ggmcmc(S, file = "TP_HORSE.pdf")
# For select parameters

# hist(out$sims.list$tau2)


# sum1 <- out$summary
# write.csv(sum1,"summary_exp.csv", row.names = T)

# traceplot(out,parameters = "a")
# traceplot(out,parameters = "lambda1")
# traceplot(out,parameters = "sigma.year")
# 
# dat$lake <- as.numeric(as.factor(as.numeric(dat$lagoslakeid)))
# dat$year <- as.numeric(as.factor(as.numeric(dat$sampleyear)))
# dat$huc <- as.numeric(as.factor(as.numeric(as.factor(dat$hu4_zoneid))))
# write.csv(dat,"tp_dat_diagnostics.csv",row.names = FALSE)
