# rm(list=ls())
library(data.table)
library(lubridate)
library(car)
library(jagsUI)
library(lme4)
library(tidyr)
library(doBy)
library(plyr)
library(dplyr)

dat <- fread('var_comp_data.csv')
dat[, .N]
head(dat)
summary(dat)

# Subset out just response of interest
dat <- dat[!is.na(median_tp),]
dat[, .N]
summary(dat)

# 27 lakes without land use
# t1 <- dat[is.na(iws_urban),]
# length(unique(t1$lagoslakeid))

# Remove observations with NA covariates
dat <- dat[!is.na(iws_urban)]
dat <- dat[!is.na(hu12_dep_totaln_1990_mean)]


# Remove outliers
dat <- dat[median_tp > 0.015,]

# hist(log(dat$median_tp))

# log_transform response
dat[, log_y:= log(median_tp)]

# Retain HU4s with >= 5 obs
tbl <- table(dat$hu4_zoneid)
dat <- droplevels(dat[dat$hu4_zoneid %in% names(tbl)[tbl >= 5],,drop=FALSE])

# Convert HU4 id to numeric
dat[, hu4_zoneidN:= as.numeric(as.factor(as.numeric(as.factor(hu4_zoneid))))]

# Create new variable that is lake and year combined
dat[, lakeyear:= paste0(lagoslakeid,sampleyear)]

##################
## Palmer data
palm <- fread("PHDIData_forTy.csv")
head(palm)
dim(palm)

# A few (~100) lakes lack palmer data, so we only want lakes in dat with palmer data
dat <- dat[dat$lagoslakeid %in% palm$lagoslakeid,]
dim(dat)
length(unique(dat$lagoslakeid))

# A few lakes lack predicted depth data from Oliver et al, so only want lakes in dat with predicted depth dat
### Bring in Oliver et al.'s predicted depth data
depth.dat <- fread("Sarah_depth_data.csv")
# Select lakes in depth.dat that are in dat
dat <- dat[dat$lagoslakeid %in% depth.dat$lagoslakeid]
dim(dat)

# Rename sampleyear for use below
dat[,Year:=sampleyear]
length(unique(dat$Year))

# Grab colmuns of interest
palm <- palm[, c('lagoslakeid','year','palmer.spring'), with=FALSE]
palm[,Year:=year]


length(unique(dat$lagoslakeid))
length(unique(dat$hu4_zoneid))
length(unique(dat$hu12_zoneid))
length(unique(dat$sampleyear))
length(unique(dat$lakeyear))
length(unique(dat$lagoslakeid)) * length(unique(dat$sampleyear))
dat[,.N, by=hu4_zoneid][order(hu4_zoneid)]


# Grab predicted depth data
depth.dat2 <- depth.dat[depth.dat$lagoslakeid %in% dat$lagoslakeid]
dim(depth.dat2)
head(depth.dat2)

# Merge predicted depth with dat
# Set keys for merging
setkey(dat, lagoslakeid)
setkey(depth.dat2, lagoslakeid)

dat <- merge(dat, depth.dat2)

# Create new depth variable
dat[, depth_new:= ifelse(is.na(maxdepth), zmaxpredict, maxdepth)]

## Sort by HU4 ID
# dat <- dat[order(hu4_zoneid)]
################################
###  Covariate processing  #####
################################
# Lake-level predictors
# dat[, z_wala:= as.numeric(scale(log(wa_la)))]
# dat[, z_area:= as.numeric(scale(log(lake_area_ha)))]
# dat[, z_iws_urban:= as.numeric(scale(logit(iws_urban)))]
# dat[, z_iws_ag:= as.numeric(scale(logit(iws_ag)))]
# dat[, z_iws_pasture:= as.numeric(scale(logit(iws_pasture)))]
# dat[, z_iws_rowcrop:= as.numeric(scale(logit(iws_rowcrop)))]
# dat[, z_iws_total_forest:= as.numeric(scale(logit(iws_total_forest)))]
# dat[, z_iws_deciduous_forest:= as.numeric(scale(logit(iws_deciduous_forest)))]
# dat[, z_iws_evergreen_forest:= as.numeric(scale(logit(iws_evergreen_forest)))]
# dat[, z_iws_mixed_forest:= as.numeric(scale(logit(iws_mixed_forest)))]
# dat[, z_iws_woody_wetland:= as.numeric(scale(logit(iws_woody_wetland)))]
# dat[, z_iws_emergent_wetland:= as.numeric(scale(logit(iws_emergent_wetland)))]
# dat[, z_iws_total_wetland:= as.numeric(scale(logit(iws_total_wetland)))]
# dat[, z_iws_streamdensity_headwaters:= as.numeric(scale(log(iws_streamdensity_headwaters_density_mperha + 0.01)))]
# dat[, z_iws_iws_streamdensity:= as.numeric(scale(log(iws_streamdensity_streams_density_mperha + 0.01)))]
# dat[, z_iws_roaddensity:= as.numeric(scale(log(iws_roaddensity_density_mperha + 0.01)))]
# dat[,z_depth:=as.numeric(scale(log(depth_new)))]

### Obtain 1 value per lake
z_wala <- summaryBy(wa_la ~ lagoslakeid, data=dat, FUN=mean)
z_wala$z_wala <- as.numeric(scale(log(z_wala$wa_la.mean)))
z_area <- summaryBy(lake_area_ha ~ lagoslakeid, data=dat, FUN=mean)
z_area$z_area<- as.numeric(scale(log(z_area$lake_area_ha.mean)))
z_iws_urban <- summaryBy(iws_urban ~ lagoslakeid, data=dat, FUN=mean) 
z_iws_urban$z_iws_urban <- as.numeric(scale(logit(z_iws_urban$iws_urban.mean)))
z_iws_ag <- summaryBy(iws_ag ~ lagoslakeid, data=dat, FUN=mean)
z_iws_ag$z_iws_ag <- as.numeric(scale(logit(z_iws_ag$iws_ag.mean)))
z_iws_pasture <- summaryBy(iws_pasture ~ lagoslakeid, data=dat, FUN=mean)
z_iws_pasture$z_iws_pasture <- as.numeric(scale(logit(z_iws_pasture$iws_pasture.mean)))
z_iws_rowcrop <- summaryBy(iws_rowcrop ~ lagoslakeid, data=dat, FUN=mean)
z_iws_rowcrop$z_iws_rowcrop <- as.numeric(scale(logit(z_iws_rowcrop$iws_rowcrop.mean)))
z_iws_total_forest <- summaryBy(iws_total_forest ~ lagoslakeid, data=dat, FUN=mean) 
z_iws_total_forest$z_iws_total_forest <- as.numeric(scale(logit(z_iws_total_forest$iws_total_forest.mean)))
z_iws_deciduous_forest <- summaryBy(iws_deciduous_forest ~ lagoslakeid, data=dat, FUN=mean) 
z_iws_deciduous_forest$z_iws_deciduous_forest <- as.numeric(scale(logit(z_iws_deciduous_forest$iws_deciduous_forest.mean)))
z_iws_evergreen_forest <- summaryBy(iws_evergreen_forest ~ lagoslakeid, data=dat, FUN=mean) 
z_iws_evergreen_forest$z_iws_evergreen_forest <- as.numeric(scale(logit(z_iws_evergreen_forest$iws_evergreen_forest.mean))) 
z_iws_mixed_forest <- summaryBy(iws_mixed_forest ~ lagoslakeid, data=dat, FUN=mean)
z_iws_mixed_forest$z_iws_mixed_forest <- as.numeric(scale(logit(z_iws_mixed_forest$iws_mixed_forest.mean)))
z_iws_woody_wetland <- summaryBy(iws_woody_wetland ~ lagoslakeid, data=dat, FUN=mean) 
z_iws_woody_wetland$z_iws_woody_wetland <- as.numeric(scale(logit(z_iws_woody_wetland$iws_woody_wetland.mean)))
z_iws_emergent_wetland <- summaryBy(iws_emergent_wetland ~ lagoslakeid, data=dat, FUN=mean) 
z_iws_emergent_wetland$z_iws_emergent_wetland <- as.numeric(scale(logit(z_iws_emergent_wetland$iws_emergent_wetland.mean)))
z_iws_total_wetland <- summaryBy(iws_total_wetland ~ lagoslakeid, data=dat, FUN=mean) 
z_iws_total_wetland$z_iws_total_wetland <- as.numeric(scale(logit(z_iws_total_wetland$iws_total_wetland.mean)))
z_iws_streamdensity_headwaters <- summaryBy(iws_streamdensity_headwaters_density_mperha ~ lagoslakeid, data=dat, FUN=mean) 
z_iws_streamdensity_headwaters$z_iws_streamdensity_headwaters <- as.numeric(scale(log(z_iws_streamdensity_headwaters$iws_streamdensity_headwaters_density_mperha.mean+ 0.01)))
z_iws_iws_streamdensity <- summaryBy(iws_streamdensity_streams_density_mperha ~ lagoslakeid, data=dat, FUN=mean) 
z_iws_iws_streamdensity$z_iws_iws_streamdensity <- as.numeric(scale(log(z_iws_iws_streamdensity$iws_streamdensity_streams_density_mperha.mean+ 0.01)))
z_iws_roaddensity <- summaryBy(iws_roaddensity_density_mperha ~ lagoslakeid, data=dat, FUN=mean)
z_iws_roaddensity$z_iws_roaddensity <- as.numeric(scale(log(z_iws_roaddensity$iws_roaddensity_density_mperha.mean+ 0.01)))
z_depth <- summaryBy(depth_new ~ lagoslakeid, data=dat, FUN=mean)
z_depth$z_depth <- as.numeric(scale(log(z_depth$depth_new.mean)))


### HUC 4 level predictors
# Average by HU4 and standardize
z_hu4_area <- summaryBy(lake_area_ha ~ hu4_zoneid, data=dat, FUN=mean) 
z_hu4_area <- z_hu4_area[,lake_area_ha.mean:=scale(lake_area_ha.mean)]
z_hu4_runoff <- summaryBy(hu4_runoff_mean ~ hu4_zoneid, data=dat, FUN=mean) 
z_hu4_runoff <- z_hu4_runoff[,hu4_runoff_mean.mean:=scale(hu4_runoff_mean.mean)]
z_hu4_baseflow <- summaryBy(hu4_baseflowindex_mean ~ hu4_zoneid, data=dat, FUN=mean)   
z_hu4_baseflow <- z_hu4_baseflow[, hu4_baseflowindex_mean.mean:=scale(hu4_baseflowindex_mean.mean)]
z_hu4_ptt <- summaryBy(hu4_prism_ppt_30yr_normal_800mm2_annual_mean ~ hu4_zoneid, data=dat, FUN=mean)   
z_hu4_ptt <- z_hu4_ptt[, hu4_prism_ppt_30yr_normal_800mm2_annual_mean.mean:=scale(hu4_prism_ppt_30yr_normal_800mm2_annual_mean.mean)]
z_hu4_temp <- summaryBy(hu4_prism_tmean_30yr_normal_800mm2_annual_mean ~ hu4_zoneid, data=dat, FUN=mean) 
z_hu4_temp <- z_hu4_temp[, hu4_prism_tmean_30yr_normal_800mm2_annual_mean.mean:=scale(hu4_prism_tmean_30yr_normal_800mm2_annual_mean.mean)]
z_hu4_TN_1990 <- summaryBy(hu4_dep_totaln_1990_mean ~ hu4_zoneid, data=dat, FUN=mean)    
z_hu4_TN_1990 <- z_hu4_TN_1990[, hu4_dep_totaln_1990_mean.mean:=scale(hu4_dep_totaln_1990_mean.mean)]
z_hu4_TN_2010 <- summaryBy(hu4_dep_totaln_2010_mean ~ hu4_zoneid, data=dat, FUN=mean)   
z_hu4_TN_2010 <- z_hu4_TN_2010[, hu4_dep_totaln_2010_mean.mean:=scale(hu4_dep_totaln_2010_mean.mean)]
z_hu4_perIsoLakes <- summaryBy(proportion_isolated_headwater ~ hu4_zoneid, data=dat, FUN=mean) 
z_hu4_perIsoLakes[,  proportion_isolated_headwater.mean:=scale(proportion_isolated_headwater.mean)]

z_hu4_wetland <- summaryBy( huc4_total_wetland ~ hu4_zoneid, data=dat, FUN=mean) 
z_hu4_wetland <- z_hu4_wetland[,huc4_total_wetland.mean:=scale(huc4_total_wetland.mean)]
z_hu4_urban <- summaryBy( huc4_urban ~ hu4_zoneid, data=dat, FUN=mean) 
z_hu4_urban <- z_hu4_urban[,huc4_urban.mean:=scale(huc4_urban.mean)]
z_hu4_ag <- summaryBy( huc4_ag ~ hu4_zoneid, data=dat, FUN=mean) 
z_hu4_ag <- z_hu4_ag[,huc4_ag.mean:=scale(huc4_ag.mean)]
z_hu4_forest <- summaryBy( huc4_total_forest ~ hu4_zoneid, data=dat, FUN=mean) 
z_hu4_forest <- z_hu4_forest[,huc4_total_forest.mean:=scale(huc4_total_forest.mean)]



# z_hu4_TN_2010$test <- as.numeric(as.factor(as.numeric(as.factor(z_hu4_TN_2010$hu4_zoneid))))

######################################################
##### HU4-specific slopes (change) in N deposition ###
# Grab data 
Ndat <- dat[, c('hu4_zoneid','hu4_dep_totaln_1990_mean','hu4_dep_totaln_1995_mean','hu4_dep_totaln_2000_mean',
                'hu4_dep_totaln_2005_mean','hu4_dep_totaln_2010_mean'), with=FALSE]

# Grab 1st row for each HU4 - obtain one value per HU4
Ndat <- Ndat[, head(.SD, 1), by = "hu4_zoneid"]
# Rename columns
colnames(Ndat) <- c('hu4_zoneid', "1990","1995","2000","2005","2010")

# Use tidyr library to convert from wide to long format
keycol <- "year" # name of new column
valuecol <- "N_dep" # name of new value column
gathercols <- c("1990", "1995", "2000","2005","2010") # source columns with data

Ndat2 <- gather_(Ndat, keycol, valuecol, gathercols)
# Sort by HU4 ID
Ndat2 <- Ndat2[order(Ndat2$hu4_zoneid),]
Ndat2$year <- as.numeric(Ndat2$year)
Ndat2$yearc <- Ndat2$year - mean(Ndat2$year)

# Fit varying int/slope model to get each HU4's slopes (trends in N dep)
m1 <- lmer(log(Ndat2$N_dep) ~ 1 + yearc + (1+yearc|hu4_zoneid), data=Ndat2)
summary(m1)
# Grab HU4 slopes
modCoef <- coef(m1)
slopes <- modCoef$"hu4_zoneid"[,'yearc']

Ndat2 <- data.table(Ndat2)
Ndat3 <- Ndat2[, head(.SD, 1), by = "hu4_zoneid"]

####### Merge slopes with HU4 IDs
Nslopes <- data.frame(Ndat3$hu4_zoneid, slopes)
Nslopes$z_slopes <- scale(Nslopes$slopes)

#####################################################
###### Precip - lake-by-HU12 predictors #######
c1 <- fread("hu12_ptt_summaries.csv")
head(c1)

# str(c1)
dim(c1)
length(unique(c1$ZoneID))
length(unique(c1$Year))

# table(c1$ZoneID)
# range(table(c1$ZoneID))

# range(c1$Year)

# Grab HUC12's that are in dat
c1 <- c1[c1$ZoneID %in% dat$hu12_zoneid,]
dim(c1)
length(unique(dat$hu12_zoneid))
length(unique(c1$ZoneID))

table(c1$ZoneID,c1$Year)
min(table(c1$ZoneID))

# Grab years of interest
c2 <- c1[Year >= 1990 & (Year <= 2011)]
summary(c2)
dim(c2)
length(unique(c2$Year)) * length(unique(c2$ZoneID))

# Standardize climate covariates
cols <- colnames(c2) # grab column names to standardize
cols <- cols[3:18]
z_c2 <- c2[ , (cols) := lapply(.SD, scale), .SDcols = cols]

###############################################
# Assign HU12 values to each lagoslakeid
# Grab first lakyear obesvation to remove within-year samples - just need one sample per year for this merge
dat_2 <- dat[, head(.SD, 1), by = "lakeyear"]
# length(unique(dat$lakeyear))

# z_c2[, hu12_zoneid:= ZoneID]
# #------- Merge HU12 climate data with lagos dat
# dat2 <- plyr::join(dat, z_c2, by=c("hu12_zoneid","Year"), type='left', match='all')
# dim(dat)
# dim(dat2)
# # write.csv(test,'test.csv')
# length(unique(dat$lagoslakeid))
# length(unique(dat2$lagoslakeid))
# length(unique(dat2$Year))
# # table(dat2$lagoslakeid)

# Fill in all years for each lakeid
all <- data.frame(dat_2 %>% expand(lagoslakeid, Year))
dim(all)
head(all)
# 
# dat_all <- full_join(dat2, all)
# dim(dat_all)
# dat_all <- plyr::join(dat2, all, by=c("lagoslakeid","Year"), type='full', match='all')

# Use right_join to add in the appropriate missing values to the
# original data
dat_all <- dat_2 %>% right_join(all)
dim(dat_all)
head(dat_all)
# test <- dat_all[!is.na(dat_all$huc4_evergreen_forest),]
# dim(test)
# dim(dat_2)

## Add in hu12_zoneid for all years based on lakeid
ids <- dat[, c("hu12_zoneid", "lagoslakeid"), with=FALSE]
# Get one value per lakeid
ids2 <- ids[, head(.SD, 1), by = "lagoslakeid"]
head(ids2)
dim(ids2)

dat_all2 <- plyr::join(dat_all, ids2, by=c("lagoslakeid"), type='left', match='all')
dim(dat_all2)
summary(dat_all2)
head(dat_all2)
# Remove hu12_zoneid with NAs
dat_all2 <- data.table(dat_all2)

dat_all2 <- dat_all2[,(5):=NULL]

## Merge standardized climate data with dat_all2 in order to get hu12 climate data assigned to each lakeid and year
z_c2[, hu12_zoneid:=ZoneID] 
dat_all3 <- plyr::join(dat_all2, z_c2, by=c("hu12_zoneid","Year"), type='left', match='all')
dim(dat_all3)
head(dat_all3)
#####################################################
# Index climate covariates by sites (J) and years (Y)

nYears <- length(unique(dat_all3$Year))
# range(z_c1$Year)
nSites <- length(unique(dat_all3$lagoslakeid))
nYears * nSites

#################################
#### Covariates in matrix form ##
#################################
##### Current year's May temp
may_ptt <- matrix(dat_all3$May, nrow=nSites, ncol=nYears, byrow=T)

# Previous year's Dec, Jan, Nov, Winter precip
# Grab years of interest
c3 <- c1[Year >= 1989 & (Year <= 2010)]
summary(c3)
dim(c3)

cols <- colnames(c3) # grab column names to standardize
cols <- cols[3:18]
z_c3 <- c3[ , (cols) := lapply(.SD, scale), .SDcols = cols]

# Rename z_c3 columns with "prev" to indicate these are lagged (previous year's) data
colnames(z_c3)[3:18] <- paste("prev_", colnames(z_c3[,c(3:18)]), sep = "")

# Rename year value so it merges with data_all3 (add 1 to each year)
z_c3[, Year:=Year+1]

## Merge with dat_all3 to get on value for each lake year

z_c3[, hu12_zoneid:=ZoneID] 
dat_all3 <- plyr::join(dat_all3, z_c3, by=c("hu12_zoneid","Year"), type='left', match='all')
dim(dat_all3)
head(dat_all3)
summary(dat_all3)

### PREVIOUS YEARS PREDICTORS
prev_dec_ptt <- matrix(dat_all3$prev_December, nrow=nSites, ncol=nYears, byrow=T)
prev_jan_ptt <- matrix(dat_all3$prev_January, nrow=nSites, ncol=nYears, byrow=T)
prev_nov_ptt <- matrix(dat_all3$prev_November, nrow=nSites, ncol=nYears, byrow=T)
prev_winter_ptt <- matrix(dat_all3$prev_winter_ptt, nrow=nSites, ncol=nYears, byrow=T)


#####################################################
###### Temp - lake-by-year predictors #######
t1 <- fread("hu12_temp_summaries.csv")
head(t1)

# str(t1)
dim(t1)
length(unique(t1$ZoneID))
length(unique(t1$Year))

table(t1$ZoneID)
range(table(t1$ZoneID))
range(t1$Year)

# Grab HUC12's that are in dat
t1 <- t1[t1$ZoneID %in% dat$hu12_zoneid,]
dim(t1)
length(unique(dat$hu12_zoneid))
length(unique(t1$ZoneID))

table(t1$ZoneID)
min(table(t1$ZoneID))

# Grab years of interest
t2 <- t1[Year >= 1990 & (Year <= 2011)]
summary(t2)
dim(t2)

# Standardize climate covariates
cols <- colnames(t2) # grab column names to standardize
cols <- cols[3:18]
z_t2 <- t2[ , (cols) := lapply(.SD, scale), .SDcols = cols]

# Re-name columns with "temp"
colnames(z_t2)[3:18] <- paste("temp_", colnames(z_t2[,c(3:18)]), sep = "")

z_t2[, hu12_zoneid:=ZoneID] 
dat_all3 <- plyr::join(dat_all3, z_t2, by=c("hu12_zoneid","Year"), type='left', match='all')
dim(dat_all3)
head(dat_all3)
summary(dat_all3)



# Index climate covariates by sites (J) and years (Y)
#################################
#### Covariates in matrix form ##
#################################
# Current year's May & June temp
may_temp <- matrix(dat_all3$temp_May, nrow=nSites, ncol=nYears, byrow=T)
june_temp <- matrix(dat_all3$temp_June, nrow=nSites, ncol=nYears, byrow=T)



## PALMER DATA
head(palm)
# Grab years of interest
palm2 <- palm[Year >= 1990 & (Year <= 2011)]
summary(palm2)
dim(palm2)

palm2 <- palm2[ palm2$lagoslakeid %in% dat$lagoslakeid,]
# length(unique(palm2$Year))
# length(unique(palm2$lagoslakeid))


## Covariate matrix
spring_palm <- matrix(as.numeric(scale(palm2$palmer.spring)), nrow=nSites, ncol=nYears, byrow=T)


###### Coherent temporal predictors ###########
## Average annual November - May precipitation across all HUC12s
cc3 <- c1[Year >= 1989 & (Year <= 2011)]
summary(cc3)
cc3[, coherent_ptt := (shift(November, 1L, type="lag") + shift(December, 1L, type="lag") + January +
                     February + March + April + May)/7]
# Average across HUCs for each year
coherent_ptt <- cc3[, lapply(.SD, mean), by=Year, .SDcols=c("coherent_ptt") ] 

coherent_ptt <- as.matrix(coherent_ptt)
# Remove year 1989
coherent_ptt <- coherent_ptt[-1,2]
# Standardize
z_coherent_ptt <- as.numeric(scale(coherent_ptt))

## July - Sept mean annual temperature across all HU12s
# Grab years of interest
tc2 <- t1[Year >= 1990 & (Year <= 2011)]
summary(tc2)
tc2[, coherent_temp := (July + August + September)/3]
# Average across HUCs for each year
coherent_temp <- tc2[, lapply(.SD, mean), by=Year, .SDcols=c("coherent_temp") ] 
coherent_temp <- as.matrix(coherent_temp[,2])
# Standardize
z_coherent_temp <- as.numeric(scale(coherent_temp))

## Annual average spring Palmer
palm2c <- palm[Year >= 1990 & (Year <= 2011)]
# Average across lakes for each year
coherent_palm <- palm2c[, lapply(.SD, mean), by=Year, .SDcols=c("palmer.spring") ] 
# Standardize
z_coherent_palm <- as.numeric(scale(coherent_palm[,2]))



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
    u1[j] ~ dnorm(lake.hat[j],tau.lake)
      lake.hat[j] <- a[1] * x1[j] + a[2] * x2[j] + a[3] * x3[j]+a[4] * x4[j]+a[5] * x5[j]+a[6] * x6[j]+a[7] * x7[j]+a[8] * x8[j]+
                     a[9] * x9[j]+a[10] * x10[j]+a[11] * x11[j]+a[12] * x12[j]+a[13] * x13[j]+a[14] * x14[j]+a[15] * x15[j]  
                      
    }
    
    # Level-1 predictors
    # Bayesian LASSO -  a Laplace (double exponential) prior
    for(k in 1:39){
        a[k] ~ ddexp(0, lambda1)
    }

# Hyper-prior on lambda
    lambda1 ~ dexp(10)
    # lambda1 ~ dgamma(0.1,0.1)

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


head(dat)

lake <- as.numeric(as.factor(as.numeric(dat$lagoslakeid)))
year <- as.numeric(as.factor(as.numeric(dat$sampleyear)))
huc <- as.numeric(as.factor(as.numeric(as.factor(dat$hu4_zoneid))))
J <- length(unique(lake))
H <- length(unique(huc))
Y <- length(unique(year))


# Load data (remove headwater stream density)
data <- list(y = dat$log_y, lake = lake, year = year, huc = huc, n = dim(dat)[1], J = J, H = H, Y = Y,
             x1=z_wala$z_wala,x2=z_area$z_area,x3=z_iws_urban$z_iws_urban,x4=z_iws_ag$z_iws_ag,x5=z_iws_pasture$z_iws_pasture,
             x6=z_iws_rowcrop$z_iws_rowcrop,x7= z_iws_total_forest$z_iws_total_forest,x8=z_iws_deciduous_forest$z_iws_deciduous_forest,
             x9=z_iws_evergreen_forest$z_iws_evergreen_forest,x10=z_iws_mixed_forest$z_iws_mixed_forest,x11=z_iws_woody_wetland$z_iws_woody_wetland,
             x12=z_iws_emergent_wetland$z_iws_emergent_wetland,
             x13=z_iws_iws_streamdensity$z_iws_iws_streamdensity,x14=z_depth$z_depth,x15=z_iws_roaddensity$z_iws_roaddensity,
             hx1=as.numeric(z_hu4_area$lake_area_ha.mean), hx2=as.numeric(z_hu4_runoff$hu4_runoff_mean.mean),
             hx3=as.numeric(z_hu4_baseflow$hu4_baseflowindex_mean.mean),hx4=as.numeric(z_hu4_ptt$hu4_prism_ppt_30yr_normal_800mm2_annual_mean.mean),
             hx5=as.numeric(z_hu4_temp$hu4_prism_tmean_30yr_normal_800mm2_annual_mean.mean),hx6=as.numeric(z_hu4_TN_1990$hu4_dep_totaln_1990_mean.mean),
             hx7=as.numeric(z_hu4_TN_2010$hu4_dep_totaln_2010_mean.mean),hx8=as.numeric(Nslopes$z_slopes),hx9=as.numeric(z_hu4_perIsoLakes$proportion_isolated_headwater.mean),
             hx10=as.numeric(z_hu4_wetland$huc4_total_wetland.mean),hx11=as.numeric(z_hu4_urban$huc4_urban.mean),
             hx12=as.numeric(z_hu4_ag$huc4_ag.mean), hx13=as.numeric(z_hu4_forest$huc4_total_forest.mean),
             lyx1=may_ptt, lyx2=prev_dec_ptt, lyx3=prev_jan_ptt,lyx4=prev_nov_ptt,lyx5=prev_winter_ptt,lyx6=may_temp,lyx7=june_temp,lyx8=spring_palm,
             y1=z_coherent_ptt, y2=z_coherent_temp, y3=z_coherent_palm)

# Headwater stream density and stream density also highly correlated (r=0.99)-so removed headwater density
# Woody wetland and emergent wetland highly correlated (r = 0.95)
# Remove on of them - total wetland removed
# cor(cbind(z_iws_woody_wetland$z_iws_woody_wetland.mean,z_iws_total_wetland$z_iws_total_wetland.mean))
# cor(cbind(z_iws_woody_wetland$z_iws_woody_wetland.mean,z_iws_emergent_wetland$z_iws_emergent_wetland.mean))

# Initial values
inits <- function (){
  list (beta0 = rnorm(1), sigma.e=runif(1), sigma.lake=runif(1),sigma.year=runif(1), 
        sigma.lake.year=runif(1), sigma.huc = runif(1) )
}


# Parameters monitored
parameters <- c("beta0","sigma.e","sigma.lake", "sigma.year", "sigma.lake.year", "sigma.huc",
                "a","lambda1")


# MCMC settings
ni <- 70000
nt <- 1
nb <- 50000
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
saveRDS(mcmcOut, file="TP_mcmc_out_cond_exp.rds")

# sum1 <- out$summary
# write.csv(sum1,"summary_exp.csv", row.names = T)

# traceplot(out,parameters = "a")
# traceplot(out,parameters = "lambda1")
# traceplot(out,parameters = "sigma.year")

