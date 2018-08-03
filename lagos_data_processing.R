library(LAGOS)
library(data.table)
library(lubridate)


# Compile flat tables
lagos_compile("1.087.0", format = "rds",
              limno_folder = "~/Manuscripts/ContLimno/Data/Version1.087.0",
              geo_folder   = "~/Manuscripts/ContLimno/Data/LAGOS_VER1.04")

# Load data
dt <- lagos_load(version = "1.087.0", format = "rds")
# names(dt)


###### Pull lake nutrient data for variance components paper  
# Criteria
# 1: Include lakes from 1990 - 2011
# 2: Use epi data from summer stratified period (15 June - 15 September)
# 3: Take median nutrient value from each lake within a season (single value/season)
# 4: If TN data don't exits, but TKH and N02N03 do - then calculate TN for those lakes as TKN + N02N03
# 5: Response variables (4): TP, TN, nitrate, Secchi

# Turn epi.nutr data frame into data table
epi <- data.table(dt$epi.nutr)
# class(epi)

# names(epi)

# Number of rows (.N is a in-built variable that holds the number of observations in the current group)
epi[, .N]

# Number of obs per year and sort by N
epi[,.N, by=sampleyear][order(-N)]

# Number of obs per year sorted by year
epi[,.N, by=sampleyear][order(-sampleyear)]

# Select years 2001 - 2011, create new data table
epir <- epi[sampleyear >= 1990 & (sampleyear <= 2011)]
# Check min and max years
epir[,max(sampleyear)]
epir[,min(sampleyear)]
epir[, length(unique(sampleyear))]
# epir[,.N]

# Calculate TN from TKN and N02N03
epir[, tn_calculated:= tkn + no2no3]

# head(epir[!is.na(tn_calculated)],50)

# Subsitute in calculated TN when TN is missing and create new TN variable called tn_new
epir[, tn_new:= ifelse(is.na(tn), tn_calculated, tn)]
# head(epir, 50)

# Format date field (lubridate package)
epir$sampledate <- ymd(epir$sampledate)

# Extract day of year as a new column
epir[, day:= day(sampledate)]
head(epir)


# Number of obs per variable
# epi_sum[, sum(!is.na(tp))]
# epi_sum[, sum(!is.na(tn))]
# epi_sum[, sum(!is.na(secchi))]
# epi_sum[, sum(!is.na(chla))]

# Use epi data from summer stratified period (15 June - 15 September)
epi_sum <- epir[samplemonth == 6 & (day >= 15) | samplemonth==7 | samplemonth==8 | samplemonth== 9 & (day <= 15) ]
# Checking subset
# epi_sum[, max(day), by=samplemonth]
# epi_sum[, min(day), by=samplemonth]
# Toal number of observations
epi_sum[,.N]

# Select out columns on interest
epi_sum2 <- epi_sum[, c('lagoslakeid','chla','tn_new','tp','secchi','no2no3','sampleyear','samplemonth','day'), with=FALSE]
epi_sum2[,.N]


# Obtain one value per lake per year (season) by taking the median value and rename columns as "median"
# Take within-year medians first by lagoslakeid
# epi_sum_ann <- epi_sum2[, .(median_tp=median(tp, na.rm=T), median_tn=median(tn_new, na.rm=T), median_chla=median(chla, na.rm=T),
#                             median_secchi=median(secchi, na.rm=T), median_no2no3=median(no2no3, na.rm=T)), by=.(lagoslakeid, sampleyear)]

# Rename because skipping previous step
epi_sum_ann <- epi_sum2
# Rename variables because of skipped step
epi_sum_ann[,median_tp:=tp]
epi_sum_ann[,median_tn:=tn_new]
epi_sum_ann[,median_secchi:=secchi]
epi_sum_ann[,median_chla:=chla]
epi_sum_ann[,median_no2no3:=no2no3]

# # Take medians across years for final measure
# epi_sum_ann <- epi_sum2[, .(median_tp=median(tp, na.rm=T), median_tn=median(tn_new, na.rm=T), median_chla=median(chla, na.rm=T),
#                             median_secchi=median(secchi, na.rm=T)), by=.(lagoslakeid)]
epi_sum_ann[, .N]
head(epi_sum_ann, 10)

# rm(epi_sum2)



# Number of obs per variable
epi_sum_ann[, sum(!is.na(median_tp))]
epi_sum_ann[, sum(!is.na(median_tn))]
epi_sum_ann[, sum(!is.na(median_secchi))]
epi_sum_ann[, sum(!is.na(median_chla))]
epi_sum_ann[, sum(!is.na(median_no2no3))]

# How many lakes with all 4 variables
# epi_sum_ann[,sum(!is.na(median_tp) & !is.na(median_tn) & !is.na(median_secchi) & !is.na(median_chla))]


######### Merge epi_sum_ann with lake location data and get relevant zoneid's
# help.search("lagoslakes", package = "LAGOS")
# Create new data table
lake_data <- data.table(dt$lagoslakes)
# lake_data[,.N]

# Select out columns on interest
lake_data2 <- lake_data[, c('lagoslakeid','nhd_lat','nhd_long','lake_area_ha', 'meandepth','maxdepth','iws_zoneid',
                          'hu4_zoneid','hu6_zoneid', 'hu8_zoneid','hu12_zoneid','edu_zoneid','lakeconnectivity'), with=FALSE][order(lagoslakeid)]
head(lake_data2)

# Set keys for merging
setkey(epi_sum_ann, lagoslakeid)
setkey(lake_data2, lagoslakeid)

epi_lake <- merge(epi_sum_ann, lake_data2)
epi_lake[,.N]
epi_sum_ann[,.N]

head(epi_lake)

# rm(lake_data2)
## Select out all lakes >= 4 ha
epi_lake <- epi_lake[lake_area_ha >= 4]
summary(epi_lake)
epi_lake[,.N]


##### Select lakes with mean and max depth information
# Select lakes with depth data
# epi_lake <- epi_lake[!is.na(meandepth) | !is.na(maxdepth)]
# epi_lake[,.N]
# summary(epi_lake)
# Checking subset
# epi_lake[is.na(meandepth) & is.na(maxdepth)]
# 
# epi_lake <- epi_lake[meandepth <= 5 | maxdepth <= 7 ]
# epi_lake[,.N] # 3,103 shallow lakes
# summary(epi_lake)

# epi_lake[maxdepth > 30,]

####### Merge epi_lake with watershed (IWS) land use
# help.search("iws.lulc", package = "LAGOS")
# Create new data table
iws_lulc <- data.table(dt$iws.lulc)
# head(iws_lulc)

# Create Developed ("urban") combined measure as a proportion
iws_lulc[, iws_urban:= (iws_nlcd2006_pct_21 + iws_nlcd2006_pct_22 + iws_nlcd2006_pct_23 + iws_nlcd2006_pct_24)/100]
# Total agricultural land use (pature and rowcrop combined)
iws_lulc[, iws_ag:= (iws_nlcd2006_pct_81 + iws_nlcd2006_pct_82)/100]
# Pasture/hay ag
iws_lulc[, iws_pasture:= iws_nlcd2006_pct_81/100]
# Row crop ag
iws_lulc[, iws_rowcrop:= iws_nlcd2006_pct_82/100]
# Deciduos forest
iws_lulc[, iws_deciduous_forest:= iws_nlcd2006_pct_41/100]
# Evergreen forest
iws_lulc[, iws_evergreen_forest:= iws_nlcd2006_pct_42/100]
# Mixed forest
iws_lulc[, iws_mixed_forest:= iws_nlcd2006_pct_43/100]
# Forested land cover (combined)
iws_lulc[, iws_total_forest:= (iws_nlcd2006_pct_41 + iws_nlcd2006_pct_42 + iws_nlcd2006_pct_43)/100]
# Wetlands
# Woody
iws_lulc[, iws_woody_wetland:= iws_nlcd2006_pct_90/100]
# Emergent
iws_lulc[, iws_emergent_wetland:= iws_nlcd2006_pct_95/100]
# Total
iws_lulc[, iws_total_wetland:= (iws_nlcd2006_pct_90 + iws_nlcd2006_pct_95)/100]


# Select out columns of interest from iws_lulc
iws_lulc2 <- iws_lulc[, c('iws_zoneid','iws_urban','iws_ag','iws_pasture', 'iws_rowcrop','iws_total_forest','iws_deciduous_forest',
                        'iws_evergreen_forest','iws_mixed_forest','iws_lagoslakeid',
                          'iws_woody_wetland','iws_emergent_wetland','iws_total_wetland','iws_roaddensity_density_mperha'), with=FALSE][order(iws_lagoslakeid)]
head(iws_lulc2)

# Set keys for merging
setkey(epi_lake, iws_zoneid)
setkey(iws_lulc2, iws_zoneid)

epi_lake_lulc <- merge(epi_lake, iws_lulc2)
# epi_lake[,.N]
epi_lake_lulc[,.N]
# head(epi_lake_lulc[order(lagoslakeid)])
# head(epi_lake[order(lagoslakeid)])
head(epi_lake_lulc)

###### Obtain IWS stream densities and headwater stream densities
conn <- data.table(dt$iws.conn)

# Select out columns on interest
conn2 <- conn[, c('iws_zoneid','iws_streamdensity_headwaters_density_mperha','iws_streamdensity_streams_density_mperha'),
              with=FALSE]
head(conn2)

# Set keys for merging
setkey(conn2, iws_zoneid)
setkey(epi_lake_lulc, iws_zoneid)

epi_lake_lulc <- merge(epi_lake_lulc, conn2)
head(epi_lake_lulc)


# Number of observations per HUC4
hist(epi_lake_lulc[,.N, by=hu4_zoneid]$N)
range(epi_lake_lulc[,.N, by=hu4_zoneid]$N)
median(epi_lake_lulc[,.N, by=hu4_zoneid]$N)
mean(epi_lake_lulc[,.N, by=hu4_zoneid]$N)
length(epi_lake_lulc[,.N, by=hu4_zoneid]$N)
######### Grab HU4 lulc
########################
huc4_lulc <- data.table(dt$hu4.lulc)

# Create Developed ("urban") combined measure as a proportion
huc4_lulc[, huc4_urban:= (hu4_nlcd2006_pct_21 + hu4_nlcd2006_pct_22 + hu4_nlcd2006_pct_23 + hu4_nlcd2006_pct_24)/100]
# Total agricultural land use (pature and rowcrop combined)
huc4_lulc[, huc4_ag:= (hu4_nlcd2006_pct_81 + hu4_nlcd2006_pct_82)/100]
# Pasture/hay ag
huc4_lulc[, huc4_pasture:= hu4_nlcd2006_pct_81/100]
# Row crop ag
huc4_lulc[, huc4_rowcrop:= hu4_nlcd2006_pct_82/100]
# deciduous forest
huc4_lulc[, huc4_deciduous_forest:= hu4_nlcd2006_pct_41/100]
# evergreen forest
huc4_lulc[, huc4_evergreen_forest:= hu4_nlcd2006_pct_42/100]
# mixed forest
huc4_lulc[, huc4_mixed_forest:= hu4_nlcd2006_pct_43/100]
# Forested land cover (combined)
huc4_lulc[, huc4_total_forest:= (hu4_nlcd2006_pct_41 + hu4_nlcd2006_pct_42 + hu4_nlcd2006_pct_43)/100]
# Wetlands
# Woody
huc4_lulc[, huc4_woody_wetland:= hu4_nlcd2006_pct_90/100]
# Emergent
huc4_lulc[, huc4_emergent_wetland:= hu4_nlcd2006_pct_95/100]
# Total
huc4_lulc[, huc4_total_wetland:= (hu4_nlcd2006_pct_90 + hu4_nlcd2006_pct_95)/100]


head(huc4_lulc)

# Select out columns of interest from hu4_lulc
huc4_lulc <- huc4_lulc[, c('hu4_zoneid','huc4_urban','huc4_ag','huc4_pasture', 'huc4_rowcrop','huc4_total_forest',
                           'huc4_deciduous_forest','huc4_evergreen_forest','huc4_mixed_forest',
                           'huc4_woody_wetland','huc4_emergent_wetland','huc4_total_wetland'), with=FALSE]
head(huc4_lulc)
huc4_lulc[,.N]

# Set keys for merging
setkey(epi_lake_lulc, hu4_zoneid)
setkey(huc4_lulc, hu4_zoneid)

epi_lake_lulc <- merge(epi_lake_lulc, huc4_lulc)
epi_lake_lulc[,.N]
head(epi_lake_lulc)
epi_lake_lulc[,.N, by=hu4_zoneid]
# Test code for merge
# test <- plyr::join(epi_lake, iws_lulc2, by=c("iws_zoneid"), type='left', match='all')
# dim(test)
# write.csv(test,'test.csv')


######## Grab N-depostion (1990 - 2010 data), baseflow, and runoff  data at the HU12 scale not available at the IWS scale
# help.search("hu12.chag", package = "LAGOS")
hu12_chag <- data.table(dt$hu12.chag)
head(hu12_chag)

hu12_chag2 <- hu12_chag[,c('hu12_zoneid', 'hu12_dep_totaln_1990_mean','hu12_dep_totaln_1995_mean','hu12_dep_totaln_2000_mean',
                           'hu12_dep_totaln_2005_mean', 'hu12_dep_totaln_2010_mean', 
                           'hu12_baseflowindex_mean','hu12_runoff_mean'), with=FALSE]
head(hu12_chag2)

# Set keys for merging
setkey(epi_lake_lulc, hu12_zoneid)
setkey(hu12_chag2, hu12_zoneid)
# Merge
epi_lake_lulc_n <- merge(epi_lake_lulc, hu12_chag2)
epi_lake_lulc_n[,.N]
head(epi_lake_lulc_n)


###### Grab change data at HU4 scale
hu4_chag <- data.table(dt$hu4.chag)
head(hu4_chag)

hu4_chag2 <- hu4_chag[,c('hu4_zoneid', 'hu4_dep_totaln_1990_mean','hu4_dep_totaln_1995_mean','hu4_dep_totaln_2000_mean',
                           'hu4_dep_totaln_2005_mean', 'hu4_dep_totaln_2010_mean', 
                           'hu4_baseflowindex_mean','hu4_runoff_mean','hu4_prism_ppt_30yr_normal_800mm2_annual_mean',
                         'hu4_prism_tmean_30yr_normal_800mm2_annual_mean'), with=FALSE]
head(hu4_chag2)


# Set keys for merging
setkey(epi_lake_lulc_n, hu4_zoneid)
setkey(hu4_chag2, hu4_zoneid)
# Merge
epi_lake_lulc_n <- merge(epi_lake_lulc_n, hu4_chag2)
epi_lake_lulc_n[,.N]
head(epi_lake_lulc_n)

###### Grab connectivity data at HU4 scale
hu4_conn <- data.table(dt$hu4.conn)
head(hu4.conn)

hu4_conn2 <- hu4_conn[,c('hu4_zoneid', 'hu4_lakes_lakes4ha_avgsize_ha','hu4_lakes_lakes4ha_count','hu4_lakes_lakes4ha_isolated_count',
                         'hu4_lakes_lakes4ha_headwater_count','hu4_lakes_lakes4ha_drstream_count',
                         'hu4_lakes_lakes4ha_drlakestream_count'), with=FALSE]
head(hu4_conn2)

## Proportion of lake conn types
hu4_conn2[, proportion_isolated:= hu4_lakes_lakes4ha_isolated_count/hu4_lakes_lakes4ha_count]
hu4_conn2[, proportion_headwater:= hu4_lakes_lakes4ha_headwater_count/hu4_lakes_lakes4ha_count]
hu4_conn2[, proportion_drstream:= hu4_lakes_lakes4ha_drstream_count/hu4_lakes_lakes4ha_count]
hu4_conn2[, proportion_drlakestream:= hu4_lakes_lakes4ha_drlakestream_count/hu4_lakes_lakes4ha_count]
hu4_conn2[, proportion_isolated_headwater:= (hu4_lakes_lakes4ha_isolated_count + hu4_lakes_lakes4ha_headwater_count)/hu4_lakes_lakes4ha_count]

# Set keys for merging
setkey(epi_lake_lulc_n, hu4_zoneid)
setkey(hu4_conn2, hu4_zoneid)
# Merge
epi_lake_lulc_n <- merge(epi_lake_lulc_n, hu4_conn2)
epi_lake_lulc_n[,.N]
head(epi_lake_lulc_n)




##### Merge IWS area for calculating WA:SA (residence time) area
iws_dat <- data.table(dt$iws)
head(iws_dat)

iws_dat2 <- iws_dat[, c("iws_zoneid","iws_areaha")]
head(iws_dat2)

setkey(epi_lake_lulc_n, iws_zoneid)
setkey(iws_dat2, iws_zoneid)

# Merge
epi_lake_final <- merge(epi_lake_lulc_n, iws_dat2)
epi_lake_final[,.N]
head(epi_lake_final)

# Calculate WA:LA
epi_lake_final[, wa_la:= iws_areaha/lake_area_ha]
head(epi_lake_final)

# Export data
write.csv(epi_lake_final, "var_comp_data.csv", row.names = FALSE)
