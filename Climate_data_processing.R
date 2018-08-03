# rm(list=ls())
library(data.table)

##### Read in merged files 
ptt_dat <- fread("HU12_ppt_merge.csv")
summary(ptt_dat)
# length(unique(ptt_dat$ZoneID))*length(unique(ptt_dat$Year))
ptt_dat[,.N]


# convert monthly vals to seasonal vals,
# where winter = Dec, Jan, Feb
# spring = March, April, May
# summer = June, Jul, Aug
# fall = Sep, Oct, Nov
# note that the first year in record (1970) has NA values because 
# winter is calculated with previous years' December value, and there is no
# 1969 data


##########################################
##### Precipitation seasonal summaries ###
##########################################
# Winter
ptt_dat[ , winter_ptt:= ifelse(Year==1972|Year==1976|Year==1980|Year==1984|Year==1988|Year==1992|Year==1996|Year==2000|Year==2004|Year==2008,
                               (January*31 + February*29 + shift(December, 1L, type="lag")*31)/91,
                               (January*31 + February*28 + shift(December, 1L, type="lag")*31)/90)]
# Spring
ptt_dat[ , spring_ptt:= (March*31 + April*30 + May*31)/92]
# Summer
ptt_dat[ , summer_ptt:= (June*30 + July*31 + August*31)/92]
# Fall
ptt_dat[ , fall_ptt:= (September*30 + October*31 + November*30)/91]
ptt_dat

write.csv(ptt_dat, "hu12_ptt_summaries.csv", row.names = FALSE)


##########################################
##### Temperature seasonal summaries #####
##########################################
##### Read in merged files 
temp_dat <- fread("HU12_tmean_merge.csv")
summary(temp_dat)
# length(unique(temp_dat$ZoneID))*length(unique(temp_dat$Year))
temp_dat[,.N]

# Winter
temp_dat[ , winter_temp:= ifelse(Year==1972|Year==1976|Year==1980|Year==1984|Year==1988|Year==1992|Year==1996|Year==2000|Year==2004|Year==2008,
                               (January*31 + February*29 + shift(December, 1L, type="lag")*31)/91,
                               (January*31 + February*28 + shift(December, 1L, type="lag")*31)/90)]
# Spring
temp_dat[ , spring_temp:= (March*31 + April*30 + May*31)/92]
# Summer
temp_dat[ , summer_temp:= (June*30 + July*31 + August*31)/92]
# Fall
temp_dat[ , fall_temp:= (September*30 + October*31 + November*30)/91]
temp_dat

write.csv(temp_dat, "hu12_temp_summaries.csv", row.names = FALSE)