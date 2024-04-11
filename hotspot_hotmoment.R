#hot spot/hot moment analysis

library(hotspomoments)

# source("./load_interpolated.R")

# df <- df %>%
#   select("NO3_mgL", "ODO_percent_tau",
#          "CO2Sat_tau", "CH4Sat_tau",
#          "chlor_ugL_tau","BGApc_ugL_tau",
#          "fDOM_QSU_tau", "specCond_tau",
#          "temp_tau", "pH_tau", "turb_FNU_tau",
#          "CH4uM_tau", "CO2uM_tau",
#          "Latitude", "Longitude", "geometry", "Name",
#          "distance_m", "AQUA_CODE", "Trip")


vars <- c("NO3_mgL", "ODO_percent_tau", 
          "CO2Sat_tau", "CH4Sat_tau",
          "chlor_ugL_tau")

df[vars] <- sapply(df[vars], as.numeric)
sapply(df, class)

# var_i <- "CO2Sat_tau"

#try this with all data and also for just main channel.
for (var_i in vars) {
  df2 <- df %>%
    select(any_of(var_i), "distance_m", "AQUA_CODE", "Trip")%>%
    # filter(AQUA_CODE=="MNC")%>%
    na.omit()
  
  hist(df2[[var_i]], main=var_i)
  print(var_i)
  print(paste("skewness = ", sample_skewness(df2[[var_i]])))
  print(paste("kurtosis = ", sample_kurtosis(df2[[var_i]])))
  
}



#so with all data, right skewed
#with just main channel, DO is not skewed
#not sure if it matters that all variables also have excess kurtosis?
#proceding with skewness as the metric
#maybe test DO for kurtosis? rest for right skew

# vars2 <- c("NO3_mgL", "CO2Sat_tau", 
#            "CH4Sat_tau", "chlor_ugL_tau")
# 
# for (var_i in vars2){
#   df2 <- df %>%
#     select(any_of(var_i), "distance_m", "AQUA_CODE", "Trip")%>%
#     filter(AQUA_CODE=="MNC")%>%
#     na.omit()
#   
#   hshmtest_no3 <- hshmtest(df2[[var_i]], stat="skewness")
#   hshmid_no3 <- hshmid(df2[[var_i]], criteria = "ref.normal", side="upper", thresh=0.95)
#   
#   #plot hshm on histogram
#   hist(df2[[var_i]], main = var_i)
#   points(df2[[var_i]][hshmid_no3], rep(0,sum(hshmid_no3)), pch=19, col="red")
#   
#   
# }

## Doing the analyses by variable manually instead of loop
#Although DO tested for right skewed, it also looks like
#it has extreme values on both sides, so keep kurtosis as metric

## NO3 ##
#perform test and identify which samples are hshm
no3 <- df %>%
  select("NO3_mgL", "distance_m", "AQUA_CODE", "Trip")%>%
  # filter(AQUA_CODE=="MNC")%>%
  na.omit()

hshmtest_no3 <- hshmtest(no3$NO3_mgL, stat="kurtosis")
hshmid_no3 <- hshmid(no3$NO3_mgL, criteria = "ref.normal", side="both", thresh=0.95)
no3_hshm <- no3[hshmid_no3,]

## ODO ##
# this doesn't seem to be working
odo <- df %>%
  select("ODO_percent_tau", "distance_m", "AQUA_CODE", "Trip")%>%
  # filter(AQUA_CODE=="MNC")%>%
  na.omit()

hshmtest_odo <- hshmtest(odo$ODO_percent_tau, stat="kurtosis")
hshmid_odo <- hshmid(odo$ODO_percent_tau, criteria = "ref.normal", thresh=0.95, side="both")
odo_hshm <- odo[hshmid_odo,]

## CO2 sat ##
co2 <- df %>%
  select("CO2Sat_tau", "distance_m", "AQUA_CODE", "Trip")%>%
  # filter(AQUA_CODE=="MNC")%>%
  na.omit()

hshmtest_co2 <- hshmtest(co2$CO2Sat_tau, stat="skewness")
hshmid_co2 <- hshmid(co2$CO2Sat_tau, criteria = "ref.normal", side="upper", thresh=0.95)
co2_hshm <- co2[hshmid_co2,]

## CH4 sat ##
ch4 <- df %>%
  select("CH4Sat_tau", "distance_m", "AQUA_CODE", "Trip")%>%
  # filter(AQUA_CODE=="MNC")%>%
  na.omit()

hshmtest_ch4 <- hshmtest(ch4$CH4Sat_tau, stat="skewness")
hshmid_ch4 <- hshmid(ch4$CH4Sat_tau, criteria = "ref.normal", side="upper", thresh=0.95)
ch4_hshm <- ch4[hshmid_ch4,]

## chlorophyll ##
chl <- df %>%
  select("chlor_ugL_tau", "distance_m", "AQUA_CODE", "Trip")%>%
  # filter(AQUA_CODE=="MNC")%>%
  na.omit()

hshmtest_chl <- hshmtest(chl$chlor_ugL_tau, stat="skewness")
hshmid_chl <- hshmid(chl$chlor_ugL_tau, criteria = "ref.normal", side="upper", thresh=0.95)
chl_hshm <- chl[hshmid_chl,]

# #plot hshm on histogram
hist(no3$NO3_mgL)
points(no3$NO3_mgL[hshmid_no3], rep(0,sum(hshmid_no3)), pch=19, col="red")
# 
# hist(odo$ODO_percent_tau)
# points(odo$ODO_percent_tau[hshmid_odo], rep(0,sum(hshmid_odo)), pch=19, col="red")
# 
# #save dataframe with hshm concentrations, trip and distance
# no3_hshm <- no3[hshmid_no3,]
# #approximately 4% of values are considered hot spot/hot moment in main channel
# 
# print(table(no3$Trip))
# #pretty evenly distributed across trip
# 
# distance <- hshm %>% count(distance_m)
