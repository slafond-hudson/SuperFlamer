# Create a dataframe with feature and local main channel data
# to investigate spatial gradients in water quality from features.

# 1. List desired features
# 2. Get river distance associated with main channel-feature confluence
# 3. Add main channel data above and below mc-f confluence
# 4. Columns: dist, feature name, feature type, concentrations
# 5. Create a column that calculates dC/dx. dx = 50 m? distance between interpolation points

library(geostats)
library(ggplot2)
library(patchwork)
library(pracma)

df <- read.csv("C:/Users/slafond-hudson/DOI/WQP Prioritized Constituents Project - Task 2 Carbon and Nutrients in the IRB/Data/Flowline_interpolated_all.csv")

output_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/Merged_Illinois_May_2022_Jul_2023/RiverDist_plots/Features"
###pre formatting df

#convert concentrations and distance_m to numeric instead of character
cols_num <- c("barom_mmHg", "NO3_mgL", "temp_tau", "specCond_tau", "pH_tau", 
              "turb_FNU_tau", "ODO_percent_tau", "chlor_ugL_tau", "BGApc_ugL_tau", 
              "fDOM_QSU_tau", "CH4uM_tau", "CO2uM_tau", "barom_mmHg_err", 
              'NO3_mgL_err',  'temp_tau_err', 'specCond_tau_err', 'pH_tau_err', 
              'turb_FNU_tau_err', 'ODO_percent_tau_err', 'chlor_ugL_tau_err', 
              'BGApc_ugL_tau_err', 'fDOM_QSU_tau_err', 'CH4uM_tau_err', 
              'CO2uM_tau_err', 'Latitude', 'Longitude', 'geometry', 'distance_m')

df[cols_num] <- sapply(df[cols_num], as.numeric)
sapply(df, class)

df$distance_km <- df$distance_m/1000

### List features of interest and get their locations

# Select tribs of interest
tribs <- c("KANKAKEE RIVER",
           "FOX RIVER", 
           "VERMILION RIVER", 
           "MACINAW RIVER", 
           "SPOON RIVER",
           "SANGAMON RIVER", 
           "LA MOINE RIVER")

trib_dist <- df %>%
  select(Name, distance_km)%>%
  filter(Name %in% tribs)%>%
  group_by(Name)%>%
  summarize(Location = min(distance_km))%>%
  mutate(Feature_type = "Trib")

# Select backwaters of interest
backwaters <- c("BUBBLY CREEK", 
                "HANSEN BACKWATER LAKE", 
                "MARSEILLES LOCK APPROACH CHANNEL",
                "QUIVER LAKE", 
                "BATH CHUTE", 
                "TREADWAY LAKE")

backwater_dist <- df %>%
  select(Name, distance_km)%>%
  filter(Name %in% backwaters)%>%
  group_by(Name)%>%
  summarize(Location = min(distance_km))%>%
  mutate(Feature_type = "Backwater")

# Add together tribs + backwaters into one dataframe
feature_dist <- rbind(trib_dist, backwater_dist)

# Vectorize names and locations to use in filtering the larger datatset
feature_name <- feature_dist$Name
mc_feat <- c(feature_name, "ILLINOIS RIVER")
feature_location <- feature_dist$Location

# Select just the columns to investigate
df_short <- df %>% 
  select(distance_km, Trip, AQUA_DESC, Name, 
         NO3_mgL, 
         specCond_tau, 
         ODO_percent_tau, 
         chlor_ugL_tau, 
         CH4Sat_tau, 
         CO2Sat_tau) %>% 
  filter(!is.na(var_i))

### How to filter main channel data just up/downstream of feature locations??

# Considered a for loop, but that seems to be considered a bad idea
#I don't know how many rows I'm grabbing from each feature location
# I could maybe figure this out actually-if there are x points per km and I'm grabbing 
# data from +/- 5km (10km total * x points per km)

# Also considered making a function and then using lapply
# But I think this is goofy since my function is to filter based on criteria

df_short2 <- df_short %>%
  filter(Name %in% mc_feat)
#quick check
df_short2_count <- df_short2 %>% group_by(Name) %>% count()

# Basically turn this into a loop for 1:length(feat_loc)
# data_i_main <- df_short %>% 
#   filter(AQUA_DESC=="Main Navigation Channel",
#          distance_km > feat_loc-5 & distance_km < feat_loc+5)

df_short3 <- df_short2 %>% 
  group_by(Name)%>%
  summarize(feat_loc=min(distance_km))%>%
  filter(distance_km > feat_loc-5 & distance_km < feat_loc+5)


df_mcf <- list()

for (i in 1:length(feature_location)){
  df_short2 <- df_short %>%
    filter(distance_km > feature_location[i] - 5 & distance_km < feature_location[i] + 5)
  rbind(df_short2, df_mcf)
}

# this doesn't grow the dataframe. Tried adding rbind(df_mcf, df_short2) 
# but that doesn't work (in addition to not being recommended)

dist_filter <- function(df, feature_location){
  df_short %>%
    filter(distance_km > feature_location[i] - 5 & distance_km < feature_location[i] + 5)
}

lapply(1:length(feature_location), dist_filter)



####################################
# Working on this manually until I can figure out how to better automate

# Change constituent and/or feature here
var_i = "CO2Sat_tau"
feat_name = "LA MOINE RIVER"

# Filter data from feature
data_i_back <- df_short %>%
  filter(Name==feat_name)%>%
  select(distance_km, any_of(var_i), Trip, AQUA_DESC) %>% 
  filter(!is.na(var_i))

#define feat_loc as the min (distance_km) for Name = feat_name
feat_loc <- feature_dist['Location'][feature_dist['Name']==feat_name]

data_i_main <- df_short %>% 
  filter(AQUA_DESC=="Main Navigation Channel",
         distance_km > feat_loc-5 & distance_km < feat_loc+5)

data_i_back$Trip <- factor(x=data_i_back$Trip, levels=c("May_2022", "Aug_2022", "Nov_2022", "Mar_2023", "Jul_2023"))
data_i_main$Trip <- factor(x=data_i_main$Trip, levels=c("May_2022", "Aug_2022", "Nov_2022", "Mar_2023", "Jul_2023"))

fig <- ggplot(data_i_back, aes(x=distance_km, y=.data[[var_i]]))+
  geom_line(linetype="dashed", linewidth = 1, color="orange3")+
  geom_point(data=data_i_main, aes(x=distance_km, y = .data[[var_i]]), color="steelblue", size=0.5)+
  facet_grid(rows=vars(Trip))+
  scale_x_reverse()+
  #limits=c(422.5, 418)
  #limits=c(518.5, 514.5)
  # scale_y_continuous(breaks = seq(0, 80, by = 30)) +
  labs(x = "Distance upstream of confluence (km)", title=feat_name)+
  theme_bw()+
  theme(legend.position = "right")
print(fig)

ggsave(file.path(output_path, paste(feat_name, '_', var_i, ".png", sep="")), 
       fig, width = 6.5, height = 9, units = "in")

