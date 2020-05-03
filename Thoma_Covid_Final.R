### Set working directory and read in CSV
setwd("/users/jthoma/Documents/GIS470/Final_Project")
states <- read.csv("state_final.csv")

### Install and library all packages
install.packages("ppcor")
install.packages("jtools")
install.packages("corrplot")
install.packages("sf")
install.packages("ggplot2")
install.packages("viridis")
library(ppcor)
library(jtools)
library(corrplot)
library(sf)
library(ggplot2)
library(viridis)

### Inspect data and choose independant and dependant varibales
head(states)
str(states)
# Dependant Varibale (Y): COVID_cases_per_million (int)
# Independant Varibale (X1): age_mean (num)
# Independant Varibale (X2): employment_rate (num)
# Independant Variable (X3): poverty (num)

### Calculate the mean and standard deviation for each varibales
### Exclude missing Values 
mean(states$COVID_cases_per_million,na.rm = TRUE)
sd(states$COVID_cases_per_million,na.rm = TRUE)

mean(states$age_mean,na.rm = TRUE)
sd(states$age_mean,na.rm = TRUE)

mean(states$employment_rate,na.rm = TRUE)
sd(states$employment_rate,na.rm = TRUE)

mean(states$poverty,na.rm = TRUE)
sd(states$poverty,na.rm = TRUE)

### Create new varibles for rates to show them as percent
states$employment_percent <- states$employment_rate*100
states$poverty_percent <- states$poverty*100

### Create correlation matrix of all varibales correlation coef
#   Create new df w/only essential variables
states_focus <- subset(states, select = c(COVID_cases_per_million, age_mean,
                                          employment_percent, poverty_percent))
#   Create corrleation matrix from new df using only complete observations
focus_cor_mat <- cor(states_focus,use = "complete.obs")
#   Create corrplot from correlation matrix 
corrplot(focus_cor_mat,method = "number",col = "black")

### Create Scatterplots comparing each X varible to the Y variable

# Create and export scatterplot of age_mean and COVID_cases_per_million
Age_plot <- ggplot(data = states,aes(x=age_mean,y=COVID_cases_per_million)) + 
  geom_point(aes(fill=region_short),size=4,pch=21)+
  xlab("Average Age")+
  ylab("COVID-19 Cases per Million")+
  ggtitle("Average Age vs COVID-19 Cases per Million")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,size = 18, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom") + 
  scale_fill_discrete(name = "Regions") +
  geom_smooth(method = "lm",se = F,color = "black")
print(Age_plot)
ggsave(plot = Age_plot,"Age_plot.png", dpi = 300)

# Create and export scatterplot of employment rate and COVID_cases_per_million
Employ_plot <- ggplot(data = states,aes(x=employment_percent,y=COVID_cases_per_million)) + 
  geom_point(aes(fill=region_short),size=4,pch=21)+
  xlab("Employment Rate (%)")+
  ylab("COVID-19 Cases per Million")+
  ggtitle("Employment Rate vs COVID-19 Cases per Million")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,size = 18, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom") + 
  scale_fill_discrete(name = "Regions") +
  geom_smooth(method = "lm",se = F,color = "black")
print(Employ_plot)
ggsave(plot = Employ_plot,"Employ_plot.png", dpi = 300)

# Create and export scatterplot of poverty rate and COVID_cases_per_million
Poverty_plot <- ggplot(data = states,aes(x=poverty_percent,y=COVID_cases_per_million)) + 
  geom_point(aes(fill=region_short),size=4,pch=21)+
  xlab("Poverty Rate (%)")+
  ylab("COVID-19 Cases per Million")+
  ggtitle("Poverty Rate vs COVID-19 Cases per Million")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,size = 18, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom") + 
  scale_fill_discrete(name = "Regions") +
  geom_smooth(method = "lm",se = F,color = "black")
print(Poverty_plot)
ggsave(plot = Poverty_plot,"Poverty_plot.png", dpi = 300)

### Create Maps of all varibales

#   Read in Shapefile
US_map <- st_read("states.shp")

#   Merge shaopefile with states data
US_map1 <- merge(US_map,states,by="STATE_NAME",all.x=T)

#   Create and export map of COVID_19 cases per million in USA
Covid_Map <- ggplot() +
  geom_sf(data = US_map1, size = 1, color = "black",
          aes(fill = COVID_cases_per_million)) +
  coord_sf() + theme_bw() + 
  ggtitle("US Map: COVID-19 Cases per Million")+ 
  scale_fill_viridis_c(option = "plasma")
print(Covid_Map)
ggsave(plot=Covid_Map,"Covid_Map.png")

#   Create and export map of average age in USA
Age_Map <- ggplot() +
  geom_sf(data = US_map1, size = 1, color = "black",
          aes(fill = age_mean)) +
  coord_sf() + theme_bw() + 
  ggtitle("US Map: Average Age of Population")+ 
  scale_fill_viridis_c(option = "plasma")
print(Age_Map)
ggsave(plot=Age_Map,"Age_Map.png")

#   Create and export map of employment rate percent in USA
Employment_Map <- ggplot() +
  geom_sf(data = US_map1, size = 1, color = "black",
          aes(fill = employment_percent)) +
  coord_sf() + theme_bw() + 
  ggtitle("US Map: Employment Rate (%)")+ 
  scale_fill_viridis_c(option = "plasma")
print(Employment_Map)
ggsave(plot=Employment_Map,"Employment_Map.png")

#   Create and export map of poverty rate percent in USA
Poverty_Map <- ggplot() +
  geom_sf(data = US_map1, size = 1, color = "black",
          aes(fill = poverty_percent)) +
  coord_sf() + theme_bw() + 
  ggtitle("US Map: Poverty Rate (%)")+ 
  scale_fill_viridis_c(option = "plasma")
print(Poverty_Map)
ggsave(plot=Poverty_Map,"Poverty_Map.png")

### Create a Multiple Regression model of Y and X1-3
#   Turn off e notation
options(scipen = 999)

#   Calculate multiple regression model
Covid_MRA <- lm(COVID_cases_per_million ~ age_mean + employment_percent +
                  poverty_percent, data = states_focus)
summ(Covid_MRA)














