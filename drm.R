library("FactoMineR")
library(dplyr)
library(tidyverse)
library(reshape2)
library(lmtest)
drm <- drm %>%
  group_by(Subregion, Date, Site, Batch) %>%
  mutate(id = cur_group_id())
data <- drm %>% select(Date, Depth, Zone, Habitat, Species, Width, 
                       Height, Bleaching, Old, Recent, Disease.Conditions,
                       Other.Conditions, id)
data <- data%>%
  mutate_if(is.character, ~replace(., is.na(.), "NONE"))

data$Bleaching[data$Bleaching == ""] <- "N"
data$Disease.Conditions[data$Disease.Conditions == ""] <- "N"
data$Other.Conditions[data$Other.Conditions == ""] <- "N"
data <- na.omit(data)
data$Year <- format(as.Date(data$Date, format="%m/%d/%Y"),"%Y")

res <- FAMD(data)
#total transects
total_transects <- data %>%
  group_by(Year, id) %>%
  summarize(n_transects = n())
total_transects <- data %>%
  group_by(Year) %>%
  summarize(n_transects = n())

#N of diseased corals
disease_by_type <- data %>%
  group_by(Year,Disease.Conditions) %>%
  summarize(total_disease = n())
disease <- dcast(data=disease_by_type, formula = Year~Disease.Conditions)
disease <- as.data.frame(disease)
disease[is.na(disease)] <- 0
disease$disease_per_coral <- (disease$`BB/RB`+disease$`BB/RB,DSD`+disease$DC+disease$DSD+disease$RTL+
  disease$STL+disease$UNK+disease$`UNK,DC`+disease$WPL+disease$WPL+disease$YB)/disease$N

barplot(disease$disease_per_coral)
#Only analyzing CNAT
#data <- data[data$Species == "CNAT",]


width <- data%>%
  group_by(Year) %>%
  summarize(total_width = sum(Width))
#width <- rbind(c(2005, 0), width) #add missing 0s
#width <- rbind(c(2022, 0), width) #add missing 0s
#width <- rbind(c(2016, 0), width) #add missing 0s
width <- width[order(width$Year),]
width <- width %>%
  mutate(avg_width = total_width/total_transects$n_transects)
barplot(width$avg_width)
d <- data.frame(disease$Year)
d$disease_per_coral <- disease$disease_per_coral
d$avg_width <- width$avg_width

grangertest(data = d, avg_width~disease_per_coral, order = 1)
