# DATA 608 Story 7

# Data Processing and Tidying

# Packages ------
library(tidyverse)
library(readxl)

# Sources ----
source('R/functions.R')


# make a portion of production by country df -----
# initialize new dataframe
minerals <- tibble(
  Country = character(),
  unit = character(),
  `Share in %` = numeric(),
  mineral = character()
)

# add minerals to dataset
minerals <- build_minerals_df(2, "chromium")
minerals <- build_minerals_df(3, "cobalt")
minerals <- build_minerals_df(4, "manganese")
minerals <- build_minerals_df(6, "nickel")
minerals <- build_minerals_df(7, "niobium")
minerals <- build_minerals_df(8, "tantalum")
minerals <- build_minerals_df(9, "titanium")
minerals <- build_minerals_df(10, "tungsten")
minerals <- build_minerals_df(11, "vanadium")
minerals <- build_minerals_df(12, "aluminum")
minerals <- build_minerals_df(13, "antimony")
minerals <- build_minerals_df(14, "arsenic")
minerals <- build_minerals_df(16, "beryllium")
minerals <- build_minerals_df(17, "bismuth")
minerals <- build_minerals_df(20, "gallium")
minerals <- build_minerals_df(21, "germanium")
minerals <- build_minerals_df(22, "indium")
minerals <- build_minerals_df(24, "lithium")
minerals <- build_minerals_df(29, "tellurium")
minerals <- build_minerals_df(30, "tin")
minerals <- build_minerals_df(31, "zinc")
minerals <- build_minerals_df(33, "palladium")
minerals <- build_minerals_df(34, "platinum")
minerals <- build_minerals_df(35, "rhodium")
minerals <- build_minerals_df(45, "fluorspar")
minerals <- build_minerals_df(46, "graphite")


minerals$Country <- ifelse(minerals$Country == 'Türkiye', 'Turkey', minerals$Country)
minerals$Country <- ifelse(minerals$Country == 'Total', NA, minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Congo, D.R.", 'Congo', minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Congo, Rep.", 'Congo', minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Korea", 'South Korea', minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Korea, South", 'South Korea', minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Korea, North", 'North Korea', minerals$Country)
minerals$Country <- ifelse(minerals$Country == 'Bosnia', 'Bosnia and Herzegovina', minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Russian", 'Russia', minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Madagasca", "Madagascar", minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Tanzani", "Tanzania", minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Brazi" , "Brazil" , minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Burma" , "Myanmar" , minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Nambia" , "Namibia" , minerals$Country)

minerals <- na.omit(minerals)

saveRDS(minerals,
        "data/rds/sharesByCountry.rds")

# Make main df with mineral, source, relationship ----
#import mineral sources dataset
temp <- read_excel("data/mineral-sources.xlsx")

# save sheet as imported
saveRDS(temp,
        "data/rds/relationships.rds")

#subset minerals
minerals <- minerals[, c(1, 4)]

#combine with collected data
minerals <- rbind(minerals, temp)

# Remove duplicate pairs of rows
minerals <- unique(minerals)

# code out typos
minerals$Country <- ifelse(minerals$Country == 'Türkiye', 'Turkey', minerals$Country)
minerals$Country <- ifelse(minerals$Country == 'Total', NA, minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Congo, D.R.", 'Congo', minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Congo, Rep.", 'Congo', minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Korea", 'South Korea', minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Korea, South", 'South Korea', minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Korea, North", 'North Korea', minerals$Country)
minerals$Country <- ifelse(minerals$Country == 'Bosnia', 'Bosnia and Herzegovina', minerals$Country)
minerals$Country <- ifelse(minerals$Country == 'Bosnia-Herzegovina', 'Bosnia and Herzegovina', minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Russian", 'Russia', minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Madagasca", "Madagascar", minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Tanzani", "Tanzania", minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Brazi" , "Brazil" , minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Burma" , "Myanmar" , minerals$Country)
minerals$Country <- ifelse(minerals$Country == "Nambia" , "Namibia" , minerals$Country)

#rename column 
minerals$mineral <- minerals$minerals
minerals <- minerals[, c(1, 3)]


#import country-relationships dataset
temp <- read_excel("data/allies.xlsx")

#combine dataframes
minerals <- merge(minerals, temp, by = "Country", all.x = TRUE)

minerals$relationship <- as.factor(minerals$relationship)

# save to rds
saveRDS(minerals,
        "data/rds/minerals.rds")

# Create a mineral stability df -----
# initialize new dataframe
stability <- tibble(
  `Political stability` = character(),
  unit = character(),
  `2018` = numeric(),
  `2019` = numeric(),
  `2020` = numeric(),
  `2021` = numeric(),
  `2022` = numeric(),
)

stability <- build_stability_df(2, "chromium")
stability <- build_stability_df(3, "cobalt")
stability <- build_stability_df(4, "manganese")
stability <- build_stability_df(6, "nickel")
stability <- build_stability_df(7, "niobium")
stability <- build_stability_df(8, "tantalum")
stability <- build_stability_df(9, "titanium")
stability <- build_stability_df(10, "tungsten")
stability <- build_stability_df(11, "vanadium")
stability <- build_stability_df(12, "aluminum")
stability <- build_stability_df(13, "antimony")
stability <- build_stability_df(14, "arsenic")
stability <- build_stability_df(16, "beryllium")
stability <- build_stability_df(17, "bismuth")
stability <- build_stability_df(20, "gallium")
stability <- build_stability_df(21, "germanium")
stability <- build_stability_df(22, "indium")
stability <- build_stability_df(24, "lithium")
stability <- build_stability_df(29, "tellurium")
stability <- build_stability_df(30, "tin")
stability <- build_stability_df(31, "zinc")
stability <- build_stability_df(33, "palladium")
stability <- build_stability_df(34, "platinum")
stability <- build_stability_df(35, "rhodium")
stability <- build_stability_df(45, "fluorspar")
stability <- build_stability_df(46, "graphite")

# make a average column
stability$average <- rowMeans(stability[, 3:7], na.rm = TRUE)

# make a percent column
# Initialize a vector to store the percentages
percentages <- numeric(nrow(stability))

# Loop over every group of 5 rows
for (i in seq(1, nrow(stability), by = 5)) {
  # Calculate the percentage for each group
  for (j in 1:4) {
    percentages[i + j - 1] <- round((stability$average[i + j - 1] / stability$average[i + 4]) * 100, 1)
  }
}

# Add the 'percent' column to the data frame
stability$percent <- percentages

# Remove rows where 'Political stability' is "Total"
stability <- stability[stability$`Political stability` != "Total", ]

saveRDS(stability,
        "data/rds/stability.rds")
