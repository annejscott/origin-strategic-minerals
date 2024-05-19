# DATA 608 Story 7

# Custom Functions

build_minerals_df <- function(sheet, mineral) {
  # Import .csv
  temp <- read_excel("data/mining-data/6.5. Share_of_World_Mineral_Production_2022_by_Countries.xlsx",
                     sheet=sheet,
                     skip=1)
  #filter for columns
  temp <- temp[, c(3, 4, 6)]
  
  # Add a new column filled with the same value in each cell
  temp$minerals <- mineral
  
  #rbind with minerals
  minerals <- rbind(minerals, temp)
  
  return(minerals)
}

build_stability_df <- function(sheet, mineral) {
  # Import .csv
  temp <- read_excel("data/mining-data/6.3c. Political_stability.xlsx",
                     sheet=sheet,
                     skip=1)
  
  # Add a new column filled with the same value in each cell
  temp$minerals <- mineral
  
  #rbind with minerals
  stability <- rbind(stability, temp)
  
  return(stability)
}