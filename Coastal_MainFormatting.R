# This script holds only the code used to format the raw data and create the cleaned data for analysis.
# The analysis can be found in the Coastal_Main.Rmd file

library(tidyverse)
library(reshape2)

###########################################
### FIRST CLEANING THE MUDDLED RAW DATA ###
###########################################

# filtering procedure because sometimes the BehaviorSpace output gets muddled in a few rows.
# This removes those problematic rows and keeps only the parsable data

filterBasics <- function(ds){
  
  ds <- ds %>% 
    filter(bs.run.number != "bs.run.number",
           spatial.foresight == "true",
           n.camps %in% c("5","15"),
           daily.time.budget %in% c("10","12"),
           hunter.percent == "0.3",
           vision.forager == "20",
           forager.movement %in% c("local-patch-choice", "random"),
           n.foragers == "60",
           days.foresight %in% c("5","10"),
           global.knowledge == "true",
           walk.speed %in% c("2","3"),
           ticks == "366") %>%
    # removes the first and last columns
    dplyr::select(!c(1, tail(names(.), 1))) %>%  
    dplyr::select(!c(spatial.foresight, hunter.percent, vision.forager, n.foragers, global.knowledge)) %>% # Removing variables that did not change
    
    # Replaces NA with "0" because at the moment, those cells are strings
    mutate(totalKcalShell = ifelse(is.na(totalKcalShell), "0", totalKcalShell),
           totalKcalShellProcess = ifelse(is.na(totalKcalShellProcess), "0", totalKcalShellProcess),
           totalKcalPlant = ifelse(is.na(totalKcalPlant), "0", totalKcalPlant),
           totalKcalMeat = ifelse(is.na(totalKcalMeat), "0", totalKcalMeat),
           previousVt = ifelse(is.na(previousVt), "0", previousVt))
  
  return(ds)
}

# These lines only need to be run once
# Read the output and uses the first column as header
ds <- read_csv("QI_HPC_Main.csv")

# Create a temporary dataset that has only the cells with projectile armatures
ds <- ds %>%
  filterBasics

# This writes the data to a CSV that can then be reread for quick column type formatting
write.csv(ds, "QI_Main_cleaned.csv", row.names=FALSE)

# Reread the dataset for clean types
ds <- read_csv("QI_Main_cleaned.csv")

###################################
### ADDING DIST FROM COAST DATA ###
###################################

distCoast <- read_csv("DistFromCoast.csv")

ds <- ds %>% 
  left_join(distCoast, by = c("xcor","ycor")) %>% 
  mutate(distCoast = distCoast / 10) # Transform in km

# Clean up because we don't need this anymore.
rm(distCoast)

############################
### CALCULATE NEW VALUES ###
############################

# Calculate the ratio of each food source per cell
# 
# This step calculates the mean and sum kcal quantities for each food source for each cell (observation).
# 
# It also uses the outputs to calculate the how long each cell was occupied.
# 
# These values can then be fed to the linear regressions below.

# This will calculate a value from the string in the dataset
getValue <- function(cell, value){
  
  a <- str_split(cell, " ", simplify = TRUE) %>% 
    unlist()   # Split the different values
  
  b <- as.numeric(a)
  
  return(value(b, na.rm = TRUE))
  
}

# In this function, for each patch, I calculate the length of time it was occupied, as well as the mean length of occupation of each occupation. I also calculate the sum kcal from each food source accumulated there, as well as the mean per day. Finally, I calculate the ratio of each food source that is accumulated on that cell.

formatSites <- function(ds){
  
  # Select the columns that have the relevant data
  a <- ds %>% 
    rowwise() %>% 
    mutate(totalLengthOccupation = ifelse(site == 0, 
                                          1, 
                                          length(unlist(stringr::str_split(totalKcalMeat, " ")))),
           MeanLengthOccupation = ifelse(site == 0,
                                         0, 
                                         totalLengthOccupation/site),
           # This is to remove more rows where wrong values might have snuck in.
           test = ifelse(site == 0, 
                         1, 
                         length(unlist(stringr::str_split(totalKcalShell, " "))))) %>% 
    ungroup() %>% 
    filter(totalLengthOccupation == test) 
  
  a %>% 
    rowwise() %>% 
    mutate(sumShellProcess = getValue(totalKcalShellProcess, sum),
           sumShellCamp = getValue(totalKcalShell, sum),
           sumShell = sumShellCamp + sumShellProcess,
           meanShell = sumShell / totalLengthOccupation,
           totalLengthOccupation = ifelse(site == 0, 
                                          0, 
                                          length(unlist(stringr::str_split(totalKcalMeat, " ")))),
           meanMeat = getValue(totalKcalMeat, mean),
           sumMeat = getValue(totalKcalMeat, sum),
           meanPlant = getValue(totalKcalPlant, mean),
           sumPlant = getValue(totalKcalPlant, sum),
           totKcal = sumShell + sumMeat + sumPlant,
           percShell = sumShell / totKcal,
           percMeat = sumMeat / totKcal,
           percPlant = sumPlant / totKcal) %>% 
    
    ungroup() %>% 
    dplyr::select(!c(totalKcalShell, totalKcalShellProcess, totalKcalMeat, totalKcalPlant)) %>% 
    # Adding a variable that defines if a cell is coastal or not (to compare)
    mutate(coastal = ifelse(vegetation %in% c(10,11,12,13,14), "Coastal", "Non-coastal"),
           mainSourceFood = ifelse(sumShell > sumPlant, 
                                   ifelse(sumShell > sumMeat, "Shell", "Meat"),
                                   ifelse(sumPlant > sumMeat, "Plant","Meat")))
  
}

# Create a new dataset with those values
ds.sites <- formatSites(ds)

######################
### ADD BIOME INFO ###
######################

normalize <- function(x){
  
  return((x - min(x)) / (max(x) - min(x)))
  
}

# Add the biomes' size and productivity information
# First import the map
library(raster)
veg <- raster("all_habitats_1ha.asc")

# Then get the size of each biome
biome <- as.data.frame(freq(veg))
# Add their productivity manually (from Netlogo code)
biome$maxKcal <- c(0,270,179,219,0,63,354,241,130,2019,87,1799,1203,1867)
# and rename the columns and remove the ocean "biome"
biome <- biome %>% 
  rename(vegN = value,
         size = count) %>% 
  filter(vegN != 0)

# Normalize the size and maxKcal to make their contribution comparable.
biome <- biome %>% 
  mutate(size = normalize(size),
         maxKcal = normalize(maxKcal))

# Add those data to the ABM output dataset
ds.sites <- ds.sites %>% 
  left_join(biome, by = c("vegetation" = "vegN"))

################################################
### FOCUSING ONLY ON CELLS OCCUPIED BY CAMPS ###
################################################

# Need to remove the cells that were never occupied (but where shellfish was processed, so they are in the output, but shouldn't be)

ds.sites.only <- ds.sites %>% 
  filter(totalLengthOccupation > 0)

#####################################
### CONDENSE INTO PALIMPSEST DATA ###
#####################################

# New dataset that compiles data per cell (palimpsest of all runs).
ds.per.patch <- ds.sites %>% 
  group_by(xcor, ycor) %>%
  summarize(site = sum(site),
            coastal = coastal[1],
            vegetation = vegetation[1],
            size = size[1],
            maxKcal = maxKcal[1],
            distCoast = distCoast[1],
            length.assemblage = sum(length.assemblage),
            totalLengthOccupation = sum(totalLengthOccupation),
            sumShell=sum(sumShell),
            sumMeat = sum(sumMeat),
            sumPlant = sum(sumPlant)) %>% 
  rowwise() %>% 
  mutate(totKcal = sumShell + sumMeat + sumPlant,
         percShell = sumShell / totKcal,
         percMeat = sumMeat / totKcal,
         percPlant = sumPlant / totKcal)

visibility.per.run <- ds.sites.only %>% 
  group_by(bs.run.number, coastal) %>% 
  summarize(meanOcc = mean(totalLengthOccupation),
            meanArtifact = mean(length.assemblage),
            meanPercShell = mean(percShell),
            meanPercMeat = mean(percMeat)) %>% 
  tidyr::pivot_wider(id_cols = bs.run.number,
                     names_from = coastal,
                     values_from = c(meanOcc,meanArtifact,meanPercShell,meanPercMeat)) %>% 
  rowwise() %>% 
  mutate(diffOcc = `meanOcc_Non-coastal` - meanOcc_Coastal,
         diffArtifact = `meanArtifact_Non-coastal` - meanArtifact_Coastal,
         diffPercShell = `meanPercShell_Non-coastal` - meanPercShell_Coastal,
         diffPercMeat = `meanPercMeat_Non-coastal` - meanPercMeat_Coastal) %>% 
  left_join(ds, by = "bs.run.number") %>% 
  dplyr::select(n.camps, daily.time.budget, forager.movement, days.foresight, walk.speed, diffOcc, diffArtifact, diffPercShell, diffPercMeat) %>% 
  distinct() %>% 
  mutate(n.camps = as.factor(n.camps),
         daily.time.budget = as.factor(daily.time.budget),
         forager.movement = as.factor(forager.movement),
         days.foresight = as.factor(days.foresight),
         walk.speed = as.factor(walk.speed))

############################
### FORMATTING FOR PLOTS ###
############################

# This formats the variables in ways that will be easier to analyse

formatDs <- function(ds){
  
  # Select the columns that have the relevant data for these analyses
  ds <- ds %>% 
    as_tibble() %>% 
    # Transform some variables into numeric values
    mutate(site = as.numeric(site),
           bs.run.number = as.numeric(bs.run.number),
           # and others into factors (for cleaner visualization)
           n.camps = factor(n.camps, levels = c(5, 15), ordered = T),
           daily.time.budget = factor(daily.time.budget,levels = c(10, 12), 
                                      ordered = T),
           days.foresight = factor(days.foresight,levels = c(5,10),
                                   ordered = T),
           walk.speed = factor(walk.speed,levels = c(2,3),
                               ordered = T),
           forager.movement = factor(forager.movement))
  
  return(ds)
  
}

ds.sites.for.plot <- formatDs(ds.sites.only)

###################################
### TEST THE IMPORTANCE OF BAGS ###
###################################

# Not run at the moment

# formatSitesBags <- function(ds){
#   
#   # Select the columns that have the relevant data
#   ds.sites %>% 
#     # some cleanup
#     mutate(siteType = ifelse(site == 0, "collecting","habitation"),
#            totalLengthOccupation = ifelse(site == 0, 0, totalLengthOccupation),
#            MeanLengthOccupation = ifelse(site == 0, 0, MeanLengthOccupation)) %>% 
#     select(!previousVt) %>% 
#     replace(is.na(.), 0) %>% 
#     group_by(bs.run.number, siteType, coastal) %>% 
#     summarize(onsiteProcessThreshold = onsiteProcessThreshold[1],
#               sumShell = sum(sumShell)) %>% 
#     ungroup() %>% 
#     tidyr::pivot_wider(names_from = c(siteType, coastal), values_from = sumShell) %>% 
#     replace(is.na(.), 0) 
#     
# }

# # These lines only need to be run once
# # Read the output and uses the first column as header
# ds <- read_csv("QI_HPC_MainBags.csv")
# 
# # Create a temporary dataset that has only the cells with projectile armatures
# ds <- ds %>%
#   filterBasics
# 
# write.csv(ds, "QI_Main_cleanedBags.csv", row.names=FALSE)

# ds <- read_csv("QI_Main_cleanedBags.csv")
# 
# ds.sites <- formatSites(ds)
# ds.sites.bags <- formatSitesBags(ds.sites)

# Save all the formatted datasets to an R image that can be loaded (speed up the analysis)

save.image("FormattedMain.RData")

