# This script was used to clean and format the raw outputs from BehaviorSpace

filterBasicsSensitivity <- function(ds, type = c("length","end", "rep")){
  
  ds <- ds %>% 
    filter(bs.run.number != "bs.run.number",
           n.camps == "5",
           daily.time.budget %in% c("8", "10", "12"),
           hunter.percent %in% c("0.3", "0.5", "0.7"),
           point.recycling %in% c("25", "75"),
           point.hunting %in% c("25", "50", "75", "100"),
           as.numeric(site) > 0,
           !is.na(totalKcalShell)) %>% 
    mutate(bs.run.number = as.numeric(bs.run.number))
  
  if(type == "end"){
    ds <- ds %>% 
      filter(ticks == "181")
  }
  
  if(type == "rep"){
    ds <- ds %>% 
      filter(ticks == "101")
  }
  
  return(ds)
}

# This function cleans the dataset:
## Remove some variables that are not used,
## Transform some variables into factors (for graph purposes)

formatDs <- function(ds){
  
  # Select the columns that have the relevant data for these analyses
  ds <- ds %>% 
    as_tibble() %>% 
    mutate(site = as.numeric(site)) %>% 
    dplyr:: select(!c(1, tail(names(.), 1))) %>%  # remoes the first and last columns
    filter(ticks > 0) %>% 
    # Transform some variables into clean factors
    mutate(n.camps = factor(n.camps, levels = c(5, 10, 15, 20), ordered = T),
           daily.time.budget = factor(daily.time.budget,levels = c(8, 10, 12), 
                                      ordered = T),
           hunter.percent = factor(hunter.percent,levels = c(0.3, 0.5, 0.7),
                                   ordered = T),
           n.foragers=factor(n.foragers,levels = c(10, 20, 40, 60, 70, 100, 400), 
                             ordered = T),
           point.recycling = factor(point.recycling,levels = c(25, 75),
                                    ordered = T),
           point.hunting = factor(point.hunting,levels = c(25,50,75,100),
                                  ordered = T),
           # Adding a variable that defines if a cell is coastal or not (to compare)
           coastal = ifelse(vegetation %in% c(10,11,12,13,14), TRUE, FALSE))
  
  return(ds)
  
}

## LENGTH OF RUNS ##

# # These lines should only be used the first time with uncleaned dataset. After cleaning, simply import the cleaned version.
# 
# # Read the output and uses the first column as header
# ds.S <- read_csv("QI_HPC_Sensitivity_Length.csv")
# 
# # Create a temporary dataset that has only the cells with projectile armatures
# ds.S <- ds.S %>%
#   filterBasicsSensitivity(type = "length") %>% 
#   dplyr::select(!assemblage) # Don't need it here, and format can be crooked
# 
# write.csv(ds.S, "QI_Length_cleaned.csv", row.names=FALSE)

# Read the cleaned version
ds.Length <- read_csv("QI_Length_cleaned.csv")

# Reformat the dataset (cleans unnecessary columns)
ds.Length <- formatDs(ds.Length)

## NUMBER OF REPETITIONS ##

# These lines should only be used the first time with uncleaned dataset. After cleaning, simply import the cleaned version.

# # Read the output and uses the first column as header
# ds.S <- read_csv("QI_HPC_Sensitivity_Repetitions.csv")
# 
# # Create a temporary dataset that has only the cells with projectile armatures
# ds.S <- ds.S %>%
#   filterBasicsSensitivity(type = "rep") %>% 
#   dplyr::select(!assemblage) # Don't need it here, and format can be crooked
# 
# write.csv(ds.S, "QI_Repetitions_cleaned.csv", row.names = F)

# Read the cleaned version
ds.Repetitions <- read.csv("QI_Repetitions_cleaned.csv")

# Reformat the dataset (cleans unnecessary columns, separates the projectile parts 
## and change vegetation into factors)
ds.Repetitions <- formatDs(ds.Repetitions)

## SETTINGS ##

filterBasicsSettings <- function(ds){
  
  ds <- ds %>% 
    filter(bs.run.number != "bs.run.number",
           n.camps %in% c("5", "20"),
           daily.time.budget %in% c("6", "8", "10"),
           hunter.percent %in% c("0.3", "0.5"),
           vision.forager %in% c("10","20"),
           days.foresight %in% c("5","10"),
           walk.speed %in% c("2","3"),
           as.numeric(site) > 0,
           !is.na(totalKcalShell),
           forager.movement %in% c("local-patch-choice", "random"),
           n.foragers %in% c("40","60"),
           global.knowledge %in% c("true","false"),
           spatial.foresight %in% c("true","false"),
           ticks == "101") %>% 
    filter(!is.na(bs.run.number)) %>% 
    mutate(bs.run.number = as.numeric(bs.run.number)) %>% 
    dplyr::select(!c(1, tail(names(.), 1))) # removes the first and last columns ([])
  
  return(ds)
}

# 
# ds.Settings <- read_csv("QI_HPC_Sensitivity_Settings2.csv")
# 
# # Create a temporary dataset that has only the cells with projectile armatures
# ds.Settings <- ds.Settings %>%
#   filterBasicsSettings()
# 
# write.csv(ds, "QI_Settings_cleaned2.csv", row.names = F)

formatDsSettings <- function(ds){
  
  # Select the columns that have the relevant data for these analyses
  ds <- ds %>% 
    mutate(site = as.numeric(site)) %>% 
    # Transform some variables into clean factors
    mutate(n.camps = factor(n.camps, levels = c(5, 20), ordered = T),
           daily.time.budget = factor(daily.time.budget,levels = c(6, 8, 10), 
                                      ordered = T),
           hunter.percent = factor(hunter.percent,levels = c(0.3, 0.5),
                                   ordered = T),
           n.foragers=factor(n.foragers,levels = c(40, 60), 
                             ordered = T),
           vision.forager = factor(vision.forager,levels = c(10, 20),
                                   ordered = T),
           days.foresight = factor(days.foresight,levels = c(5,10),
                                   ordered = T),
           walk.speed = factor(walk.speed,levels = c(2,3),
                               ordered = T),
           forager.movement = factor(forager.movement),
           onsiteProcessThreshold = factor(onsiteProcessThreshold, levels = c(10, 30, 50), ordered=T),
           # Adding a variable that defines if a cell is coastal or not (to compare)
           coastal = ifelse(vegetation %in% c(10,11,12,13,14), TRUE, FALSE))
  
  return(ds)
  
}

ds.Settings <- read_csv("QI_Settings_cleaned2.csv")
ds.Settings <- formatDsSettings(ds.Settings)

########################
## GETTING RATIO FOOD ##
########################

# I took this from the QI_Coastal file

# This will calculate a value from the string in the dataset
getValue <- function(cell, value){
  
  a <- stringr::str_split(cell, " ", simplify = TRUE) %>% 
    unlist()   # Split the different values
  
  b <- as.numeric(a)
  
  return(value(b, na.rm = TRUE))
  
}

# Using mean KcalShell in coastal cells vs KcalShell in inland cells works.

CompileRatioPerHabitat <- function(ds){
  
  temp <- ds %>% 
    rowwise() %>% 
    mutate(lengthOccupation = (length(unlist(stringr::str_split(totalKcalMeat, " "))))/site,
           meanShell = getValue(totalKcalShell, mean),
           meanMeat = getValue(totalKcalMeat, mean),
           meanPlant = getValue(totalKcalPlant, mean)) %>% 
    ungroup() %>% 
    group_by(bs.run.number, ticks, coastal) %>% 
    summarise(meanLengthOccupation = mean(lengthOccupation),
              meanShellHabitat = mean(meanShell),
              meanMeatHabitat = mean(meanMeat),
              meanPlantHabitat = mean(meanPlant)) %>% 
    ungroup()
  
  # widens the values 
  temp2 <- temp %>% 
    pivot_wider(names_from = coastal, 
                values_from = c(meanShellHabitat, meanMeatHabitat,
                                meanPlantHabitat, meanLengthOccupation)) %>% 
    replace(is.na(.), 0) %>% # replace all NA by 0
    mutate(ratioShell = meanShellHabitat_TRUE / (meanShellHabitat_TRUE + meanShellHabitat_FALSE),
           ratioMeat = meanMeatHabitat_TRUE / (meanMeatHabitat_TRUE + meanMeatHabitat_FALSE),
           ratioPlant = meanPlantHabitat_TRUE / (meanPlantHabitat_TRUE + meanPlantHabitat_FALSE))
  
  return(temp2)
  
}

ds.Length <- CompileRatioPerHabitat(ds.Length)
ds.Repetitions <- CompileRatioPerHabitat(ds.Repetitions)

# Compile the kcal ratio per habitat per run (coastal or not)
ds <- CompileRatioPerHabitat(ds.Settings)

# Create a key dataset with unique bs.run.number

ds.keys <- ds.Settings %>%
  distinct(bs.run.number, .keep_all = T) %>%
  # Keep only the variable information (and kcal)
  select(bs.run.number, n.camps, spatial.foresight, daily.time.budget, hunter.percent, vision.forager, forager.movement, n.foragers, days.foresight, global.knowledge, walk.speed, onsiteProcessThreshold, mean.kcal)

# Then link the run number with its variables to plot
ds.Settings.full <- ds %>%
  left_join(ds.keys, by = "bs.run.number") %>% 
  mutate(ratioShell = ifelse(is.na(ratioShell), 0, ratioShell),
         ratioMeat = ifelse(is.na(ratioMeat), 0, ratioMeat),
         ratioPlant = ifelse(is.na(ratioPlant), 0, ratioPlant)) %>% 
  filter(bs.run.number != 0) # Troubleshooting

rm(ds.keys, ds)

# Saving it all as a RData file
save.image("FormattedSensitivity.RData")
