###############################################################################
############# Creating Nitrogen Budgets in Wetland plots ######################
###############################################################################

# Written by: Pearl Ruston and Sarah Herbert 
# Written for R version 4.3.1
# Contact email: pearl.ruston@vuw.ac.nz

#####
#### This is the R-code complementing Pearl Ruston's Masters thesis 
#####

# Recent Edit: 28_02_2025

# Set Working Directory
setwd("")

#Load required packages

library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(readxl)

#=== Load datasheets ===#
Biomass_Dryweight <- read_excel("insertdatasheet.xlsx") 
NIWA_Nitrogen <- read_excel("insertdatsheet.xlsx")
RootBiomass_Dryweight <- read_excel("RootBiomass_Pearl2024.xlsx") # Make sure R is BB
Subplot_species_data <- read_excel("SubplotSpeciesFoliarWeight.xlsx") # File created from FoliarBiomassCalc.RScript # make sure R is BB
ShannonBulkDensity <- read_excel("insertname.xlsx")
Mineral_Nitrogen <- read_excel("MineralN_11_24.xlsx") 
MicrobialLipidData <- read_excel("PLFAPearl_EachLipidsConc_NValue_ver2_09042025.xlsx", sheet = "Sheet1")


###############################################################################
################# 1.0: Herb Layer Nitrogen (aka Ground Biomass) ###############
###############################################################################

Combine_GroundBiomass_NIWAdata <- Biomass_Dryweight %>%
  left_join(NIWA_Nitrogen, by = c("Site_Code", "State", "Group", "Sample_Type"))


#Quantify herb layer dry weight (g) to the 1m2 subplot level
GroundBiomass_Dryweight_1m2quantified <- Biomass_Dryweight %>%
  mutate(AverageDryWeight1m2_g = AverageDryWeight_g*(1/0.04)) # 1m2, 20cm by 20cm = 400cm2 = 0.04m2

# Calculate nitrogen content for each row
GroundBiomassNcontent <- Combine_GroundBiomass_NIWAdata %>%
  mutate(Nitrogen_Content_g = AverageDryWeight_g * (`All Correct Amt %N Flash TCD` / 100))

# Summing herb layer nitrogen by subplot 20cm2 for each wetland
# Ground Biomass: (20cmx20cm subplot = 400cm2 * 9 = 3600cm2 = 0.36m2) # Summed Ground Biomass Sample Area
# Quantify to 1m2 plot level: 1m2/0.36m2 = 2.77778

GroundbiomassNitrogen_subplots <- GroundBiomassNcontent %>%
  group_by(Site_Code, State, Subplot_No) %>%
  summarise(TotalSubplot_Nitrogen_g = sum(Nitrogen_Content_g, na.rm = TRUE),
            GB1M2QuanFactor = 1/0.36)
view(GroundbiomassNitrogen_subplots)
GroundbiomassNitrogen_subplots1m2 <- GroundbiomassNitrogen_subplots  %>%
  mutate(TotalSubplot_Nitrogen_g1m2 = TotalSubplot_Nitrogen_g * GB1M2QuanFactor)
view(GroundbiomassNitrogen_subplots1m2)

######## 1.1: Quantify Herb Layer to the 20m by 20m plot level ################

# Ground Biomass: (20cmx20cm subplot = 400cm2 * 9 = 3600cm2 = 0.36m2) # Summed herb layer sample area
# Quantify to 20mx20m plot level: 400m2/0.36m2 = 1111.111111111111
# So sum of subplot ground biomass * 1111.111111111111 = total ground biomass N of plot.

#Quantifying Factor
Quantifying_FactorAGB <- (400/0.36)

# Quantifying GroundBiomass TN to the 20m2 plot level

TotalN_AGB <- GroundbiomassNitrogen_subplots %>%
  group_by(Site_Code, State) %>%
  summarise(TotalPlot_ABGNitrogen_g = sum(TotalSubplot_Nitrogen_g, na.rm = TRUE) * Quantifying_FactorAGB)

view(TotalN_AGB)
write_xlsx(TotalN_AGB, "insertname.xlsx")

###############################################################################
####################### 2.0: Root Nitrogen ####################################
###############################################################################

### Pre notes/explanation: 
# Dry weight of biomass is in grams. Root samples split into fine (<2mm) and coarse (>2mm) roots.
# Samples were taken at point 4, 5 and 6 and pooled for dry weight and %N analysis. 
# Root samples were taken with a 10cm deep, 5.5cm diameter soil core. 
###

view(RootBiomass_Dryweight) 

Combined_RootBiomass_Ndata <- RootBiomass_Dryweight %>%
  left_join(NIWA_Nitrogen, by = c("Site_Code", "State", "Group", "Sample_Type"))

# Calculate root nitrogen content for each row
RootBiomass_TNdata <- Combined_RootBiomass_Ndata %>%
  mutate(Nitrogen_Content_g = AverageDryWeight_g * (`All Correct Amt %N Flash TCD` / 100)) # Get total nitrogen of fine and coarse roots of each wetland

# Summing fine and coarse root biomass nitrogen for each plot (just summed the three samples)
totalN_root <- RootBiomass_TNdata %>%
  group_by(Site_Code, State) %>%
  summarise(SumRoot_Nitrogen_g = sum(Nitrogen_Content_g, na.rm = TRUE)) #Combined Fine and Coarse roots for each wetland

# view the result
view(totalN_root) 
write_xlsx(totalN_root, "Root_NIWA_N_g_3samplessummed.xlsx")

### Quantifying Root Biomass to the plot level ########

SampleRootArea <- (3 * (pi*((0.0275)^2))) # in Surface area in m2 

#Quantifying_factor_root
Quantifying_FactorRoot <- 400/SampleRootArea

TotalN_Plot_Root <- totalN_root %>%
  group_by(Site_Code, State) %>%
  summarise(TotalNPlotRoot_g = sum(SumRoot_Nitrogen_g, na.rm = TRUE) * Quantifying_FactorRoot)

view(TotalN_Plot_Root)

###############################################################################
####################### 3.0: Shrub and Canopy Nitrogen ########################
###############################################################################

# Filter niwa_data for "Restored" or "Remnant" in wetland_state and "Foliar" in Sample_Type
filtered_foliarniwa_data <- NIWA_Nitrogen %>%
  filter(State %in% c("Restored", "Remnant") & Sample_Type == "Foliar")

view(filtered_foliarniwa_data)

# Modify the filtered data so that "ShrubCanopy" entries are split into "Shrub" and "Canopy"
expanded_foliarniwa_data <- filtered_foliarniwa_data %>%
  # Identify rows with "ShrubCanopy" in the "Group" column and change them to "Shrub"
  mutate(Group = ifelse(Group == "ShrubCanopy", "Shrub", Group)) %>%
  # Create a new version of the "ShrubCanopy" rows labeled as "Canopy" only
  bind_rows(filtered_foliarniwa_data %>% filter(Group == "ShrubCanopy") %>% mutate(Group = "Canopy"))

# view the result
view(expanded_foliarniwa_data) 

# Check column names in expanded_foliarniwa_data to prepare for combining data
colnames(expanded_foliarniwa_data)

# Join datasets
combinedfoliar_data <- Subplot_species_data %>%
  inner_join(expanded_foliarniwa_data, by = c("Site_Code" = "Site_Code", "State" = "State", "Species_Group", "Layer" = "Group"))

view(combinedfoliar_data) 

# Calculate nitrogen content in grams for each species in each layer
combinedNfoliar_data <- combinedfoliar_data %>%
  mutate(Nitrogen_Content_g = SpeciesSubplot_FoliarWeight_g * (`All Correct Amt %N Flash TCD` / 100))

# Summing nitrogen by layer (Shrub and Canopy) for each subplot nitrogen
subplot_layer_nitrogen <- combinedNfoliar_data%>%
  group_by(Site_Code, State, Subplot_No., Layer) %>%
  summarise(Total_Layer_Nitrogen_g = sum(Nitrogen_Content_g, na.rm = TRUE))

# Summing all subplots for each plot (Wetland_Code) by layer
plot_layer_nitrogen <- subplot_layer_nitrogen %>%
  group_by(Site_Code, State, Layer) %>%
  summarise(Total_Plot_Layer_Nitrogen_g = sum(Total_Layer_Nitrogen_g, na.rm = TRUE)) 

# Summing shrub and canopy layers for total foliar nitrogen per plot
total_foliar_nitrogen_plot <- plot_layer_nitrogen %>%
  group_by(Site_Code, State) %>%
  summarise(Total_Foliar_Nitrogen_g = sum(Total_Plot_Layer_Nitrogen_g, na.rm = TRUE))

view (total_foliar_nitrogen_plot) # note this result is only for the summation of 9 x 1m2 subplots, not the whole plot.
write_xlsx(total_foliar_nitrogen_plot, "Foliar_NIWA_N_g.xlsx")


############# Quantify Shrub and Canopy Foliar Layer to the 20m by 20m plot level #################

# SampleArea: (1m2 * 9 = 9m2)
# Quantify to 20mx20m (400m2) plot level: 400m2/9m2 = 44.44444444
# So sum of subplot ground biomass * 44.44444444 = total Foliar (Shrub+Canopy) Biomass for 20mx20m plot 

#Quantifying Factor
Quantifying_Factor_ShrubCanopy <- (400/9)

#Quantifying GroundBiomass TN to the 20m2 plot level
TotalN_FoliarBiomass <- total_foliar_nitrogen_plot %>%
  group_by(Site_Code, State) %>%
  summarise(TotalPlot_Foliar_g = sum(Total_Foliar_Nitrogen_g, na.rm = TRUE) * Quantifying_Factor_ShrubCanopy)

view(TotalN_FoliarBiomass)
write_xlsx(TotalN_FoliarBiomass, "Foliar_NIWA_N_g_plotlevel.xlsx")

###############################################################################
################# 3.1: Special Case: Wetland Restored G ########################
###############################################################################

###### Special Case: Wetland Restored G was treated like Unrestored, but had 7 plantings (PHOten, CARsec) that need to be included in it's N budget
  
G_PHOten_TN <- 1.4496516
G_CARsec_TN <- 1.642267

G_PHOten_LMA <- 0.000151532932374945 #g/m2
G_CARsec_LMA <- 0.0000547076023391813 #g/m2

G_CARsec_PlantingA_Area <- (2 * pi * 0.165 * 1.3) + (2 * pi * (0.165^2))  # in meters using field measurements basal diameter (330mm) and height (1.3m)
G_CARsec_PlantingB_Area <- (2 * pi * 0.135 * 1.6) + (2 * pi * (0.135^2))  # in meters using field measurements basal diameter (270mm) and height (1.6m) 

Biomass_G_CARsec_PlantingA <- G_CARsec_PlantingA_Area * G_CARsec_LMA  
Biomass_G_CARsec_PlantingB <- G_CARsec_PlantingB_Area * G_CARsec_LMA 

G_RES_CARsec_TN <- (Biomass_G_CARsec_PlantingA * (G_CARsec_TN/100)) + (Biomass_G_CARsec_PlantingB * (G_CARsec_TN/100))


G_PHOten_PlantingG_Area <- (((0.7*(0.025/4))/2)*4) # Field Measurements: 0.7m height, 0.025m basal diameter, #4 leaves
G_PHOten_PlantingC_Area <-  (((0.9*(0.1/6))/2)*6) # 0.9m height, 0.1m basal diameter, #6 leaves
G_PHOten_PlantingD_Area <-  (((0.65*(0.025/4))/2)*4) # 0.65m height, 0.025m basal diameter, #4 leaves
G_PHOten_PlantingE_Area <-  (((1*(0.05/8))/2)*8) # 1m height, 0.5m basal diameter, #8 leaves
G_PHOten_PlantingF_Area <-  (((1*(0.05/3))/2)*3) # 1m height, 0.5m basal diameter, #3 leaves
#Area of triangle was used to calculate PHOten's leaf area. I found base of PHO ten leaves by dividing the basal diameter by the number of leaves. We field measured length/height of leaf value. 
Biomass_G_PHOten_PlantingG <-  G_PHOten_PlantingG_Area *G_PHOten_LMA # IN GRAM
Biomass_G_PHOten_PlantingC <-  G_PHOten_PlantingC_Area *G_PHOten_LMA
Biomass_G_PHOten_PlantingD <-  G_PHOten_PlantingD_Area *G_PHOten_LMA
Biomass_G_PHOten_PlantingE <-  G_PHOten_PlantingE_Area *G_PHOten_LMA
Biomass_G_PHOten_PlantingF <-  G_PHOten_PlantingF_Area *G_PHOten_LMA

G_RES_PHOten_TN <- (Biomass_G_PHOten_PlantingG * (G_PHOten_TN/100)) + (Biomass_G_PHOten_PlantingC * (G_PHOten_TN/100)) + (Biomass_G_PHOten_PlantingD * (G_PHOten_TN/100)) + (Biomass_G_PHOten_PlantingE * (G_PHOten_TN/100)) + (Biomass_G_PHOten_PlantingF * (G_PHOten_TN/100))  

G_RES_PlantingBiomass <- Biomass_G_CARsec_PlantingA + Biomass_G_CARsec_PlantingB + Biomass_G_PHOten_PlantingG + Biomass_G_PHOten_PlantingC + Biomass_G_PHOten_PlantingD + Biomass_G_PHOten_PlantingE + Biomass_G_PHOten_PlantingF
  
###############################################################################
####################### 4.0: Soil Nitrogen ####################################
###############################################################################

###======== 4.1: Finding Bulk Density Values for 0m, 10m and 20m #####

### See Shannon Bentley's thesis Figure:2.8B -> https://openaccess.wgtn.ac.nz/articles/thesis/Quantifying_soil_microbial_and_plant_community_changes_following_wetland_restoration_on_private_land/17104400?file=31626536 ======#####
##### Step 1: Create a Scatter Plot #######

# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Load the data
ShannonBulkDensity <- read_excel("insertname.xlsx")

# Filter data for RES and UNR Treatment_Pearl
res_data <- ShannonBulkDensity %>% filter(Treatment_Pearl == "RES")
unr_data <- ShannonBulkDensity %>% filter(Treatment_Pearl == "UNR")

# Create scatter plot with regression lines
scatter_plot <- ggplot() +
  # Scatter plot and regression line for RES
  geom_point(data = res_data, aes(x = Distance_Pearl, y = Bulk_density), color = "blue", alpha = 0.7) +
  geom_smooth(data = res_data, aes(x = Distance_Pearl, y = Bulk_density), method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  # Scatter plot and regression line for UNR
  geom_point(data = unr_data, aes(x = Distance_Pearl, y = Bulk_density), color = "red", alpha = 0.7) +
  geom_smooth(data = unr_data, aes(x = Distance_Pearl, y = Bulk_density), method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  # Add axis labels and title
  labs(
    x = "Distance (Pearl)",
    y = "Bulk Density (g/cm3)",
    title = "Scatter Plot of Shannon's BD values w/ Linear Fits",
    subtitle = "Blue = Restored, Red = Unrestored"
  ) +
  theme_minimal()

# Print the plot
print(scatter_plot) 

##### Step 2: Extract Linear Line Equation #######

# Fit linear models to extract slope and intercept # the line of best fit seen in the scatter plot. 
res_model <- lm(Bulk_density ~ Distance_Pearl, data = res_data)
unr_model <- lm(Bulk_density ~ Distance_Pearl, data = unr_data)

# Extract coefficients (slope and intercept) from the models
res_coeff <- coef(res_model)
unr_coeff <- coef(unr_model)

res_coeff
unr_coeff
######## RESULTS #########
#> coef(res_model)
#(Intercept) Distance_Pearl 
#0.41861454     0.01881806 
#> unr_model
#Call:
#lm(formula = Bulk_density ~ Distance_Pearl, data = unr_data)
#Coefficients:
#(Intercept)  Distance_Pearl  
#0.746622        0.004852  

##### Bulk Density values for x= 0m, 10m, 20m  # y = mx + c # g per cm3
RES_LOW_BD_lm <- 0.01881806*0+0.41861454
RES_MID_BD_lm <- 0.01881806*10+0.41861454
RES_HIGH_BD_lm <- 0.01881806*20+0.41861454
UNR_LOW_BD_lm <- 0.004852*0+0.746622
UNR_MID_BD_lm <- 0.004852*10+0.746622
UNR_HIGH_BD_lm <- 0.004852*20+0.746622
###############################################################################

#### Soil Core Size (cm^3)
# Soil samples were taken with a 10cm deep, 5.5cm diameter soil core
SoilCoreVolume <- pi*(2.75^2)*10  # in cm^3 #Volume
SurfaceSoilCoreArea = pi*((0.0275)^2) #in m2 # for plot level
## BD (Bulk Density) values are dry weight. Confirmed pg 26 of Shannon's thesis

####============== Calculating Total Nitrogen Soil ===============#####

# Filter rows where Sample_Type is "Soil"
soil_data <- NIWA_Nitrogen[NIWA_Nitrogen$Sample_Type == "Soil", ]

# view the filtered data
view(soil_data)

# Step 1: Create a new column "SoilCoreVolume_cm3"
soil_data$SoilCoreVolume_cm3 <- SoilCoreVolume # Add the value to a new column

# Step 2: Create a new column "Bulk_Density_LM"
soil_data$Bulk_Density_LM <- NA # Initialize column with NA values

# Assign values for Restored Wetland_Type
soil_data$Bulk_Density_LM <- with(soil_data, ifelse(
  State == "Restored" & Soil_Point_No %in% c(3, 6, 9), RES_LOW_BD_lm,
  ifelse(
    State == "Restored" & Soil_Point_No %in% c(2, 5, 8), RES_MID_BD_lm,
    ifelse(
      State == "Restored" & Soil_Point_No %in% c(1, 4, 7), RES_HIGH_BD_lm,
      Bulk_Density_LM # Keep existing value (NA or previous assignment)
      )
      )
      ))

# Assign values for Unrestored Wetland_Type
soil_data$Bulk_Density_LM <- with(soil_data, ifelse(
  State == "Unrestored" & Soil_Point_No %in% c(3, 6, 9), UNR_LOW_BD_lm,
  ifelse(
    State == "Unrestored" & Soil_Point_No %in% c(2, 5, 8), UNR_MID_BD_lm,
    ifelse(
      State == "Unrestored" & Soil_Point_No %in% c(1, 4, 7), UNR_HIGH_BD_lm,
      Bulk_Density_LM # Keep existing value (NA or previous assignment)
    )
  )
))

# Assign values for Remnant Wetland_Type (use RES and assuming BD of RES and REM are the same)
soil_data$Bulk_Density_LM <- with(soil_data, ifelse(
  State == "Remnant" & Soil_Point_No %in% c(3, 6, 9), RES_LOW_BD_lm,
  ifelse(
    State == "Remnant" & Soil_Point_No %in% c(2, 5, 8), RES_MID_BD_lm,
    ifelse(
      State == "Remnant" & Soil_Point_No %in% c(1, 4, 7), RES_HIGH_BD_lm,
      Bulk_Density_LM # Keep existing value (NA or previous assignment)
    )
  )
))

view (soil_data) 

### Calculating Each Soil Cores Dry weight (BD(g.cm3) * Core Size(cm3))
soil_data$SoilCoreWeight_g <- (soil_data$Bulk_Density_LM * soil_data$SoilCoreVolume_cm3) #now I have a gram value of each core.
view(soil_data)

#### Calculating each cores Nitrogen weight (grams) ((Core Weight * Nitrogen %) / 100)
# Create the new column "SoilCoreN_g" with the calculated values
soil_data$SoilCoreN_g <- (soil_data$SoilCoreWeight_g * soil_data$`All Correct Amt %N Flash TCD`) / 100
view(soil_data) 

# Sum SoilCoreMicrobialN_ug by Sample_ID, Wetland_Type
soil_datasum <- soil_data %>%
  group_by(Site_Code, State) %>%
  summarise(
    SummedSoilCoreN_g = sum(SoilCoreN_g, na.rm = TRUE), # Sum across cores 
    RowCount = n(), # Count the number of cores summed
    SurfaceSoilCoreArea = pi*((0.0275)^2), # in m2
    TotalSurfaceSoilCoreArea = SurfaceSoilCoreArea*RowCount, #in m2. 
    SoilTNQuantifyingFactor = 400/TotalSurfaceSoilCoreArea
  ) 
view(soil_datasum)

####### Summing soil total nitrogen to the 20m2 (400m2) plot level ########
### Getting Soil Surface Area
TotalSurfaceSoilCoreArea <- soil_datasum$SurfaceSoilCoreArea*soil_datasum$RowCount
SoilTNQuantifyingFactor <- 400/TotalSurfaceSoilCoreArea #what I need to multiply the summed 9 points by inorder to quantify to 20m plot level

# Total Plot Soil Nitrogen: Summing SoilCoreN_g by Wetland_Code and Wetland_Type, then multiplying by SoilTNQuantifyingFactor
Wetland_TNSoil_Sums <- soil_datasum %>%
  mutate(Total_PlotSoilTN_g = SummedSoilCoreN_g * SoilTNQuantifyingFactor)

# view the result
view(Wetland_TNSoil_Sums)
write.xlsx(Wetland_TNSoil_Sums, "Wetland_PlotTNSoil_Sums.xlsx", rowNames = FALSE)

###############################################################################
####################### 5.0: Soil Mineral Nitrogen ############################
###############################################################################

#Upload Mineral Nitrogen Spreadsheet 
Mineral_Nitrogen <- read_excel("MineralN_11_24.xlsx") 

## Create a new column "Soil Volume cm3"
Mineral_Nitrogen$SoilCoreVolume_cm3 <- SoilCoreVolume # Add the value to a new column

## Create a new column "Bulk_Density_LM"
Mineral_Nitrogen$Bulk_Density_LM <- NA # Initialize column with NA values

  #Assign values for Restored Wetland_Type
Mineral_Nitrogen$Bulk_Density_LM <- with(Mineral_Nitrogen, ifelse(
  Wetland_Type == "Restored" & Soil_Point_No %in% c(3, 6, 9), RES_LOW_BD_lm,
  ifelse(
    Wetland_Type == "Restored" & Soil_Point_No %in% c(2, 5, 8), RES_MID_BD_lm,
    ifelse(
      Wetland_Type == "Restored" & Soil_Point_No %in% c(1, 4, 7), RES_HIGH_BD_lm,
      Bulk_Density_LM # Keep existing value (NA or previous assignment)
    )
  )
))

# Assign values for Unrestored Wetland_Type
Mineral_Nitrogen$Bulk_Density_LM <- with(Mineral_Nitrogen, ifelse(
  Wetland_Type == "Unrestored" & Soil_Point_No %in% c(3, 6, 9), UNR_LOW_BD_lm,
  ifelse(
    Wetland_Type == "Unrestored" & Soil_Point_No %in% c(2, 5, 8), UNR_MID_BD_lm,
    ifelse(
      Wetland_Type == "Unrestored" & Soil_Point_No %in% c(1, 4, 7), UNR_HIGH_BD_lm,
      Bulk_Density_LM # Keep existing value (NA or previous assignment)
    )
  )
))

# Assign values for Remnant Wetland_Type (use RES and assuming BD of RES and REM are the same)
Mineral_Nitrogen$Bulk_Density_LM <- with(Mineral_Nitrogen, ifelse(
  Wetland_Type == "Remnant" & Soil_Point_No %in% c(3, 6, 9), RES_LOW_BD_lm,
  ifelse(
    Wetland_Type == "Remnant" & Soil_Point_No %in% c(2, 5, 8), RES_MID_BD_lm,
    ifelse(
      Wetland_Type == "Remnant" & Soil_Point_No %in% c(1, 4, 7), RES_HIGH_BD_lm,
      Bulk_Density_LM # Keep existing value (NA or previous assignment)
    )
  )
))

view(Mineral_Nitrogen)

### Calculating Each Soil Cores Dry weight (BD(g.cm3) * Core Size(cm3))
Mineral_Nitrogen$SoilCoreWeight_g <- (Mineral_Nitrogen$Bulk_Density_LM * Mineral_Nitrogen$SoilCoreVolume_cm3)

## Sum NH4 and NO3 values for each row
Mineral_Nitrogen$MineralN <- (Mineral_Nitrogen$NO3Ndriedsoil_µgg_1 + Mineral_Nitrogen$NH4Ndriedsoil_µgg_1)

## Calculating each cores Mineral Nitrogen weight (in ug) 
Mineral_Nitrogen$SoilCoreMineralN_ug <- (Mineral_Nitrogen$SoilCoreWeight_g * Mineral_Nitrogen$MineralN)

SoilMineralN <- Mineral_Nitrogen
write_xlsx(SoilMineralN, "SoilMineralN_ug.xlsx")
view(SoilMineralN)

### NO3-N Core Value
Mineral_Nitrogen$NO3N_Core_ug <- (Mineral_Nitrogen$NO3Ndriedsoil_µgg_1 * Mineral_Nitrogen$SoilCoreWeight_g)
### NH4-N Core Value
Mineral_Nitrogen$NH4N_Core_ug <- (Mineral_Nitrogen$NH4Ndriedsoil_µgg_1 * Mineral_Nitrogen$SoilCoreWeight_g)
view(Mineral_Nitrogen)

#Core Level
write.xlsx(Mineral_Nitrogen, "Wetland_MineralNSoil_corelevel.xlsx", rowNames = FALSE)


### Calc Mineral Nitrogen for soil core samples and summing soil cores 
# Sum SoilCoreMicrobialN_ug by Sample_ID, Wetland_Type
Mineralcoresample_summary <- Mineral_Nitrogen %>%
  group_by(Site_Code, Wetland_Type) %>%
  summarise(
    TotalCoreSampleMineralNvalue_ug = sum(SoilCoreMineralN_ug, na.rm = TRUE), # Sum across cores # So now have sum of microbial nitrogen of all cores taken in each wetland
    RowCount = n(), # Count the number of cores summed
    NO3NCoreSampleMineralNvalue_ug = sum(NO3N_Core_ug, na.rm = TRUE),
    NH4NCoreSampleMineralNvalue_ug = sum(NH4N_Core_ug, na.rm = TRUE)
  ) 
  
view(Mineralcoresample_summary)

######## Quantifying Mineral Nitrogen to the plot level ##############

#Add column for SurfaceAreaSoil core (m2)
Mineralcoresample_summary$SurfaceSoilCoreAream2 <- pi*((0.0275)^2) # in m2
## Add Column for each sites TotalSurfaceSoilCoreArea
Mineralcoresample_summary$TotalSurfaceSoilCoreArea <- Mineralcoresample_summary$SurfaceSoilCoreAream2*Mineralcoresample_summary$RowCount # Row count as not all sites had 9 samples taken
### Add Column for Quantfiying Factor
Mineralcoresample_summary$SoilMineralNQuantifyingFactor <- 400/Mineralcoresample_summary$TotalSurfaceSoilCoreArea #what I need to multiply the summed sample core points by... inorder to quantify to 20m plot level

view(Mineralcoresample_summary)

# Total Plot Microbial Soil Nitrogen: Summing TotalSampleMineralN by Wetland_Code and Wetland_Type, then multiplying by SoilMicrobialNQuantifyingFactor
Wetland_MineralNSoil_Plot <- Mineralcoresample_summary %>%
  group_by(Site_Code, Wetland_Type) %>%
  mutate(Total_PlotMineralSoilN_ug = TotalCoreSampleMineralNvalue_ug * SoilMineralNQuantifyingFactor,
         NO3N_PlotMineralN_ug = NO3NCoreSampleMineralNvalue_ug * SoilMineralNQuantifyingFactor,
         NH4N_PlotMineralN_ug = NH4NCoreSampleMineralNvalue_ug * SoilMineralNQuantifyingFactor) # in ug

view (Wetland_MineralNSoil_Plot)

# Convert Total_PlotMicrobialSoilN_ug from micrograms to grams
Wetland_MineralNSoil_Plot <- Wetland_MineralNSoil_Plot %>%
  mutate(Total_PlotMineralSoilN_g = Total_PlotMineralSoilN_ug / 1e6,
         NO3N_PlotMineralSoilN_g = NO3N_PlotMineralN_ug / 1e6,
         NH4N_PlotMineralSoilN_g = NH4N_PlotMineralN_ug / 1e6) # Divide by 10^6

# view the updated dataset
view(Wetland_MineralNSoil_Plot)
write.xlsx(Wetland_MineralNSoil_Plot, "Wetland_MineralNSoil_Plot.xlsx", rowNames = FALSE)
###############################################################################
####################### 6.0: Soil Microbial Nitrogen ############################
###############################################################################
# C:N ratios = BACTERIA (6:1), FUNGI (16:1)

##### Getting the sheet ready ############################
# Step 1: Load data
MicrobialLipidData <- read_excel("PLFAPearl_EachLipidsConc_NValue_ver2_09042025.xlsx", sheet = "Sheet1")

# Step 2: Remove rows where FvsB is "NAFA"
MicrobialLipidData_clean <- MicrobialLipidData %>%
  filter(FvsB != "NAFA")

# Step 3: Create Carbon_No column using a named vector for mapping
carbon_map <- c(
  "10Me16:0" = 16, "10Me17:0" = 17, "10Me18:0" = 18,
  "14:0" = 14, "15:0" = 15, "16:0" = 16,
  "16:1w5" = 16, "16:1w7c" = 16, "16:1w7t" = 16,
  "17:0" = 17, "18:0" = 18, "18:1w7c" = 18,
  "18:1w7t" = 18, "18:1w9c" = 18, "18:1w9t" = 18,
  "18:2w6" = 18, "19:1w9c" = 19,
  "a15:0" = 15, "a17:0" = 17,
  "delta17:0" = 17, "delta19:0" = 19,
  "i15:0" = 15, "i16:0" = 16, "i17:0" = 17
)

MicrobialLipidData_clean <- MicrobialLipidData_clean %>%
  mutate(Carbon_No = carbon_map[Name])

# Step 4: Add Carbon_MolarMass_gmol (same value for all)
MicrobialLipidData_clean <- MicrobialLipidData_clean %>%
  mutate(Carbon_MolarMass_gmol = 12.0107)

# Step 5: Calculate LipidCarbonWeight_gmol
MicrobialLipidData_clean <- MicrobialLipidData_clean %>%
  mutate(LipidCarbonWeight_gmol = Carbon_No * Carbon_MolarMass_gmol)

# Step 6: Create C:N ratio based on FvsB
MicrobialLipidData_clean <- MicrobialLipidData_clean %>%
  mutate(C_Nratio = case_when(
    FvsB == "Bacteria" ~ 6,
    FvsB == "Fungi" ~ 16,
    TRUE ~ NA_real_
  ))

# Step 7: Calculate Number of Nitrogen Atoms per Lipid
MicrobialLipidData_clean <- MicrobialLipidData_clean %>%
  mutate(Number_of_Nitrogen_Atoms_per_Lipid = Carbon_No / C_Nratio)

# Step 8: Add Nitrogen_MW (same value for all)
MicrobialLipidData_clean <- MicrobialLipidData_clean %>%
  mutate(Nitrogen_MW = 14.0067)

# Step 9:
MicrobialLipidData_clean <- MicrobialLipidData_clean %>%
mutate(Nitrogen_Weight_per_Lipid_gmol = Number_of_Nitrogen_Atoms_per_Lipid * Nitrogen_MW)

#Step 10:
MicrobialLipidData_clean <- MicrobialLipidData_clean %>%
  mutate(N_WeightinSample_gperg = (conc_FA/1000000000)*Nitrogen_Weight_per_Lipid_gmol)

#Step 11: Convert to ug
MicrobialLipidData_clean <- MicrobialLipidData_clean %>%
  mutate(N_WeightinSampleugg = N_WeightinSample_gperg*1000000)

write_xlsx(MicrobialLipidData_clean, "MicrobialNitrogenSampleLipiddata_ugperg.xlsx")

#Step 12: Adding in soil core weight in grams
##### Bulk Density values for x= 0m, 10m, 20m  # y = mx + c
RES_LOW_BD_lm <- 0.01881806*0+0.41861454
RES_MID_BD_lm <- 0.01881806*10+0.41861454
RES_HIGH_BD_lm <- 0.01881806*20+0.41861454
UNR_LOW_BD_lm <- 0.004852*0+0.746622
UNR_MID_BD_lm <- 0.004852*10+0.746622
UNR_HIGH_BD_lm <- 0.004852*20+0.746622

SoilCoreVolume <- pi*(2.75^2)*10  # in cm^3 #Volume

#### Creating/Calculatingn each soil cores weight

# Step 1: Create a new column "SoilCoreVolume_cm3"
MicrobialLipidData_clean$SoilCoreVolume_cm3 <- SoilCoreVolume # Add the value to a new column

# Step 2: Create a new column "Bulk_Density_LM"
MicrobialLipidData_clean$Bulk_Density_LM <- NA # Initialize column with NA values

# Assign values for Restored Wetland_Type
MicrobialLipidData_clean$Bulk_Density_LM <- with(MicrobialLipidData_clean, ifelse(
  State == "Restored" & Point_No %in% c(3, 6, 9), RES_LOW_BD_lm,
  ifelse(
    State == "Restored" & Point_No %in% c(2, 5, 8), RES_MID_BD_lm,
    ifelse(
      State == "Restored" & Point_No %in% c(1, 4, 7), RES_HIGH_BD_lm,
      Bulk_Density_LM # Keep existing value (NA or previous assignment)
    )
  )
))

# Assign values for Unrestored Wetland_Type
MicrobialLipidData_clean$Bulk_Density_LM <- with(MicrobialLipidData_clean, ifelse(
  State == "Unrestored" & Point_No %in% c(3, 6, 9), UNR_LOW_BD_lm,
  ifelse(
    State == "Unrestored" & Point_No %in% c(2, 5, 8), UNR_MID_BD_lm,
    ifelse(
      State == "Unrestored" & Point_No %in% c(1, 4, 7), UNR_HIGH_BD_lm,
      Bulk_Density_LM # Keep existing value (NA or previous assignment)
    )
  )
))

# Assign values for Remnant Wetland_Type (use RES and assuming BD of RES and REM are the same)
MicrobialLipidData_clean$Bulk_Density_LM <- with(MicrobialLipidData_clean, ifelse(
  State == "Remnant" & Point_No %in% c(3, 6, 9), RES_LOW_BD_lm,
  ifelse(
    State == "Remnant" & Point_No %in% c(2, 5, 8), RES_MID_BD_lm,
    ifelse(
      State == "Remnant" & Point_No %in% c(1, 4, 7), RES_HIGH_BD_lm,
      Bulk_Density_LM # Keep existing value (NA or previous assignment)
    )
  )
))

### Calculating Each Soil Cores Dry weight (BD(g.cm3) * Core Size(cm3))
MicrobialLipidData_clean$SoilCoreWeight_g <- (MicrobialLipidData_clean$Bulk_Density_LM * MicrobialLipidData_clean$SoilCoreVolume_cm3) #now I have a gram value of each core.
view(MicrobialLipidData_clean)

## Calculating each cores Microbial Nitrogen weight (in ug) 
#Cornvet nmol/ug

# Create the new column "SoilCoreN_g" with the calculated values
MicrobialLipidData_clean$SoilCoreMicrobialN_ug <- (MicrobialLipidData_clean$SoilCoreWeight_g * MicrobialLipidData_clean$N_WeightinSampleugg) #Microbial Nitrogen weight of each soil core
view(MicrobialLipidData_clean)
write_xlsx(MicrobialLipidData_clean, "MicrobialNitrogen_ug.xlsx")


##### Calc Microbial Nitrogen for soil core samples and summing soil cores #### 

# Sum SoilCoreMicrobialN_ug by Sample_ID, Wetland_Type, and Soil_Point_No
Microbialcoresample_summary <- MicrobialLipidData_clean %>%
  group_by(Site_Code, State, Point_No, Plot_Code, Elevation) %>%
  summarise(
    TotalCoreSampleMicrobialNvalue_ug_g = sum(SoilCoreMicrobialN_ug, na.rm = TRUE) # Sum within each core
  )
# Write into CSV for reference
write.xlsx(Microbialcoresample_summary, "MicrobialCore_Level_Summary.xlsx", rowNames = FALSE) 

# Sum TotalCoreSampleMicrobialNvalue by Sample_ID and Wetland_Type
microbial_sample_level_summary <- Microbialcoresample_summary %>%
  group_by(Site_Code, State) %>%
  summarise(
    TotalSampleMicrobialN = sum(TotalCoreSampleMicrobialNvalue, na.rm = TRUE), 
    RowCount = n() # Count the number of cores summed
  )

# view the results
view(microbial_sample_level_summary) # view allcore-level summary

################ Quantifying Microbial Nitrogen to the plot level #############

#Add column for SurfaceAreaSoil core (m2)
microbial_sample_level_summary$SurfaceSoilCoreAream2 <- pi*((0.0275)^2) # in m2
## Add Column for each sites TotalSurfaceSoilCoreArea
microbial_sample_level_summary$TotalSurfaceSoilCoreArea <- microbial_sample_level_summary$SurfaceSoilCoreAream2*microbial_sample_level_summary$RowCount # Row count as not all sites had 9 samples taken
### Add Column for Quantfiying Factor
microbial_sample_level_summary$SoilMicrobialNQuantifyingFactor <- 400/microbial_sample_level_summary$TotalSurfaceSoilCoreArea #what I need to multiply the summed sample core points by... inorder to quantify to 20m plot level

view(microbial_sample_level_summary)

# Total Plot Microbial Soil Nitrogen: Summing TotalSampleMicrobialN by Wetland_Code and Wetland_Type, then multiplying by SoilMicrobialNQuantifyingFactor
Wetland_MicrobialNSoil_Plot <- microbial_sample_level_summary %>%
  group_by(Site_Code, State) %>%
  mutate(Total_PlotMicrobialSoilN_ug = TotalSampleMicrobialN * SoilMicrobialNQuantifyingFactor) # in ug

view (Wetland_MicrobialNSoil_Plot)

# Convert Total_PlotMicrobialSoilN_ug from micrograms to grams
Wetland_MicrobialNSoil_Plot <- Wetland_MicrobialNSoil_Plot %>%
  mutate(Total_PlotMicrobialSoilN_g = Total_PlotMicrobialSoilN_ug / 1e6) # Divide by 10^6

# view the updated dataset
view(Wetland_MicrobialNSoil_Plot)

write.xlsx(Wetland_MicrobialNSoil_Plot, "Wetland_MicrobialNSoil_Plot.xlsx", rowNames = FALSE)

################################################################################
################################################################################
###############################################################################
###############################################################################

###############################################################################
################ 7.0: Summing N pools | Nitrogen Budget  ####################
###############################################################################

# Rename columns in TotalN_FoliarBiomass
TotalN_FoliarBiomass <- TotalN_FoliarBiomass %>%
  rename(Site_Code = Wetland_Code, State = Wetland_Type)

Wetland_MicrobialNSoil_Plot <- Wetland_MicrobialNSoil_Plot %>%
  rename(Site_Code = Site_Code, State = State)

Wetland_MineralNSoil_Plot <- Wetland_MineralNSoil_Plot %>%
  rename(Site_Code = Wetland_Code, State = Wetland_Type)

Wetland_TNSoil_Sums <- Wetland_TNSoil_Sums %>%
  rename(Site_Code = Wetland_Code, State = State)

TotalN_AGB <- TotalN_AGB %>%
  rename(Site_Code = Wetland_Code, State = Wetland_Type)

TotalN_Plot_Root <- TotalN_Plot_Root %>%
  rename(Site_Code = Site_Code, State = State)

# Soil Data Tables ( Plot Level ( 20m by 20m) pools in grams)
view (Wetland_MicrobialNSoil_Plot)
view (Wetland_MineralNSoil_Plot)
view (Wetland_TNSoil_Sums)
view (TotalN_AGB)
view (TotalN_Plot_Root)
view (TotalN_FoliarBiomass)

# Merge datasets by Wetland_Code and Wetland_Type
Npoolsmerged_data <- TotalN_AGB %>%
  full_join(TotalN_Plot_Root, by = c("Site_Code", "State")) %>%
  full_join(TotalN_FoliarBiomass, by = c("Site_Code", "State")) %>%
  full_join(Wetland_MicrobialNSoil_Plot, by = c("Site_Code", "State")) %>%
  full_join(Wetland_MineralNSoil_Plot, by = c("Site_Code", "State")) %>%
  full_join(Wetland_TNSoil_Sums, by = c("Site_Code", "State"))


##### Adding RES Wetland G PHOten and CARsec values #########

  # Extract the row for Wetland_Code "G" and Wetland_Type "Restored"
  G_restored_row <- Npoolsmerged_data %>%
  filter(Site_Code == "G", State == "Restored")

# Update the TotalPlot_Foliar_g value by adding G_RES_PHOten_TN and G_RES_CARsec_TN
G_restored_row <- G_restored_row %>%
  mutate(TotalPlot_Foliar_g = G_RES_PHOten_TN + G_RES_CARsec_TN)
view (G_restored_row)

# Update the main dataset by removing the old row and adding the updated row
Npoolsmerged_data_updated <- Npoolsmerged_data %>%
  filter(!(Site_Code == "G" & State == "Restored")) %>%
  bind_rows(G_restored_row)

# view the updated dataset
view(Npoolsmerged_data_updated)
 
write_xlsx(Npoolsmerged_data_updated, "All_TN_Pools_Pearl1.xlsx")


################################################################################
################################################################################
###############################################################################
###############################################################################