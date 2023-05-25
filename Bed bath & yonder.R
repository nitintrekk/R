#' Author: Nitin Madduri
#' Title: EDA in R
#' Purpose: EDA Analysis of loyal customers of BBY
#' Date: Mar 29, 2023


## Set the working directory
setwd('~/Hult_R/personalFiles')

# Libs
library(ggthemes)
library(ggplot2)
library(powerjoin)
library(dplyr)
library(purrr)
library(radiant)
library(DataExplorer)
library(naniar)
library(ggplot2)

# Get the inhouse data as `households`
households <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/inHouse_EDA_10k.csv')

# Load all of the supplemental data sources
consumer <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/consumerData_training15K_studentVersion.csv')


donations <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/DonationsData_training15K_studentVersion.csv')


magazine <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/magazineData_training15K_studentVersion.csv')


political <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/politicalData_training15K_studentVersion.csv')


mergedDF <- left_join(households, consumer, by = "tmpID")

mergedDF <- reduce(list(consumer, donations, magazine, political), 
                      function(x, y) left_join(x, y, by = "tmpID"))
bbyLoyalty <- left_join(households, mergedDF, by = "tmpID")

# Replace blank cells with the mode in the column HomeOwnerRenter
mode_val <- names(sort(table(bbyLoyalty$HomeOwnerRenter), decreasing = TRUE))[1]
bbyLoyalty$HomeOwnerRenter <- ifelse(bbyLoyalty$HomeOwnerRenter == "", mode_val, bbyLoyalty$HomeOwnerRenter)

# Replace blank cells with the mode in the column PresenceOfChildrenCode
mode_val <- names(sort(table(bbyLoyalty$PresenceOfChildrenCode), decreasing = TRUE))[1]
bbyLoyalty$PresenceOfChildrenCode <- ifelse(bbyLoyalty$PresenceOfChildrenCode == "", mode_val, bbyLoyalty$PresenceOfChildrenCode)

# Replace blank cells with the mode in the column ResidenceHHGenderDescription
mode_val <- names(sort(table(bbyLoyalty$ResidenceHHGenderDescription), decreasing = TRUE))[1]
bbyLoyalty$ResidenceHHGenderDescription <- ifelse(bbyLoyalty$ResidenceHHGenderDescription == "Cannot Determine", mode_val, bbyLoyalty$ResidenceHHGenderDescription)

# Replace blank cells with the mode in the column Gender
mode_val <- names(sort(table(bbyLoyalty$Gender), decreasing = TRUE))[1]
bbyLoyalty$Gender <- ifelse(bbyLoyalty$Gender == "", mode_val, bbyLoyalty$Gender)

# Replace blank cells with the mode in the column PropertyType
mode_val <- names(sort(table(bbyLoyalty$PropertyType), decreasing = TRUE))[1]
bbyLoyalty$PropertyType <- ifelse(bbyLoyalty$PropertyType == "Unknown", mode_val, bbyLoyalty$PropertyType)

# Replace blank cells with No instead of Unknown in the column ComputerOwnerInHome
bbyLoyalty$ComputerOwnerInHome <- ifelse(bbyLoyalty$ComputerOwnerInHome == "Unknown", "No", bbyLoyalty$ComputerOwnerInHome)

# Replace blank cells with the mode in the column PropertyType
mode_val <- names(sort(table(bbyLoyalty$PropertyType), decreasing = TRUE))[1]
bbyLoyalty$PropertyType <- ifelse(bbyLoyalty$PropertyType == "Unknown", mode_val, bbyLoyalty$PropertyType)

# calculate the mean of the non-NA values in the age column
mean_age <- mean(bbyLoyalty$Age, na.rm = TRUE)

# replace the NA values in the age column with the mean
bbyLoyalty$Age<- ifelse(is.na(bbyLoyalty$Age), mean_age, bbyLoyalty$Age)
bbyLoyalty$Age <- round(bbyLoyalty$Age)

# rounding the household spend
bbyLoyalty$y_householdSpend <- round(bbyLoyalty$y_householdSpend)

# replace blank spaces with Unknown in CatOwner
bbyLoyalty$CatOwner <- ifelse(bbyLoyalty$CatOwner == "", "Unknown", bbyLoyalty$CatOwner)

# replace blank spaces with Unknown in DogOwner
bbyLoyalty$DogOwner <- ifelse(bbyLoyalty$DogOwner == "", "Unknown", bbyLoyalty$DogOwner)

# create a new column indicating if the person has a pet or not
bbyLoyalty$HasPet <- ifelse(bbyLoyalty$CatOwner == "Yes" & bbyLoyalty$DogOwner == "Yes", "Yes",
                    ifelse(bbyLoyalty$CatOwner == "Unknown" & bbyLoyalty$DogOwner == "Unknown", "Unknown",
                           ifelse(bbyLoyalty$CatOwner =="Yes" & bbyLoyalty$DogOwner =="Unknown", "Yes",
                                  ifelse(bbyLoyalty$CatOwner =="Unknown" & bbyLoyalty$DogOwner =="Yes", "Yes", "Unknown"))))


# Create a new subset with the columns we require
bbyLoyaltySub <- subset(bbyLoyalty, select = c(tmpID, 
                                               FirstName, 
                                               LastName, 
                                               Gender, 
                                               Age, 
                                               state, 
                                               storeVisitFrequency,
                                               PropertyType, 
                                               y_householdSpend, 
                                               ResidenceHHGenderDescription, 
                                               PresenceOfChildrenCode, 
                                               HomeOwnerRenter, 
                                               ComputerOwnerInHome,
                                               HasPet))

#write.csv(bbyLoyaltySub, "bbyLoyaltySub.csv", row.names = FALSE)


## grouping state and homeownerrenter and getting their counts

grouped_df <- bbyLoyaltySub %>%
  group_by(state,HomeOwnerRenter) %>%
  count()
grouped_df


# Create the plot object and add the column chart
ggplot(grouped_df, aes(x = n, y = state, fill = HomeOwnerRenter )) +
  geom_col(position = "dodge") +
  labs(title = "Statewise owners", x = "Count", y = "States", fill = "HomeOwnerRenter")


## grouping property type and homeowner renter to see their distribution 

grouped_df <- bbyLoyaltySub %>%
  group_by(PropertyType,HomeOwnerRenter) %>%
  count()
grouped_df

ggplot(grouped_df, aes(x = HomeOwnerRenter, y = n, fill = PropertyType)) +
  geom_col(position = "dodge") +
  labs(x = "Homeowner/Renter", y = "Count", title = "Customers by Property Type and Homeowner/Renter Status") +
  scale_fill_discrete(name = "Property Type")



## bar plot to see property type distribution of the consumers 


ggplot(bbyLoyaltySub, aes(x = PropertyType)) + 
  geom_bar(fill = "steelblue", color = "black") + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of customers based on property type", 
       x = "Property Type", y = "Count") +
  scale_fill_manual(values = c("Condo" = "red", "Single Family" = "blue", "Townhouse" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

## grouping property type and residence gender description 

grouped_df <- bbyLoyaltySub %>%
  group_by(PropertyType,ResidenceHHGenderDescription) %>%
  count()
grouped_df

ggplot(grouped_df, aes(x = ResidenceHHGenderDescription, y = n, fill = PropertyType)) +
  geom_col(position = "dodge") +
  labs(x = "ResidenceHHGenderDescription", y = "Count", title = "Customers by Property Type and Residence Gender") +
  scale_fill_discrete(name = "Property type") +
  theme_minimal()

