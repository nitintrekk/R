# Importing necessary libraries
library(dplyr)
library(skimr)
library(stringr)
library(psych)
library(ROSE)
library(ggplot2)
library(caret)
library(naniar)
library(gridExtra)
library(randomForest)
library(ranger)


# Set working directory
setwd("~/Hult_R/personalFiles")

# Read the training datasets into R
hospital_train <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTrain.csv')
meds_train <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTrain.csv')
patient_train <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTrain.csv')

# Read the test datasets into R
hospital_test <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTest.csv')
meds_test <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTest.csv')
patient_test <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTest.csv')

# Combine the train data into a single dataframe
df_train <- hospital_train %>% 
  left_join(meds_train, by = "tmpID") %>% 
  left_join(patient_train, by = "tmpID")

# Combine the test data into a single dataframe
df_test <- hospital_test %>% 
  left_join(meds_test, by = "tmpID") %>% 
  left_join(patient_test, by = "tmpID")


# Replace all blanks and "?" with "Other"
df_train[df_train == "" | df_train == "?"] <- "Other"
# Print the updated dataframe
print(df_train)
# Count the number of blanks and "?" in the dataframe
num_blanks <- sum(sapply(df_train, function(x) sum(is.na(x) | x == "")))
num_question_marks <- sum(sapply(df_train, function(x) sum(x == "?")))
# Print the results
cat("Number of blanks:", num_blanks, "\n")
cat("Number of question marks:", num_question_marks, "\n")

# Replace all blanks and "?" with "Other"
df_test[df_test == "" | df_test == "?"] <- "Other"
# Print the updated dataframe
print(df_test)
# Count the number of blanks and "?" in the dataframe
num_blanks <- sum(sapply(df_test, function(x) sum(is.na(x) | x == "")))
num_question_marks <- sum(sapply(df_test, function(x) sum(x == "?")))
# Print the results
cat("Number of blanks:", num_blanks, "\n")
cat("Number of question marks:", num_question_marks, "\n")


# Create bins for the age column for train
df_train$age_bins <- cut(df_train$age, breaks = seq(0, 100, 10), right = TRUE, include.lowest = TRUE)
# Create bins for the age column for test
df_test$age_bins <- cut(df_test$age, breaks = seq(0, 100, 10), right = TRUE, include.lowest = TRUE)


# Drop columns
df_train <- df_train[, !(names(df_train) %in% c("citoglipton", "examide", "tolazamide", "troglitazone", "acetohexamide", "diag_1_desc", "diag_2_desc", "diag_3_desc","age"))]
df_test <- df_test[, !(names(df_test) %in% c("citoglipton", "examide", "tolazamide", "troglitazone", "acetohexamide", "diag_1_desc", "diag_2_desc", "diag_3_desc","age"))]

# Drop patients whose discharge_disposition_id is "Expired" in df_train
df_train <- df_train[df_train$discharge_disposition_id != "Expired", ]
df_test <- df_test[df_test$discharge_disposition_id != "Expired", ]


# creating a barplot for number of patients by gender
# Create a summary table of the number of patients by gender
summary_table <- df_train %>% 
  group_by(gender) %>% 
  summarize(n = n()) %>% 
  mutate(percent = n/sum(n) * 100) %>% 
  arrange(desc(percent))  # arrange in descending order by percent
# Create a barplot of the number of patients by gender with percentages
ggplot(summary_table, aes(x = reorder(gender, desc(percent)), y = n, fill = gender)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3) +
  labs(x = "Gender", y = "Number of Patients") +
  ggtitle("Number of Patients by Gender") 


# creating a barplot for number of patients by race
# Create a summary table of the number of patients by race and sort in descending order
summary_table <- df_train %>%
  group_by(race) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n) * 100) %>%
  arrange(desc(percent))
# Create the barplot
ggplot(summary_table, aes(x = n, y = reorder(race, -percent), fill = race)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of Patients", y = "Race") +
  ggtitle("Number of Patients by Race") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3)


# creating a barplot for distribution of patients across age bins
# Create a frequency table of age bins in df_train
age_freq <- df_train %>% 
  count(age_bins, sort = TRUE)
# Create a new column for the percentage of patients in each age bin
age_freq <- age_freq %>%
  mutate(perc = n/sum(n)*100)
# Create a bar plot of the age bins
ggplot(age_freq, aes(x = n, y = age_bins, fill = age_bins)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(name = "Number of Patients", labels = scales::comma) +
  scale_fill_discrete(name = "Age Bin") +
  ylab("") +
  ggtitle("Distribution of Patients Across Age Bins") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))


# Create a barplot for patients readmitted for train dataset
counts <- table(df_train$readmitted_y)
df_counts <- data.frame(readmitted_y = names(counts),
                        n = as.numeric(counts),
                        percent = round(as.numeric(counts) / sum(counts) * 100, 2))
# Create the bar plot
ggplot(df_counts, aes(x = n, y = readmitted_y, fill = readmitted_y)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  geom_text(aes(label = paste(percent, "%"), x = n + 50, y = readmitted_y), 
            size = 4, fontface = "bold") +
  labs(x = "Number of patients", y = "Readmitted", 
       title = "Number and percentage of patients readmitted") +
  theme_minimal()


# Count the number of patients in each category of diabetesMed and readmitted
counts <- table(df_train$diabetesMed, df_train$readmitted_y)
# Calculate the percentage of patients in each category of diabetesMed and readmitted
percentages <- prop.table(counts) * 100
# Create a barplot with counts and percentages
barplot(counts, main = "Number of Patients by Diabetes Medication and Readmission Status", xlab = "Diabetes Medication and Readmission Status", ylab = "Number of Patients", 
        legend.text = rownames(percentages), col = c("dodgerblue", "salmon"))
text(x = barplot(counts), y = counts, labels = paste0(counts, " (", round(percentages, 1), "%)"), pos = 3)



# Plot a KDE graph of time in hospital vs readmitted_y in the training data 
ggplot(df_train, aes(x = time_in_hospital, fill = readmitted_y)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("darkorange", "blueviolet")) +
  labs(title = "Time in Hospital vs Readmitted Status", x = "Time in Hospital", y = "Density") +
  theme_classic() +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "bottom")



# Creating a matrix of plots for numeric variables to check their distribution
par(mfrow=c(3,2))
# Plotting the histograms of numeric columns
hist(df_train$num_lab_procedures, main="Number of lab procedures", xlab="Count")
hist(df_train$time_in_hospital, main="Days hospitalized", xlab="Count")
hist(df_train$num_medications, main="Number of medications", xlab="Count")
hist(df_train$num_procedures, main="Number of procedures", xlab="Count")



# creating catplot for age and number of medications
ggplot(df_train, aes(x=age_bins, y=num_medications)) +
  geom_jitter(width=0.25) +
  labs(title = "Age and Number of medications", x = "Age", y = "Number of medications") +
  theme_bw() +
  theme(plot.title = element_text(size=22))



# creating catplot for race and number of medications
ggplot(df_train, aes(x = race, y = num_medications)) +
  geom_point(position = position_jitter(width = 0.25)) +
  labs(title = "Race and Number of medications", x = "Race", y = "Number of medications") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(size = 22, face = "bold"))



# creating a catplot for gender and number of medications
ggplot(df_train, aes(x = gender, y = num_medications)) +
  geom_point(position = position_jitter(width = 0.25)) +
  ggtitle("Gender and Number of medications") +
  theme(plot.title = element_text(size = 22)) +
  labs(x = "Gender", y = "Number of medications") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_blank(),
        legend.position = "none") +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))



# creating a catplot for A1Cresult and number of medications
ggplot(df_train, aes(x=A1Cresult, y=num_medications, color=change)) +
  geom_jitter(width=0.25) +
  ggtitle("A1Cresult and Number of medications") +
  labs(x="A1Cresult", y="Number of medications") +
  theme(plot.title = element_text(size=22), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))



# creating a catplot for discharge desposition and readmitted status
ggplot(df_train, aes(x = discharge_disposition_id, y = num_medications, fill = readmitted_y)) +
  geom_bar(position = "dodge", stat = "summary", fun = "median", color = "black") +
  scale_fill_manual(values = c("darkorange", "blueviolet")) +
  theme_classic() +
  labs(title = "Discharge disposition and readmitted status", x = "Discharge disposition ID", y = "Number of medications") +
  guides(fill = guide_legend(title = "Readmitted status")) +
  coord_flip() +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "bottom")



# Create boxplot for num_medications
ggplot(df_train, aes(y = num_medications)) +
  geom_boxplot(fill = "green") +
  ggtitle("Boxplot: number of medications") +
  theme(plot.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"))



# Create boxplot for weight distribution
ggplot(df_train, aes(y = wgt)) +
  geom_boxplot(fill = "green") +
  ggtitle("Boxplot: Weight of the set") +
  theme(plot.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"))



# Create the boxplots for different  numeric variables to check for outliers
a <- ggplot(df_train, aes(y = num_medications)) + 
  geom_boxplot(fill = "darkorange", color = "black") +
  ggtitle("Boxplot: number of medications") +
  theme(plot.title = element_text(size = 16, face = "bold"), 
        axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 14), 
        axis.line = element_line(color = "black"))
b <- ggplot(df_train, aes(y = num_procedures)) + 
  geom_boxplot(fill = "darkorange", color = "black") +
  ggtitle("Boxplot: number of procedures") +
  theme(plot.title = element_text(size = 16, face = "bold"), 
        axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 14), 
        axis.line = element_line(color = "black"))
c <- ggplot(df_train, aes(y = num_lab_procedures)) + 
  geom_boxplot(fill = "darkorange", color = "black") +
  ggtitle("Boxplot: number of lab procedures") +
  theme(plot.title = element_text(size = 16, face = "bold"), 
        axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 14), 
        axis.line = element_line(color = "black"))
d <- ggplot(df_train, aes(y = time_in_hospital)) + 
  geom_boxplot(fill = "darkorange", color = "black") +
  ggtitle("Boxplot: days in hospital") +
  theme(plot.title = element_text(size = 16, face = "bold"), 
        axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 14), 
        axis.line = element_line(color = "black"))
e <- ggplot(df_train, aes(y = number_diagnoses)) + 
  geom_boxplot(fill = "darkorange", color = "black") +
  ggtitle("Boxplot: number of diagnoses") +
  theme(plot.title = element_text(size = 16, face = "bold"), 
        axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 14), 
        axis.line = element_line(color = "black"))
# Arrange the plots in a grid
grid.arrange(a, b, c, d, e, nrow = 3, ncol = 2, widths = c(1, 1), heights = c(1, 1, 1))

skim(df_train)

# Split the data into training and testing sets
set.seed(123)
train_data <- df_train
test_data <- df_test

# Convert response variable to factor
train_data$readmitted_y <- as.factor(train_data$readmitted_y)
test_data$readmitted_y <- as.factor(test_data$readmitted_y)

# Train the random forest model
model <- randomForest(readmitted_y ~ ., data = train_data)

# Make predictions on the test data
predictions <- predict(model, test_data)

# Make predictions on the test data
probability <- predict(model, test_data,type = 'prob')

write.csv(probability, file = "probs.csv", row.names = FALSE)

# Evaluate the model
confusionMatrix(predictions, test_data$readmitted_y)

write.csv(predictions,file="predictions.csv",row.names=FALSE)

#fitting data to check for the most important features 
fit <- ranger(as.factor(readmitted_y)~., 
              train_data, 
              importance = 'permutation',
              probability = T)

# Look at improved var importance
varImpDF <- data.frame(variables = names(importance(fit)),
                       importance = importance(fit),
                       row.names = NULL)
varImpDF <- varImpDF[order(varImpDF$importance, decreasing = T),]
ggplot(varImpDF, aes(x=importance, y = reorder(variables, importance))) + 
  geom_bar(stat='identity', position = 'dodge') + 
  ggtitle('Variable Importance') + theme_minimal()


# check if probability matrix is empty
if (nrow(probability) == 0) {
  stop("Probability matrix is empty.")
}

# create vector of patient names
patient_names <- rep("", nrow(probability))
for (i in 1:nrow(probability)) {
  patient_names[i] <- paste0("Patient_", i)
}

# create data frame with corrected inputs
DF <- data.frame(patient = patient_names,
                 notAdmitted = probability[,0],
                 admitted = probability[,1],
                 class = max.col(probability))

# check the resulting data frame
head(DF)

# filter and order data
result <- DF[DF$class == 1,]
result <- result[order(-result$admitted),]
result <- result[1:100,]

# check the resulting data
head(result)

write.csv(result,file="result.csv",row.names=FALSE)


# Convert predictions to a data frame with a single column
predictions_df <- data.frame(readmitted_y = predictions)


#plot graph and compare the predicted readmitted status vs the original data
ggplot(predictions_df, aes(x = readmitted_y)) +
  geom_bar(fill = "dodgerblue") +
  geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..)), y = ..count..), 
            vjust = -1, size = 5) +
  labs(title = "Readmitted status in predicted data", x = "Readmitted status", y = "Count") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())



ggplot(df_test, aes(x = readmitted_y)) +
  geom_bar(fill = "dodgerblue") +
  geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..)), y = ..count..), 
            vjust = -1, size = 5) +
  labs(title = "Readmitted status in test data", x = "Readmitted status", y = "Count") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())





