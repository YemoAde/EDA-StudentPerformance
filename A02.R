# Library Imports
library(dplyr)
library(ggplot2)n
install.packages("assertive")
library(assertive)
library(lattice)
library(reshape2)
library(ggcorrplot)

# Color palettes
cp_2 <- c("#FEA47F", "#F97F51")
cp_3 <- c("#2A363B", "#E84A5F", "#FF847C")
cp_5 <- c("#2A363B", "#E84A5F", "#FF847C", "#FECEAB", "#99B898")
cp_4 <- c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12")
cp_8 <- c("#FEA47F", "#F97F51", "#B33771", "#3B3B98", "#58B19F", "#BDC581", "#2C3A47", "#82589F")

#Import data
performance_data <- read.csv('StudentsPerformance.csv', header = T)
glimpse(performance_data)

#Rename Columns
names(performance_data) <- c('gender', 'race', 'parent_education', 'lunch', 'test_preparation_course', 'math_score', 'reading_score', 'writing_score')

## Check for duplicates and missing data ##
sum(duplicated(performance_data))
sum(is.na.data.frame(performance_data))
## ##


##### Math Score #####

summary(performance_data$math_score)
# Check for NA
performance_data$math_score %>% is.na() %>% sum()

# Check for outliers
performance_data$math_score %>% boxplot(horizontal = T, border = "#6fa058", outcol = "#ee853f")


##### #####


##### Reading Score #####
summary(performance_data$reading_score)
# Check for NA
performance_data$reading_score %>% is.na() %>% sum()

# Check for outliers
performance_data$reading_score %>% boxplot(horizontal = T, border = "#6fa058", outcol = "#ee853f")

##### #####

##### Writing Score #####
summary(performance_data$writing_score)
# Check for NA
performance_data$writing_score %>% is.na() %>% sum()

# Check for outliers
performance_data$writing_score %>% boxplot(horizontal = T, border = "#6fa058", outcol = "#ee853f")
##### #####



### Correlation Between Scores ###
score_correlation <- cor(performance_data[,c(6:8)])
ggcorrplot(score_correlation, lab = T)
####


##### Gender #####
summary(performance_data$gender)
performance_data$gender <- as.factor(performance_data$gender)
attributes(performance_data$gender)

ggplot(performance_data, aes(x=gender, y=..count.., fill=gender)) +
  geom_bar() +
  scale_fill_manual(values = cp_2)+
  labs(x="Gender", y="Frequency", fill="Gender", title="Frequency of different Gender") +
  theme_minimal()

gender_chart <- melt(performance_data,id.vars='gender', measure.vars=c('math_score','reading_score','writing_score'))
ggplot(gender_chart) +
  geom_boxplot(aes(x=gender, y=value, fill=variable), outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Gender") +
  ylab("Score") +
  ggtitle("\n") +
  scale_fill_manual(values=c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12"), 
                    name="Scores",
                    labels=c("Math Score", "Reading Score", "Writing Score")) +
  theme(plot.title = element_text(size = 11, face="bold"))
###

##### Race #####
performance_data$race %>% is.na() %>% sum()
summary(performance_data$race)
performance_data$race <- as.factor(performance_data$race)
attributes(performance_data$race)

temp <- performance_data %>% group_by(race) %>% summarize(meanMathScore = mean(math_score), meanReadingScore = mean(reading_score), meanWritingScore = mean(writing_score))
temp_reshaped <- melt(temp, id.vars="race", measure.vars = c("meanMathScore", "meanReadingScore", "meanWritingScore"))

ggplot(performance_data, aes(x=race, y=..count.., fill=race)) +
  geom_bar() +
  theme_light(base_size = 11) +
  xlab("Race") +
  ylab("Frequency") +
  ggtitle("Frequency of different Races") +
  scale_fill_manual(values= cp_5, name="Races:") +
  theme(plot.title = element_text(size = 11, face="bold"))

race_chart <- melt(performance_data,id.vars='race', measure.vars=c('math_score','reading_score','writing_score'))
ggplot(race_chart) +
  geom_boxplot(aes(x=race, y=value, fill=variable), outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Race") +
  ylab("Score") +
  ggtitle("\n") +
  scale_fill_manual(values=c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12"), 
                    name="Scores",
                    labels=c("Math Score", "Reading Score", "Writing Score")) +
  theme(plot.title = element_text(size = 11, face="bold"))

ggplot(temp_reshaped, aes(race, value, fill=variable)) + geom_col(position = "dodge") +
  xlab("Race") +
  ylab("Score") +
  ggtitle("Mean Score by race") +
  scale_fill_manual(name="Scores",
                    values=cp_5,
                    labels=c("Mean Math Score", "Mean Reading Score", "Mean Writing Score")) +
  theme(plot.title = element_text(size = 11, face="bold"))
###


##### Parent Education #####
performance_data$parent_education %>% is.na() %>% sum()
summary(performance_data$parent_education)
performance_data$parent_education <- as.factor(performance_data$parent_education)
attributes(performance_data$parent_education)

ggplot(performance_data, aes(x=reorder(parent_education,parent_education, function(x)-length(x)), y=..count.., fill=parent_education)) +
  geom_bar() +
  theme_light(base_size = 11) +
  xlab("Parent Education Level") +
  ylab("Frequency") +
  ggtitle("Frequency of different Education Levels") +
  scale_fill_manual(values= cp_8, name="Education Level:") +
  theme(plot.title = element_text(size = 11, face="bold"))

edu_chart <- melt(performance_data,id.vars='parent_education', measure.vars=c('math_score','reading_score','writing_score'))
ggplot(edu_chart) +
  geom_boxplot(aes(x=parent_education, y=value, fill=variable), outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Parent Education Level") +
  ylab("Score") +
  ggtitle("\n") +
  scale_fill_manual(values=c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12"), 
                    name="Scores",
                    labels=c("Math Score", "Reading Score", "Writing Score")) +
  theme(plot.title = element_text(size = 11, face="bold"))

temp_edu <- performance_data %>% group_by(parent_education) %>% summarize(meanMathScore = mean(math_score), meanReadingScore = mean(reading_score), meanWritingScore = mean(writing_score))
temp_edu <- melt(temp_edu, id.vars="parent_education", measure.vars = c("meanMathScore", "meanReadingScore", "meanWritingScore"))
ggplot(temp_edu, aes(parent_education, value, fill=variable)) + geom_col(position = "dodge") +
  xlab("Parent Education") +
  ylab("Score") +
  ggtitle("Mean Score by Parent Education") +
  scale_fill_manual(name="Scores",
                    values=cp_5,
                    labels=c("Mean Math Score", "Mean Reading Score", "Mean Writing Score")) +
  theme(plot.title = element_text(size = 11, face="bold"))
##### #####


##### Lunch #####
performance_data$lunch %>% is.na() %>% sum()
summary(performance_data$lunch)
performance_data$lunch <- as.factor(performance_data$lunch)
attributes(performance_data$lunch)

ggplot(performance_data, aes(x=reorder(lunch,lunch, function(x)-length(x)), y=..count.., fill=lunch)) +
  geom_bar() +
  theme_light(base_size = 11) +
  xlab("Lunch Type") +
  ylab("Frequency") +
  ggtitle("Lunch Type") +
  scale_fill_manual(values= cp_8, name="Lunch Type") +
  theme(plot.title = element_text(size = 11, face="bold"))

lunch_chart <- melt(performance_data,id.vars='lunch', measure.vars=c('math_score','reading_score','writing_score'))
ggplot(lunch_chart) +
  geom_boxplot(aes(x=lunch, y=value, fill=variable), outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Lunch Type") +
  ylab("Score") +
  ggtitle("\n") +
  scale_fill_manual(values=c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12"), 
                    name="Scores",
                    labels=c("Math Score", "Reading Score", "Writing Score")) +
  theme(plot.title = element_text(size = 11, face="bold"))

temp_lunch <- performance_data %>% group_by(lunch) %>% summarize(meanMathScore = mean(math_score), meanReadingScore = mean(reading_score), meanWritingScore = mean(writing_score))
temp_lunch <- melt(temp_lunch, id.vars="lunch", measure.vars = c("meanMathScore", "meanReadingScore", "meanWritingScore"))
ggplot(temp_lunch, aes(lunch, value, fill=variable)) + geom_col(position = "dodge") +
  xlab("Lunch Type") +
  ylab("Score") +
  ggtitle("Mean Score by Lunch Type") +
  scale_fill_manual(name="Scores",
                    values=cp_5,
                    labels=c("Mean Math Score", "Mean Reading Score", "Mean Writing Score")) +
  theme(plot.title = element_text(size = 11, face="bold"))
##### #####



###### Test Preparation Course #####
performance_data$test_preparation_course %>% is.na() %>% sum()
summary(performance_data$test_preparation_course)
performance_data$test_preparation_course <- as.factor(performance_data$test_preparation_course)
attributes(performance_data$test_preparation_course)


ggplot(performance_data, aes(x=test_preparation_course, y=..count.., fill=test_preparation_course)) +
  geom_bar() +
  theme_light(base_size = 11) +
  xlab("Test Preparation Course") +
  ylab("Frequency") +
  ggtitle("Test Preparation Course") +
  scale_fill_manual(values= cp_8, name="Test Preparation Course") +
  theme(plot.title = element_text(size = 11, face="bold"))

temp_prepared <- performance_data %>% group_by(test_preparation_course) %>% summarize(meanMathScore = mean(math_score), meanReadingScore = mean(reading_score), meanWritingScore = mean(writing_score))
temp_prepared <- melt(temp_prepared, id.vars="test_preparation_course", measure.vars = c("meanMathScore", "meanReadingScore", "meanWritingScore"))
ggplot(temp_prepared, aes(test_preparation_course, value, fill=variable)) + geom_col(position = "dodge") +
  xlab("Test Peparation Course") +
  ylab("Score") +
  ggtitle("Mean Score by Test Preparation") +
  scale_fill_manual(name="Scores",
                    values=cp_5,
                    labels=c("Mean Math Score", "Mean Reading Score", "Mean Writing Score")) +
  theme(plot.title = element_text(size = 11, face="bold"))

#####

