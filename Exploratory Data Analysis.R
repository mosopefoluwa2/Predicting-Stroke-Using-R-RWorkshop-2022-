#PREDICTING STROKE WITH R

#importing necessary packages
library(dplyr)
library(readr)
library(tidyr)
library(SmartEDA)
library(ggplot2)
library(Hmisc)
library(treemapify)


#add dataset
stroke_data <- read.csv(file="healthcare-dataset-stroke-data.csv",
                        head=TRUE,
                        sep=",")

#Exploratory Data Analysis
#view the data
View(stroke_data)

#view the first 6 rows
head(stroke_data)

#view the last 6 rows
tail(stroke_data)

dim(stroke_data)
str(stroke_data)
summary(stroke_data)
ExpData(data = stroke_data, type = 2)

# View all distinct categorical variable
lapply(subset(stroke_data, select = c(2, 6, 7, 8, 11)), unique)

#missing values
class(stroke_data$bmi)
stroke_data$bmi = as.numeric(stroke_data$bmi)
class(stroke_data$bmi)
colSums(is.na(stroke_data))

#repeat summary statistics
summary(stroke_data)
ExpData(data = stroke_data, type = 2)
#check for duplicates
sum(duplicated(stroke_data))

#Univariate Analysis

hist.data.frame(stroke_data)

#1. Gender
# Count the unique variables in the gender column
table(stroke_data$gender)

#graphical illustration
#bar graph
gender_bar <- stroke_data %>% count(gender)
ggplot(data = gender_bar, aes(gender, y = n, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(x = "Gender", 
       y = "Frequency",
       title = "Gender Distribution") +
  geom_text(aes(label = signif(n)), nudge_y = 4)

#pie chart
gender_pie <- stroke_data %>%
  count(gender) %>%
  arrange(desc(gender)) %>%
  mutate(prop = round(n*100/sum(n), 2),
         lab.ypos = cumsum(prop) - 0.5*prop)

gender_pie$label <- paste0(gender_pie$gender, "\n",
                         gender_pie$prop, "%")
ggplot(gender_pie, 
       aes(x = "", 
           y = prop, 
           fill = gender)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = 1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Gender Distribution")


#2. Age
#graphical illustration
#histogram
ggplot(stroke_data, aes(x = age)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 binwidth = 5) + 
  labs(title="Participants by age", 
       subtitle = "binwidth = 5 years",
       x = "Age")

#kernel density plot
ggplot(stroke_data, aes(x = age)) +
  geom_density(fill = "indianred", 
               bw = 1) + 
  labs(title = "Participants by age")


#3. Hypertension
# Count the unique variables in the hypertension column
table(stroke_data$hypertension)

#graphical illustration
#bar graph
hypertension_bar <- stroke_data %>% count(hypertension)
ggplot(data = hypertension_bar, aes(hypertension, y = n)) +
  geom_bar(stat = "identity", width = 0.5, fill = "skyblue") +
  labs(x = "Blood Pressure", 
       y = "Frequency",
       title = "Blood Pressure Distribution") +
  geom_text(aes(label = signif(n)))

#pie chart
hypertension_pie <- stroke_data %>%
  count(hypertension) %>%
  arrange(desc(hypertension)) %>%
  mutate(prop = round(n*100/sum(n), 2),
         lab.ypos = cumsum(prop) - 0.5*prop)
hypertension_pie$label <- paste0(hypertension_pie$hypertension,"\n",
                                 hypertension_pie$prop, "%")
ggplot(hypertension_pie, 
       aes(x = "", 
           y = prop, 
           fill = hypertension)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", start = 0, direction = 1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Blood Pressure Distribution")


#4. Heart Disease
# Count the unique variables in the heart_disease column
table(stroke_data$heart_disease)

#bar graph
heart_bar <- stroke_data %>% count(heart_disease)
ggplot(data = heart_bar, aes(heart_disease, y = n)) +
  geom_bar(stat = "identity", width = 0.5, fill = "orange") +
  labs(x = "Heart Disease", 
       y = "Frequency",
       title = "Heart Disease Distribution") +
  geom_text(aes(label = signif(n)))

#pie chart
heart_pie <- stroke_data %>%
  count(heart_disease) %>%
  arrange(desc(heart_disease)) %>%
  mutate(prop = round(n*100/sum(n), 2),
         lab.ypos = cumsum(prop) - 0.5*prop)
heart_pie$label <- paste0(heart_pie$heart_disease,"\n",
                                 heart_pie$prop, "%")
ggplot(heart_pie, 
       aes(x = "", 
           y = prop, 
           fill = heart_disease)) +
  geom_bar(width = 1, stat = "identity", color = "black", fill = c("skyblue", "blue")) +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", start = 0, direction = 1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Heart Disease Distribution")


#5. Ever married
# Count the unique variables in the ever_married column
table(stroke_data$ever_married)

#graphical illustration
#bar graph
married_bar <- stroke_data %>% count(ever_married)
ggplot(married_bar, aes(ever_married, y = n, fill = ever_married)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "Marriage Status", 
       y = "Frequency",
       title = "Marriage Status of Participants") +
  geom_text(aes(label = signif(n)))

#pie chart
married_pie <- stroke_data %>%
  count(ever_married) %>%
  arrange(desc(ever_married)) %>%
  mutate(prop = round(n*100/sum(n), 2),
         lab.ypos = cumsum(prop) - 0.5*prop)
married_pie$label <- paste0(married_pie$ever_married,"\n",
                          married_pie$prop, "%")
ggplot(married_pie, 
       aes(x = "", 
           y = prop, 
           fill = ever_married)) +
  geom_bar(width = 1, stat = "identity", color = "black", fill = c("violet", "purple")) +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", start = 0, direction = 1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Marriage Status of Participants")


#6. Work Type
# Count the unique variables in the work_type column
table(stroke_data$work_type)

#graphical illustration
#bar graph
work_bar <- stroke_data %>% count(work_type)
ggplot(work_bar, aes(work_type, y = n, fill = work_type)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "Employment Status", 
       y = "Frequency",
       title = "Employment Status of Participants") +
  geom_text(aes(label = signif(n)))

#pie chart
work_pie <- stroke_data %>%
  count(work_type) %>%
  arrange(desc(work_type)) %>%
  mutate(prop = round(n*100/sum(n), 2),
         lab.ypos = cumsum(prop) - 0.5*prop)
work_pie$label <- paste0(work_pie$work_type,"\n",
                            work_pie$prop, "%")
ggplot(work_pie, 
       aes(x = "", 
           y = prop, 
           fill = work_type)) +
  geom_bar(width = 1, stat = "identity", color = "black", fill = rainbow(length(work_pie))) +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", start = 0, direction = 1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Employment Status of Participants")

#Tree Map
tree_work <- stroke_data %>%
  count(work_type)

ggplot(tree_work, 
       aes(fill = work_type, 
           area = n)) +
  geom_treemap() + 
  labs(title = "Employment Status")


#7. Residence type
# Count the unique variables in the Residence_type column
table(stroke_data$Residence_type)

#graphical illustration
#bar graph
residence_bar <- stroke_data %>% count(Residence_type)
ggplot(residence_bar, aes(Residence_type, y = n, fill = Residence_type)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "Residential Status", 
       y = "Frequency",
       title = "Residential Status of Participants") +
  geom_text(aes(label = signif(n)))

#pie chart
residence_pie <- stroke_data %>%
  count(Residence_type) %>%
  arrange(desc(Residence_type)) %>%
  mutate(prop = round(n*100/sum(n), 2),
         lab.ypos = cumsum(prop) - 0.5*prop)
residence_pie$label <- paste0(residence_pie$Residence_type,"\n",
                              residence_pie$prop, "%")
ggplot(residence_pie, 
       aes(x = "", 
           y = prop, 
           fill = Residence_type)) +
  geom_bar(width = 1, stat = "identity", color = "black", fill = c("chartreuse", "chartreuse4")) +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", start = 0, direction = 1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Residential Status of Participants")


#8. Average Glucose Level
#graphical illustration
#histogram
ggplot(stroke_data, aes(x = avg_glucose_level)) +
  geom_histogram(fill = "aquamarine", 
                 color = "black", 
                 binwidth = 5) + 
  labs(title="Average Glucose Levels of Participants",
       x = "Avg Glucose Level")

#kernel density plot
ggplot(stroke_data, aes(x = avg_glucose_level)) +
  geom_density(fill = "indianred3") + 
  labs(title = "Participants by AGL")


#9. BMI
min(table(stroke_data$bmi))
max(table(stroke_data$bmi))
#graphical illustration
#histogram
ggplot(stroke_data, aes(x = bmi)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 binwidth = 5) + 
  labs(title="Participants by BMI",
       x = "BMI")


#10. Smoking status
# Count the unique variables in the smoking_status column
table(stroke_data$smoking_status)

#graphical illustration
#bar graph
smoking_bar <- stroke_data %>% count(smoking_status)
ggplot(smoking_bar, aes(smoking_status, y = n, fill = smoking_status)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "Smoking Status", 
       y = "Frequency",
       title = "Smoking Status of Participants") +
  geom_text(aes(label = signif(n)))

#pie chart
smoking_pie <- stroke_data %>%
  count(smoking_status) %>%
  arrange(desc(smoking_status)) %>%
  mutate(prop = round(n*100/sum(n), 2),
         lab.ypos = cumsum(prop) - 0.5*prop)
smoking_pie$label <- paste0(smoking_pie$smoking_status,"\n",
                            smoking_pie$prop, "%")
ggplot(smoking_pie, 
       aes(x = "", 
           y = prop, 
           fill = smoking_status)) +
  geom_bar(width = 1, stat = "identity", color = "black", col = heat.colors(4)) +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", start = 0, direction = 1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Smoking Status of Participants")


#11. Stroke
# Count the unique variables in the stroke column
table(stroke_data$stroke)

#graphical illustration
#bar graph
stroke_bar <- stroke_data %>% count(stroke)
ggplot(data = stroke_bar, aes(stroke, y = n)) +
  geom_bar(stat = "identity", width = 0.5, fill="#CC6699") +
  labs(x = "Stroke", 
       y = "Frequency",
       title = "Occurence of Stroke") +
  geom_text(aes(label = signif(n)))

#pie chart
stroke_pie <- stroke_data %>%
  count(stroke) %>%
  arrange(desc(stroke)) %>%
  mutate(prop = round(n*100/sum(n), 2),
         lab.ypos = cumsum(prop) - 0.5*prop)
stroke_pie$label <- paste0(stroke_pie$troke,"\n",
                            stroke_pie$prop, "%")
ggplot(stroke_pie, 
       aes(x = "", 
           y = prop, 
           fill = stroke)) +
  geom_bar(width = 1, stat = "identity", color = "black", fill = c("chocolate1", "chocolate4")) +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", start = 0, direction = 1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Smoking Status of Participants")



