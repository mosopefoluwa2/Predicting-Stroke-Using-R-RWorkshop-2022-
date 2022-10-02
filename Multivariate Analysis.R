#MULTIVARIATE ANALYSIS

#importing necessary packages
library(dplyr)
library(readr)
library(ggplot2)
library(dlookr)
library(ggcorrplot)

#add dataset
stroke_data <- read.csv(file="healthcare-dataset-stroke-data.csv",
                        head=TRUE,
                        sep=",")

#what is the average age for men and women with and without stroke?
stroke_data %>%
  group_by(gender, stroke) %>%
  summarise(
    n = n(),
    'mean age' = mean(age))

#how many men and women living in rural areas are smokers?
stroke_data %>%
  group_by(gender) %>%
  summarise(
    'mean age' = mean(age),
    'n' = n(),
    'from rural area' = sum(Residence_type == 'Rural'),
    'from rural area and smoking' = sum(Residence_type == 'Rural' 
                                        & (smoking_status == 'smokes')))

#how many men and women have hypertension, heart disease and stroke?
stroke_data %>%
  group_by(gender) %>%
  summarise(
    'n' = n(),
    "HTN" = sum(hypertension == 1),
    'HD' = sum(heart_disease == 1),
    'str' = sum(stroke ==1),
    'all 3' = sum(hypertension == 1 &
                    (heart_disease == 1 & (stroke == 1))))

#where do people who have hypertension, heart disease and stroke live?
stroke_data %>%
  group_by(Residence_type) %>%
  summarise(
    'n' = n(),
    "HTN" = sum(hypertension == 1),
    'HD' = sum(heart_disease == 1),
    'str' = sum(stroke ==1),
    'all 3' = sum(hypertension == 1 &
                    (heart_disease == 1 & (stroke == 1))))
#are people who have hypertension, heart disease and stroke single or married?
stroke_data %>%
  group_by(ever_married) %>%
  summarise(
    'n' = n(),
    "HTN" = sum(hypertension == 1),
    'HD' = sum(heart_disease == 1),
    'str' = sum(stroke ==1),
    'all 3' = sum(hypertension == 1 &
                    (heart_disease == 1 & (stroke == 1))))


#what is the smoking status of men and women who have stroke?
stroke_data %>%
  group_by(gender) %>%
  summarise(
    'n' = n(),
    "smoker" = sum(smoking_status == "smokes"),
    'smoke & strk' = sum(smoking_status == "smokes" &
                               (stroke == 1)),
    'frmr smoker' = sum(smoking_status == "formerly smoked"),
    'frmr smoker & strk' = sum(smoking_status == "formerly smoked" &
                                 (stroke == 1)),
    'non-smoker' = sum(smoking_status == "never smoked"),
    'non-smoker & strk'= sum(smoking_status == "never smoked" &
                               (stroke == 1)),
    'status unknown' = sum(smoking_status == "Unknown"),
    'status unknown & strk'= sum(smoking_status == "Unknown" &
                                       (stroke == 1)))


#correlation coefficients
cor_data <- cor(stroke_data[,sapply(stroke_data,is.numeric)],
    use="complete.obs",method="pearson")
round(cor_data,2)
 
#correlation matrix
ggcorrplot(cor_data, 
           hc.order = TRUE,
           lab = TRUE)
