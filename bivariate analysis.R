#PREDICTING STROKE WITH R

#importing necessary packages
library(dplyr)
library(readr)
library(tidyr)
library(SmartEDA)
library(ggplot2)
library(Hmisc)
library(treemapify)
library(dlookr)
#add dataset
stroke_data <- read.csv(file="healthcare-dataset-stroke-data.csv",
                        head=TRUE,
                        sep=",")

#Bivariate Analysis

#1. Gender vs Others
#relationship between gender and hypertension
stroke_data %>%
  group_by(gender, hypertension) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = hypertension, 
           fill = gender)) + 
  geom_bar(position = position_dodge(preserve = "single"))

#relationship between gender and heart disease
stroke_data %>%
  group_by(gender, heart_disease) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = heart_disease, 
           fill = gender)) + 
  geom_bar(position = position_dodge(preserve = "single"))

#relationship between gender and marital status
stroke_data %>%
  group_by(gender, ever_married) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = gender, 
           fill = ever_married)) + 
  geom_bar(position = position_dodge(preserve = "single"))


#relationship between gender and work type
stroke_data %>%
  group_by(gender, work_type) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = work_type, 
           fill = gender)) + 
  geom_bar(position = position_dodge(preserve = "single"))

#relationship between gender and residence type
stroke_data %>%
  group_by(gender, Residence_type) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = gender, 
           fill = Residence_type)) + 
  geom_bar(position = position_dodge(preserve = "single"))

#relationship between gender and smoking status
stroke_data %>%
  group_by(gender, smoking_status) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = smoking_status, 
           fill = gender)) + 
  geom_bar(position = position_dodge(preserve = "single"))

#relationship between gender and stroke
stroke_data %>%
  group_by(gender, stroke) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = stroke, 
           fill = gender)) + 
  geom_bar(position = position_dodge(preserve = "single"))


#2. Hypertension vs Others
#relationship between hypertension and heart disease
stroke_data %>%
  group_by(hypertension, heart_disease) %>%
  summarise(n = n())

#relationship between hypertension and marital status
stroke_data %>%
  group_by(hypertension, ever_married) %>%
  summarise(n = n())

#relationship between hypertension and work type
stroke_data %>%
  group_by(hypertension, work_type) %>%
  summarise(n = n())

#relationship between hypertension and residence type
stroke_data %>%
  group_by(hypertension, Residence_type) %>%
  summarise(n = n())

#relationship between hypertension and smoking status
stroke_data %>%
  group_by(hypertension, smoking_status) %>%
  summarise(n = n())

#relationship between hypertension and stroke
stroke_data %>%
  group_by(hypertension, stroke) %>%
  summarise(n = n())

#graphical representation
# Create hypertension counts table
hypercounts <- as.data.frame(table(stroke_data$hypertension, stroke_data$stroke))
# Replace num to char
hypercounts$Var1 <- ifelse(hypercounts$Var1 == 0, "No", 'Yes')
hypercounts$Var2 <- ifelse(hypercounts$Var2 == 0, "No", 'Yes')
# Replace headers
colnames(hypercounts)[1] <- 'Hypertension'
colnames(hypercounts)[2] <- 'Stroke'
# Bar Chart of Hypertension : No vs. Yes     
ggplot(hypercounts, aes(x = Hypertension, y = Freq, fill = Stroke)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Hypertension Status of Patients",x ="Hypertension", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

#3. Heart Disease vs Others
#relationship between heart_disease and marital status
stroke_data %>%
  group_by(heart_disease, ever_married) %>%
  summarise(n = n())

#relationship between heart_disease and work type
stroke_data %>%
  group_by(heart_disease, work_type) %>%
  summarise(n = n())

#relationship between heart_disease and residence type
stroke_data %>%
  group_by(heart_disease, Residence_type) %>%
  summarise(n = n())

#relationship between heart_disease and smoking status
stroke_data %>%
  group_by(heart_disease, smoking_status) %>%
  summarise(n = n())

#relationship between heart_disease and stroke
stroke_data %>%
  group_by(heart_disease, stroke) %>%
  summarise(n = n())
#graphical representation below
# Create heart disease counts table
heartcounts <- as.data.frame(table(stroke_data$heart_disease, stroke_data$stroke))
# Replace num to char
heartcounts$Var1 <- ifelse(heartcounts$Var1 == 0, "No", 'Yes')
heartcounts$Var2 <- ifelse(heartcounts$Var2 == 0, "No", 'Yes')
# Replace headers
colnames(heartcounts)[1] <- 'Heart_Disease'
colnames(heartcounts)[2] <- 'Stroke'
# Bar Chart of Heart Disease : No vs. Yes     
ggplot(heartcounts, aes(x = Heart_Disease, y = Freq, fill = Stroke)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Heart Disease Status of Patients",x ="Heart Disease", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))


#4. Marital Status vs Others
#relationship between ever_married and work type
stroke_data %>%
  group_by(ever_married, work_type) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = work_type, 
           fill = ever_married)) + 
  geom_bar(position = position_dodge(preserve = "single"))

#relationship between ever_married and residence type
stroke_data %>%
  group_by(ever_married, Residence_type) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = ever_married, 
           fill = Residence_type)) + 
  geom_bar(position = position_dodge(preserve = "single"))

#relationship between ever_married and smoking status
stroke_data %>%
  group_by(ever_married, smoking_status) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = smoking_status, 
           fill = ever_married)) + 
  geom_bar(position = position_dodge(preserve = "single"))

#relationship between ever_married and stroke
stroke_data %>%
  group_by(ever_married, stroke) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = stroke, 
           fill = ever_married)) + 
  geom_bar(position = position_dodge(preserve = "single"))


#5. Work type vs Others
#relationship between work_type and residence type
stroke_data %>%
  group_by(work_type, Residence_type) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = work_type, 
           fill = Residence_type)) + 
  geom_bar(position = position_dodge(preserve = "single"))

#graphical representation
ggplot(stroke_data, 
       aes(x = Residence_type, 
           fill = work_type)) + 
  geom_bar(position = position_dodge(preserve = "single"))

#relationship between work_type and smoking status
stroke_data %>%
  group_by(work_type, smoking_status) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = work_type, 
           fill = smoking_status)) + 
  geom_bar(position = position_dodge(preserve = "single"))

#relationship between work_type and stroke
stroke_data %>%
  group_by(work_type, stroke) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = stroke, 
           fill = work_type)) + 
  geom_bar(position = position_dodge(preserve = "single"))


#relationship between children and stroke
stroke_data %>%
  filter(work_type == 'children', stroke == 1)

#relationship between children and heart disease
stroke_data %>%
  filter(work_type == 'children', heart_disease == 1)

#relationship between children and smoking status
stroke_data %>%
  filter(work_type == 'children', smoking_status == 'formerly smoked')
stroke_data %>%
  filter(work_type == 'children', smoking_status == 'smokes')

#6. Residence type vs Others
#relationship between Residence_type and smoking status
stroke_data %>%
  group_by(Residence_type, smoking_status) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = smoking_status, 
           fill = Residence_type)) + 
  geom_bar(position = position_dodge(preserve = "single"))

#relationship between Residence_type and stroke
stroke_data %>%
  group_by(Residence_type, stroke) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = stroke, 
           fill = Residence_type)) + 
  geom_bar(position = position_dodge(preserve = "single"))

#7. Smoking status vs Others
#relationship between smoking_status and stroke
stroke_data %>%
  group_by(smoking_status, stroke) %>%
  summarise(n = n())

#graphical representation
ggplot(stroke_data, 
       aes(x = stroke, 
           fill = smoking_status)) + 
  geom_bar(position = position_dodge(preserve = "single"))
