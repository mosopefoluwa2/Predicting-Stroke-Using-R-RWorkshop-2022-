#MULTIVARIATE ANALYSIS
#find a name for this
stroke_data %>%
  group_by(gender, stroke) %>%
  summarise(
    n = n(),
    'median age' = median(age))

#also find a name for this
stroke_data %>%
  group_by(gender) %>%
  summarise(
    'mean age' = mean(age),
    'n' = n(),
    'from rural area' = sum(Residence_type == 'Rural'),
    'from rural area and smoking' = sum(Residence_type == 'Rural' 
                                        & (smoking_status == 'smokes'))
  )


stroke_data %>%
  filter(work_type == 'Govt_job', stroke == 1, heart_disease == 1)