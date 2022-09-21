library(readr)
df <- read.csv("D:/Subhan Qureshi/IEC/Projects/Project 2/brain_stroke.csv", 
                         col_types = cols(age = col_number(), 
                                          hypertension = col_number(), heart_disease = col_number(), 
                                          avg_glucose_level = col_number(), 
                                          bmi = col_number(), stroke = col_number()))

View(df)

library(readr)
newDf <- read_csv("D:/Subhan Qureshi/IEC/Projects/Project 2/brain_stroke.csv", 
                         col_types = cols(age = col_integer(), 
                                          hypertension = col_number(), heart_disease = col_number(), 
                                          ever_married = col_character(), avg_glucose_level = col_number(), 
                                          bmi = col_number(), stroke = col_logical()))
View(newDf)


install.packages("tidyverse")
library(tidyverse)

install.packages("dplyr")
library(dplyr)


library(ggplot2)


str(df)

df <- drop_na(df)

str(df)

set.seed(1234)

rows <- sample(nrow(df))

df <- df[rows,]

nrow(df)*0.8


#train <- df[1:3985, ]

#test <- df[(3985+1):nrow(df), ]

#View(train)
#View(test)

summary(df)




is.na(df)

df <- df %>% mutate(hypertension=if_else(hypertension==1,"YES","NO"),heart_disease=if_else(heart_disease==1,"YES","NO"),stroke=if_else(stroke==1,"YES","NO"))
view(df)

train %>%
  select(gender, Residence_type, stroke) %>%
  count(gender, Residence_type, stroke) %>%
  filter(stroke == 1) %>%
  arrange(desc(gender)) %>%
  View()

ggplot(data = train ) + 
  geom_bar(mapping = aes(x=Residence_type, fill=gender)) + 
  facet_wrap(~gender)


ggplot(data = train ) + 
  geom_bar(mapping = aes(x = avg_glucose_level, fill= ever_married)) + 
  facet_wrap(~gender)


train %>%
  select(gender, heart_disease, smoking_status) %>%
  count(gender, heart_disease, smoking_status) %>%
  arrange(desc(gender)) %>%
  View()

train %>%
  select(heart_disease, smoking_status) %>%
  count(heart_disease, smoking_status) %>%
  
  #arrange(desc(gender)) %>%
  View()


train %>%
  select(hypertension, work_type) %>%
  count(hypertension, work_type) %>%
  View()

train %>%
  select(heart_disease, work_type) %>%
  count(heart_disease, work_type) %>%
  View()


train %>%
  select(stroke, Residence_type) %>%
  count(stroke, Residence_type) %>%
  arrange(desc(n)) %>%
  View()

train %>%
  select(stroke, age) %>%
  count(stroke, age) %>%
  arrange(desc(n)) %>%
  View()

train %>%
  select(stroke, gender) %>%
  count(stroke, gender) %>%
  arrange(desc(n)) %>%
  View()

train %>%
  select(Residence_type, work_type) %>%
  count(Residence_type, work_type) %>%
  arrange(desc(Residence_type)) %>%
  View()

train %>%
  select(gender, bmi) %>%
  filter(bmi < 18.5 ) %>%
  arrange(desc(bmi)) %>%
  View() 

ggplot(data=train, aes(x=gender, y=bmi)) + 
  {if(train$bmi < 18.5){title = "Underweight"}}+ 
  geom_line(aes(color = gender)) 

barplot(train$bmi < 18.5)

if ( train$bmi < 18.5) {
  print("Underweight")
} else if ( train$bmi < 24.9) {
  print("Healthyweight")
} else if ( train$bmi < 29.9) {
  print("Overweight")
} else {
  print("Obesity")
}


plot(train[train$bmi < 18.5,])


BMIdis <- 
  
train %>%
  select(gender, bmi) %>%
  #filter( bmi < 18.5 ) %>%
  mutate(bmi18.5 = bmi < 18.5) %>%
  #arrange(desc(bmi)) %>%
  group_by(gender) %>%
  mutate(average_BMI = mean(bmi)) %>%
  View() 
  #summarise()


train %>%
  select(gender, bmi) %>%
  mutate(Underweight = bmi < 18.5) %>%
  mutate(Healthyweight = bmi < 24.9) %>%
  mutate(Overweight = bmi < 29.9) %>%
  mutate(Obesity = bmi > 34.5) %>%
  view()

train$bmiDes <- Underweight = train$bmi < 18.5

train %>%
  select(gender, bmi) %>%
  mutate(train, bmi = ifelse(train$bmi < 18.5, "Pass", "Fail"))
  view()

str(train)

class(train$bmi) = "Numeric"





#Transform to normal distribution

hist(df$age)
hist(df$avg_glucose_level)
hist(df$bmi)


install.packages("ggpubr")
library(ggpubr)

install.packages("moments")
library(moments)



# Distribution of CONT variable
ggdensity(df, x = "age", fill = "lightgray", title = "AGE") +
  scale_x_continuous(limits = c(3, 12)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")


skewness(df$age, na.rm = TRUE)



# Distribution of CONT variable
ggdensity(df, x = "bmi", fill = "lightgray", title = "BMI") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")



qplot(x = age, data = df)

summary(df$age)

summary(log10(df$age))

summary(log10(df$age + 1))

summary(sqrt(df$age))


install.packages('rcompanion')
library(rcompanion)
install.packages('psych')
library(psych)

plotNormalHistogram(df$age)
age_sqrt <- sqrt(df$age)

plotNormalHistogram(age_sqrt)

age_cube <- sign(df$age) * abs(df$age)^(1/3)
plotNormalHistogram(age_cube)

age_log <- sign(df$age) * abs(df$age)^(1/3)
plotNormalHistogram(age_log)

age_tuk <- transformTukey(df, plotit = TRUE)
plotNormalHistogram(age_tuk)


install.packages('MASS')
library(MASS)


BOX <- boxcox(df$age~1, lambda = seq(-2,2,0.1))
Cox <- data.frame(Box$x, Box$y)


install.packages("gridExtra")
library(gridExtra)

a1 <- qplot(x = age, data = df)
a2 <- qplot(x = log10(age + 1), data = df)
a3 <- qplot(x = sqrt(age), data = df)

grid.arrange(a1, a2, a3, ncol = 1)


g1 <- qplot(x = avg_glucose_level, data = df)
g2 <- qplot(x = log10(avg_glucose_level + 1), data = df)
g3 <- qplot(x = sqrt(avg_glucose_level), data = df)

grid.arrange(g1, g2, g3, ncol = 1)


#Normal distribution of BMI

b1 <- qplot(x = bmi, data = df)
b2 <- qplot(x = log10(bmi + 1), data = df)
b3 <- qplot(x = sqrt(bmi), data = df)
b3

grid.arrange(b1, b2, b3, ncol = 1)


#Hypothesis Testing


summary(df$age)


x <- df$age
y <- df$bmi

y <- df$stroke
  
x <- rnorm(df$age)

# Two Sample T-Test
t.test(x, y)

t.test(df$age, mu = 40)
t.test(newDf$stroke, mu = 1)
#t.test(df$age, mu = 43.42, alternative = "less")
#t.test(df$age, mu = 43.42, alternative = "two.sided", conf = 0.95)

hist(x)
summary(x)
t.test(x, mu = 40)

#fisher.test(df$ever_married, df$stroke)




chisq.test(df$ever_married, df$stroke, correct=FALSE)



chisq.test(df$gender, df$stroke, correct=FALSE)

count(df$ever_married)

#Probability
x <- table(df$ever_married) %>% 
  prop.table()

y <- table(df$stroke) %>% 
  prop.table()


x <- 0.65 - 0.95
x <- -0.3


t.test(x, mu = 0)

cont.plot <- ggplot(data = df, aes(x= age, y = bmi, color = stroke))+geom_point()
cont.plot

ggplot(df,aes(x=df$gender,fill=df$stroke))+
  geom_bar()+
  labs(x="Gender",title="Stroke on Gender and Residence Type")+
  facet_wrap(~Residence_type)

ggplot(df,aes(x=df$avg_glucose_level,y=df$gender,fill=df$gender))+
  geom_boxplot()+
  labs(x="Glucose Level",y="Gender",title="Marital Status VS Glucose level")+
  theme_bw()+
  facet_wrap(~df$ever_married)

ggplot(df,mapping = aes(smoking_status,bmi,fill=gender)) +
  labs(x="smoking_status",y="BMI",title = "Smoking status VS BMI")+
  coord_flip() +
  theme_bw()+
  geom_boxplot(fill="lightblue")

question5 <- df%>%
  select(gender,heart_disease,smoking_status)
View(question5)


question5%>% ggplot(mapping = aes(heart_disease,fill=gender))+
  geom_bar()+
  facet_wrap(~smoking_status)+
  theme_bw()+
  theme_gray()+
  labs(title="Smoking VS heart disease")
  


question6 <- df%>%
  select(gender,Residence_type,heart_disease)
View(question6)


question6%>%
  ggplot(aes(x=gender,fill=heart_disease))+
  geom_bar()+facet_wrap(~Residence_type)+
  theme_bw()+
  labs(title="Area VS heart disease")

question8 <- df%>%
  select(gender,work_type,bmi)
View(question8)

question8%>%
  ggplot(aes(x=gender,y=bmi,fill=gender))+
  geom_boxplot()+
  facet_wrap(~work_type)+
  theme_bw()+
  theme_classic()+
  theme_gray()
labs(y="bmi",title="work type VS BMI")


question9 <- df%>%
  select(gender,ever_married,bmi)
View(question9)

question9%>%
  ggplot(mapping=aes(gender,bmi))+
  geom_boxplot(fill="lightblue")+
  labs(x="Gender",y="bmi",title="Marital Status VS bmi")+
  theme_bw()+
  theme_gray()+
  facet_wrap(~ever_married)



