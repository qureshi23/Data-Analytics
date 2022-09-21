library(readr)
df <- read.csv("D:/Subhan Qureshi/IEC/Projects/Project 2/brain_stroke.csv", 
                         col_types = cols(age = col_number(), 
                                          hypertension = col_number(), heart_disease = col_number(), 
                                          avg_glucose_level = col_number(), 
                                          bmi = col_number(), stroke = col_number()))

View(df)


#1 : Comparison smoking effects on heart disease

train %>%
  select(heart_disease, smoking_status) %>%
  count(heart_disease, smoking_status) %>%  
  #arrange(desc(gender)) %>%
  View()


#2 : Comparison smoking effects on heart disease gender wise

train %>%
  select(gender, heart_disease, smoking_status) %>%
  count(gender, heart_disease, smoking_status) %>%
  arrange(desc(gender)) %>%
  View()

#3 : Comparison work type effects on hypertension

train %>%
  select(hypertension, work_type) %>%
  count(hypertension, work_type) %>%
  View()


#4 : Comparison gender and residence effects on stroke

train %>%
  select(gender, Residence_type, stroke) %>%
  count(gender, Residence_type, stroke) %>%
  filter(stroke == 1) %>%
  arrange(desc(gender)) %>%
  View()

#5 : Comparison work type effects on heart diseases


train %>%
  select(heart_disease, work_type) %>%
  count(heart_disease, work_type) %>%
  View()




#6 : Comparison residence type effects on stroke


train %>%
  select(stroke, Residence_type) %>%
  count(stroke, Residence_type) %>%
  arrange(desc(n)) %>%
  View()

#7 : Comparison age effects on stroke

train %>%
  select(stroke, age) %>%
  count(stroke, age) %>%
  arrange(desc(n)) %>%
  View()

#8 : Comparison gender effects on stroke

train %>%
  select(stroke, gender) %>%
  count(stroke, gender) %>%
  arrange(desc(n)) %>%
  View()



# 1.  Can work type be linked to stroke?
ggplot(df, aes(x = work_type, fill = stroke))+
  geom_bar(position = "fill")+
  stat_count(geom = "text",
             aes(label = stat(count)),
             #position = "fill", color = "black"
             position = position_fill(vjust = 0.5), color = "black")

#2 Can marital status be linked to stroke?

ggplot(df, aes(x = stroke, fill = ever_married))+
  geom_bar(position = "fill")+
  stat_count(geom = "text",
             aes(label = stat(count)),
             #position = "fill", color = "black"
             position = position_fill(vjust = 0.5), color = "black")

#3 BMI Vs Stroke for each Gender and heart_disease Indicator

ggplot(df, aes(x = stroke, y = bmi, fill = heart_disease, color = gender))+
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5, position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))

#4  Smoking Status Vs stroke for each Residence Type and Glucose levels

ggplot(df, aes(x = stroke, y = avg_glucose_level))+
  geom_bar(aes(fill = Residence_type),stat = "identity", position = position_dodge(0.9))+
  facet_wrap(~smoking_status, scales = "free")


#Question.1 :.......................................
#.does male living in rural areas have more stroke rate then those who are living in urban areas?
Is it true that the stroke rate of people in urban areas is higher as compared to people in rural areas.what are the main reasons behind it.
(give theoretical reasons)
………………………………………
ggplot(aew,aes(x=aew$gender,fill=aew$stroke))+
  geom_bar()+
  facet_wrap(~Residence_type)


#question2.....................................
Does marital status affect glucose level or not ?
has the married ppl higher glucose level than other ppl?
(because 1.bmi incr. which increse glucose lvl 2.stress incresel) 
( first we prove this question then we will give reasons why this happens)
………………………………………

ggplot(df,aes(x=df$avg_glucose_level,y=df$gender,fill=df$gender))+
  geom_boxplot()+
  labs(x="Glucose Level",y="Gender",title="Marital Status VS Glucose level")+
  theme_bw()+
  facet_wrap(~df$ever_married)



#question3.......................
#Does smoking status increase or decrease the average BMI of  men?
What is the reason behind the fact that the people who used to smoke have lower BMI lvl?
(1st we prove it, then we give some theoretical reasons)
………………………………………
question3 <- aew%>%
  select(gender,bmi,smoking_status)%>%
  filter(gender=="Male")
View(question3)
#....................................
question3%>%
  ggplot(aew,mapping = aes(smoking_status,bmi,fill=gender)) +
  labs(x="smoking_status",y="BMI",title = "Smoking status VS BMI")+
  coord_flip() +
  theme_bw()+
  geom_boxplot(fill="lightblue")


#question.5............................
#Do the men and women who never smoked have heart disease or not ?
What is the ratio of heart disease in those people who never smoked then those ppl who mostly smoke. Why the smoking increases the chance of heart disease?
(1st we prove it then we give theoretical proves )
………………………………………


question5 <- aew%>%
  select(gender,heart_disease,smoking_status)
View(question5)


question5%>% ggplot(mapping = aes(heart_disease,fill=gender))+
  geom_bar()+
  facet_wrap(~smoking_status)+
  theme_bw()+
  theme_gray()+
  labs(y="none",title="Smoking VS heart disease")


#question.6..........................
#.do women living in urban areas have more heart diseases than women in rural areas?

Can we say that women who live in urban areas have a higher ratio of heart disease than other women? Even after the availability of good facilities, why the ratio increasing gradually.
(give theoretical reasons about this statement)
………………………………………
question6 <- aew%>%
  select(gender,Residence_type,heart_disease)
View(question6)


question6%>%
  ggplot(aes(x=gender,fill=heart_disease))+
  geom_bar()+facet_wrap(~Residence_type)+
  theme_bw()+
  labs(title="Area VS heart disease")

#...............................


#question.7..........................
#.do the work type affect bmi or not?

question8 <- aew%>%
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


#question.8.........................
#.does the men who are married have greater bmi then those men who are not married?

question9 <- aew%>%
  select(gender,ever_married,bmi)
View(question9)

question9%>%
  ggplot(mapping=aes(gender,bmi))+
  geom_boxplot(fill="yellow")+
  labs(x="Gender",y="bmi",title="Marital Status VS bmi")+
  theme_bw()+
  theme_gray()+
  facet_wrap(~ever_married)


question7 <- aew%>%
  select(gender,bmi,heart_disease)%>%
  filter(gender=="Male")
View(question7)

question7%>%
  ggplot(aes(x=gender,y=bmi,fill=gender))+
  geom_boxplot()+
  facet_wrap(~heart_disease)+
  theme_bw()+
  labs(y="bmi",title="heart disease VS BMI")


