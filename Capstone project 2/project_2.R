#1 Gender, Hypertension, Heart diseases, and marital status distribution


p1 <- ggplot(data = df) +geom_bar(mapping = aes(x = gender,fill = "steelblue"))
p2 <-ggplot(data = df) +geom_bar(mapping = aes(x = hypertension))
p3 <-ggplot(data = df) +geom_bar(mapping = aes(x = heart_disease)) 
p4 <-ggplot(data = df) +geom_bar(mapping = aes(x = ever_married)) 
grid.arrange(p1,p2,p3,p4, ncol= 2)


#2 Work type, Residence type, smoking status, and stroke distribution

p5 <-ggplot(data = df) +geom_bar(mapping = aes(x = work_type))
p6 <-ggplot(data = df) +geom_bar(mapping = aes(x = Residence_type))
p7 <-ggplot(data = df) +geom_bar(mapping = aes(x = smoking_status))
p8 <-ggplot(data = df) +geom_bar(mapping = aes(x = stroke))
grid.arrange(p5,p6,p7,p8, ncol= 2)


#3 Stroke on BMI and AGE

cont.plot <- ggplot(data = df, aes(x= age, y = bmi, color = stroke))+geom_point()
cont.plot

#4 Visualization of Age, Glucose Level, and BMI

c1 <- ggplot(data = df) + geom_histogram(mapping = aes(x = age), binwidth = 0.5, col = 'steelblue')
c2 <- ggplot(data = df) + geom_histogram(mapping = aes(x = avg_glucose_level), binwidth = 0.5, col = 'steelblue')
c3 <- ggplot(data = df) + geom_histogram(mapping = aes(x = bmi), binwidth = 0.5, col = 'steelblue')
grid.arrange(c1,c2,c3, ncol= 2)


#5 Work Type Vs Stroke

ggplot(df, aes(x = work_type, fill = stroke))+
  geom_bar(position = "fill")+
  stat_count(geom = "text",
             aes(label = stat(count)),
             #position = "fill", color = "black"
             position = position_fill(vjust = 0.5), color = "black")

#6 Marital Status vs Stroke

ggplot(df, aes(x = stroke, fill = ever_married))+
  geom_bar(position = "fill")+
  stat_count(geom = "text",
             aes(label = stat(count)),
             #position = "fill", color = "black"
             position = position_fill(vjust = 0.5), color = "black")

#7 Smoking Status vs Stroke

ggplot(df, aes(x = smoking_status, fill = stroke))+
  geom_bar(position = 'fill')+
  stat_count(geom = "text",
             aes(label = stat(count)),
             #position = "fill", color = "black"
             position = position_fill(vjust = 0.5), color = "black")

#8 stroke on gender & residence type

ggplot(df,aes(x=gender,fill=stroke))+
  geom_bar()+
  facet_wrap(~Residence_type)

#9 Has married people higher glucose level than other people?

ggplot(df,aes(x=df$avg_glucose_level,y=df$gender,fill=df$gender))+
  geom_boxplot()+
  labs(x="Glucose Level",y="Gender",title="Marital Status VS Glucose level")+
  theme_bw()+
  facet_wrap(~df$ever_married)

#10 What is the reason behind the fact that the people who used to smoke have lower BMI levels?

ggplot(df,mapping = aes(smoking_status,bmi,fill=gender)) +
  labs(x="smoking_status",y="BMI",title = "Smoking status VS BMI")+
  coord_flip() +
  theme_bw()+
  geom_boxplot(fill="lightblue")

#11 Does the work type affect BMI or not?

ggplot(df, mapping = aes(x=gender,y=bmi,fill=gender))+
  geom_boxplot()+
  facet_wrap(~work_type)+
  theme_bw()+
  theme_classic()+
  theme_gray()

#12 Can we say that people who live in urban areas have a higher ratio of heart disease than other people? Even after the availability of good facilities, Why is this so?


ggplot(df,aes(x=gender,fill=heart_disease))+
  geom_bar()+facet_wrap(~Residence_type)+
  theme_bw()

#13 What is the rate of heart disease in those who never smoked than in those who mostly smoke? Why the smoking increases the chance of heart disease?

ggplot(df,mapping = aes(heart_disease,fill=gender))+
  geom_bar()+
  facet_wrap(~smoking_status)+
  theme_bw()+
  theme_gray()+
  labs(y="none",title="Smoking VS heart disease")

#14 Do the men who are married have greater BMI than those men who are not married?

ggplot(df,mapping=aes(gender,bmi))+
  geom_boxplot(fill="lightblue")+
  labs(x="Gender",y="bmi",title="Marital Status VS bmi")+
  theme_bw()+
  theme_gray()+
  facet_wrap(~ever_married)

#15 Stroke Probability of Randomly Selected person who is male 


# Stroke Probability of Randomly Selected person who is Female ? 
  

xtabs(~smoking_status+stroke) %>% 
  prop.table()

#16 If we select a person randomly who smokes formerly, then what is probability that he had a stroke?

# If we select a person randomly who never smokes , then what is probability that he had a stroke?
  
#  If we select a person randomly who smokes , then what is probability that he had a stroke?


xtabs(~smoking_status+stroke) %>% 
  prop.table()

#17 If we select a married person randomly, then what is the probability that he had a stroke?

# If we select a person randomly who never married, then what is the probability that he had a stroke?
  
xtabs(~ever_married+stroke) %>% 
  prop.table()

#18 If we select a person randomly who lives in Rural area, then what is probability that he had a stroke?
  
#  If we select a person randomly who lives in Urban area, then what is probability that he had a stroke?
  
xtabs(~Residence_type+stroke) %>% 
  prop.table()