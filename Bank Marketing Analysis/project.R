library(dplyr)
library(ggplot2)
data <- read.csv("D:\Subhan Qureshi\IEC\Projects\Project 3\Bank Marketing.csv", 
                 header = TRUE, sep = ";")
df <- read.csv("D:/Subhan Qureshi/IEC/Projects/Project 3/Bank Marketing.csv",header = TRUE, sep = ";")


View(df)
head(df)

#The column "y" has binary values "yes" and "no" (subscribed to a term deposit). I'm going to encode it into 1s and 0s. After that, I can easily calculate the converstion rate.

df <- df %>%
  mutate(y=ifelse(y=="no", 0, 1))
df$y <- as.integer(df$y)

#total number of conversions
sum(df$y)

#total number of clients in the data
nrow(df)

#conversion rate
sum(df$y)/nrow(df)*100.0


#Conversion Rate by Age

#group clients into 6 age groups(18-30, 30-40, 40-50, 50-60, 60-70, >70)
conversionsAgeGroup <- df %>%
  group_by(AgeGroup=cut(age, breaks=seq(20, 70, by=10))) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100)

#rename the 6th group
conversionsAgeGroup$AgeGroup <- as.character(conversionsAgeGroup$AgeGroup)
conversionsAgeGroup$AgeGroup[6] <- "70+"

#visualizing conversions by age group
ggplot(data=conversionsAgeGroup, aes(x=AgeGroup, y=ConversionRate)) +
  geom_bar(width=0.5, stat="identity", fill="darkgreen") + 
  labs(title="Conversion Rates by Age Group")


#Conversions by age group and marital status

# group the data
conversionsAgeMarital <- df %>%
  group_by(AgeGroup=cut(age, breaks=seq(20,70, by=10)),
           Marital=marital) %>%
  summarize(Count=n(), NumConversions=sum(y)) %>%
  mutate(TotalCount=sum(Count)) %>%
  mutate(ConversionRate=NumConversions/TotalCount*100)

#rename the last groups
conversionsAgeMarital$AgeGroup <- as.character(conversionsAgeMarital$AgeGroup)
conversionsAgeMarital$AgeGroup[is.na(conversionsAgeMarital$AgeGroup)] <- "70+"

#visualizing conversions by age group and marrital status
ggplot(conversionsAgeMarital, aes(x=AgeGroup, y=ConversionRate, fill=Marital)) +
  geom_bar(width=0.5, stat = "identity") +
  labs(title="Conversion Rates by Age Group and Marital Status")


#Conversions by job

#group the data
conversionsJob <- df %>%
  group_by(Job=job) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

#order the jobs DESC for the bar chart
conversionsJob$Job <- factor(conversionsJob$Job, 
                             levels = conversionsJob$Job[order(-conversionsJob$ConversionRate)])

# visualizing conversions by job
ggplot(conversionsJob, aes(x=Job, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="darkgreen") +
  labs(title="Conversion Rates by Job") +
  theme(axis.text.x = element_text(angle = 90))


#Conversions by education

#group the data
conversionsEdu <- df %>%
  group_by(Education=education) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

#order DESC for the bar chart
conversionsEdu$Education <- factor(conversionsEdu$Education, 
                                   levels = conversionsEdu$Education[order(-conversionsEdu$ConversionRate)])
#visualizing conversions by education
ggplot(conversionsEdu, aes(x=Education, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="darkgreen") +
  labs(title="Conversion Rates by Education") +
  theme(axis.text.x = element_text(angle = 90))


#Conversions by having or not a credit in default

#group the data
conversionsDefaultCredit <- df %>%
  group_by(HasCredit=default) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

#visualizing the data
ggplot(conversionsDefaultCredit, aes(x=HasCredit, y=ConversionRate, fill=HasCredit)) +
  geom_bar(width=0.5, stat = "identity") +
  labs(title="Conversion Rates by Default Credit")


#Conversions by having a housing loan and a personal loan

#group the data - housing loan
conversionsHousing <- df %>%
  group_by(HousingLoan=housing) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

#visualizing the data
ggplot(conversionsHousing, aes(x=HousingLoan, y=ConversionRate, fill=HousingLoan)) +
  geom_bar(width=0.5, stat = "identity") +
  labs(title="Conversion Rates by Housing Loan")

#group the data - personal loan
conversionsLoan <- df %>%
  group_by(Loan=loan) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))



#visualizing the data
ggplot(conversionsLoan, aes(x=Loan, y=ConversionRate, fill=Loan)) +
  geom_bar(width=0.5, stat = "identity") +
  labs(title="Conversion Rates by Personal Loan")


#Conversions by contact type

conversionsContact <- df %>%
  group_by(Contact=contact) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

head(conversionsContact)


#Conversions by the last contact month of a year

# group the data by months
conversionsMonth <- df %>%
  group_by(Month=month) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

#reorder DESC
conversionsMonth$Month <- factor(conversionsMonth$Month, 
                                 levels = conversionsMonth$Month[order(-conversionsMonth$ConversionRate)])
#visualizing the data
ggplot(conversionsMonth, aes(x=Month, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="darkgreen") +
  labs(title="Conversion Rates by Last Contact Month") +
  theme(axis.text.x = element_text(angle = 90))


#Conversions by the last contact day of a week

#group the data by days of a week
conversionsDayOfWeek <- df %>%
  group_by(Day_Of_Week=day_of_week) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

#reorder DESC
conversionsDayOfWeek$Day_Of_Week <- factor(conversionsDayOfWeek$Day_Of_Week, 
                                           levels = c("mon", "tue", "wed", "thu", "fri"))
#visualizing the data
ggplot(conversionsDayOfWeek, aes(x=Day_Of_Week, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="darkgreen") +
  labs(title="Conversion Rates by Last Contact Day of Week") +
  theme(axis.text.x = element_text(angle = 90))


#Correlation between subscribing to a term deposit and call duration

data_duration <- df %>%
  group_by(Subscribed=y) %>%
  summarise(Average_Duration=mean(duration))
head(data_duration)


#Conversions by the number of contacts performed during the campaign

conversionsCamp <- df %>%
  group_by(Campaign=campaign) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

head(conversionsCamp)


#Conversions by the outcome of the previous campaign

#group the data by the previous outcome
conversionsPOutcome <- df %>%
  group_by(Previous_Outcome=poutcome) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

# visualizing the data
ggplot(conversionsPOutcome, aes(x=Previous_Outcome, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="darkgreen") +
  labs(title="Conversion Rates by Outcome of the Previous Campaign")




