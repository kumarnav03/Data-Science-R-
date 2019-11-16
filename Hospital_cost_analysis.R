
setwd("G:\\Simplilearn\\R\\Naveen")

library(readxl)
hos_cost = read_excel("hospitalcosts.xlsx")
str(hos_cost)
summary(hos_cost)
sum(anyNA(hos_cost))

#converting desired variables into factor

hos_cost$FEMALE = as.factor(hos_cost$FEMALE)
hos_cost$RACE = as.factor((hos_cost$RACE))

# removed the NA values as count is small and won't impact my data

hos_cost1 = hos_cost[-which(is.na(hos_cost$RACE)),]

# Q1 -To record the patient statistics, the agency wants
#to find the age category of people who frequent the hospital and has the maximum expenditure.

library(ggplot2)
library(dplyr)

hos_cost1$AgeCategory = cut(hos_cost1$AGE,breaks = c(-1,4,8,12,16,20),
                            labels = c("0-4","4-8","8-12","12-16","16-20"))

hos_cost1$AgeCategory1 = cut(hos_cost1$AGE, breaks = c(-1,1,4,8,10,12,14,16,18,20),
                             labels = c("0-1","1-4","4-8","8-10","10-12","12-14","14-16","16-18","18-20") )

# Uni-variate data visualisation - histogram
hist(hos_cost1$AGE, breaks = c(0,2,4,6,8,10,12,14,16,18,20), main = "Histogram of Patients Age",
     xlab = "Age Category")

# Uni-variate data visualisation - KDE
ggplot(hos_cost1,aes(x= AGE))+
  geom_density()+
  ggtitle("Frequency distribution density")

# Uniform Age category with frequesnt stay and totat expenditure
hos_cost1 %>%
  group_by(AgeCategory) %>%
  summarise(sumexp = sum(TOTCHG),sumstay = sum(LOS)) %>%
  select(AgeCategory, sumexp , sumstay) %>%
  arrange(desc(sumexp,sumstay))

# Non- uniform Age of pateints with frequent stay and total expenditure
hos_cost1 %>%
  group_by(AgeCategory1) %>%
  summarise(sumexp = sum(TOTCHG),sumstay = sum(LOS)) %>%
  select(AgeCategory1, sumexp , sumstay) %>%
  arrange(desc(sumexp,sumstay))


# Q2 - On basis of severity & treatment expense find diagonisis related groups that 
# has maximun hospitalization & expenditure

 ans2 = hos_cost1 %>%
  select(APRDRG,TOTCHG,LOS) %>%
  group_by(APRDRG) %>%
  summarise( SumExp = sum(TOTCHG) , SumStay = sum(LOS)) %>%
  arrange(desc(SumExp,SumStay))

View(ans2)
   

# Q3 - to make sure there is o malpractice , agency wants to idnetify 
   # the relationship between race & hospital cost

# We will do Data visualisation to see how the data(RACE) varies in our dataset
# And we will do data visualization to see the variance in hosiptal cost across all RACE
# Boxplot shows that there is high varience for race 2 i.e. Race 2 has paid more amount as compared
# to others. So we will do one-way ANOVA test to confirm

race_vs_cost = table(hos_cost1$RACE)

barplot(race_vs_cost,col = rainbow(6), legend = rownames(race_vs_cost),xlab = "RACE",ylab = "Frequency"
        ,main = "Race distribution", ylim = c(0,500),beside = TRUE)

boxplot(log(hos_cost1$TOTCHG) ~ hos_cost1$RACE, xlab = "Types of RACE",
        ylab = "Log Function of hospital cost",main=" Race Distribution",col = rainbow(6)) 

# We are doing anova test to idnetify if there is relation between RACE and TOTCHG

# Ho = mean(Race1) = mean(race2) = mean(Race3) = mean(race4)
# Ha = mean(Race1) != mean(Race2) !=mean(Race3) != mean(Race4) != mean(Race5)


# Calculating std deviation of log of total hospital cost
hos_cost1 %>%
     group_by(RACE) %>%
     summarise(min(TOTCHG), mean(TOTCHG),median(TOTCHG),max(TOTCHG),sd(TOTCHG),sd(log(TOTCHG)))

# this is one-way anova test where one variable is categorical
race_model = aov(formula = TOTCHG ~ RACE, data = hos_cost1)
race_model
summary(race_model)
#  RACE is not related to cost as p-value =  0.943 > 0.05(alpha) 
#Means  we fail to reject the NUll hypotheses at all alpha value
# Statistically we don't have enough evidence to proof that the hospital is doing malpractice



#Q4 - To properly utilize the costs, the agency has to analyze
#the severity of the hospital costs by age and gender for the proper allocation of resources

#Box plot against age category and sex with the total hospital cost

hos_cost1 %>%
  ggplot(aes(x= AgeCategory, y = TOTCHG , fill = FEMALE))+
  geom_boxplot()+
  ggtitle("Hospital cost distribution based on Age & Sex") +
  scale_x_discrete(" Age Category") +
  scale_y_continuous(" Hospital cost")

# Box plot against age category & sex with log func of total hospital cost

hos_cost1 %>%
  ggplot(aes(x= AgeCategory, y = log(TOTCHG) , fill = FEMALE))+
  geom_boxplot()+
  ggtitle("Hospital cost (Log)distribution based on Age & Sex") +
  scale_x_discrete(" Age Category") +
  scale_y_continuous(" Hospital cost")

# extracting the data/values for age category and sex with total cost

hos_cost1 %>%
  group_by(AgeCategory,FEMALE) %>%
  select(AgeCategory,FEMALE,TOTCHG) %>%
  summarise(sum(TOTCHG))

# Seeing if age & sex is significant variable for total hospital cost
# since the p-value is less than 0.05 for both Age & Sex so we can say both significant variable
model1 = aov(formula = TOTCHG ~ AGE + FEMALE, data = hos_cost1)
summary(model1)

# AGE - Pr(>F) 0.00323 ** (< 0.01 it is significant even at alpha = 1%)
# FEMALE - Pr(>F) 0.03638 * (< 0.05 it is significant even at alpha = 5%)
# As per proabilty value we can say that both Age & sex are significant variable and 
# Age is more significant variable than Sex


#Q5 - Since the length of stay is the crucial factor for inpatients, 
# the agency wants to find if the length of stay can be predicted from age, gender, and race.
hos_cost2 = hos_cost[-which(is.na(hos_cost$RACE)),]
model5 = lm(formula = LOS ~ AGE + FEMALE + RACE,data = hos_cost2)
summary(model5)


#Q6 - To perform a complete analysis, 
# the agency wants to find the variable that mainly affects hospital costs.

hos_cost2 = hos_cost[-which(is.na(hos_cost$RACE)),]
model6 = lm(formula = TOTCHG ~. , data = hos_cost2)
summary(model6)
