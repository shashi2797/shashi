setwd("E:\\Excelr Data\\R Codes\\Hyothesis Testing")

##### Normality Test##################
install.packages("readxl")

install.packages("readxl")
library(readxl)


######## Cutlets.mtw.xlsx data ###################

Cutlets.mtw<-read_excel(file.choose())# Cutlets.mtw.xlsx
View(Cutlets.mtw)
#attach(Cutlets.mtw)
colnames(Cutlets.mtw)<-c("Unit A","Unit B")
# Changing column names
View(Cutlets.mtw)
attach(Cutlets.mtw)

#############Normality test###############

shapiro.test(`Unit A`)
# p-value = 0.32 >0.05 so p high null fly => It follows normal distribution

shapiro.test(`Unit B`)
# p-value = 0.5225 >0.05 so p high null fly => It follows normal distribution

#############Variance test###############
#var(Unit A) =var(Unit B)
#var(Unit A) is not equal to var(Unit B)
var.test(`Unit A`)
var.test(`Unit A`,`Unit B`)#variance test
# p-value = 0.3136 > 0.05 so p high null fly => Equal variances

############2 sample T Test ##################
#mean(Unit A)=mean(Unit B)
#mean(Unit A) is not equal to mean(Unit B)
?t.test
t.test(`Unit A`,`Unit B`,alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test
# alternative = "two.sided" means we are checking for equal and unequal
# means
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal means
# p-value = 0.4723 < 0.05 accept alternate Hypothesis 
# unequal means


boxplot(`Unit A`)
boxplot(`Unit B`)

if pvalue is less use below codes
?t.test
t.test(`Unit A`,`Unit B`,alternative = "greater",var.equal = T)

# alternative = "greater means true difference is greater than 0
# Null Hypothesis -> (Unit A-Unit B) <= 0
# Alternative Hypothesis ->  (Unit A-Unit B) > 0
# p-value = 0.2361 > 0.05 => p low null go => accept alternate hypothesis
# InterestRateWaiver better strategy than StandardPromotion

##FOR LABTAT###

#############Anova (Lab TAT )##########
Lab TAT<-read_excel(file.choose())   # Lab TAT(unstacked).xlsx
View(LabTAT)
attach(LabTAT)
shapiro.test(Laboratory.1)
shapiro.test(Laboratory.2)
shapiro.test(Laboratory.3)
shapiro.test(Laboratory.4)
Stacked_Data <- stack(LabTAT)
View(Stacked_Data)
attach(Stacked_Data)
?bartlett.test
?aov
bartlett.test(values~ind, data=Stacked_Data) #test for homogeneity of variances
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
# p-value = 0.1069 > 0.05 accept null hypothesis 
#Null hypothesis: All means are equal
#Alternate hypothesis: means are not equal

boxplot(Laboratory.1)
boxplot(Laboratory.2)


#########Chi Square(Bahaman Research)#################

library(readxl)
BuyerRatio.xlsx<-read_excel(file.choose()) 
View(BuyerRatio.xlsx)
attach(BuyerRatio.xlsx)
?table
?attach
table(`Observed Values`,North)
table(`Observed Values`,South)
table(`Observed Values`,East)
table(`Observed Values`,West)


#t2 <- prop.table(table(Observed values))
#t1 <- table(North,South,East,West)
?chisq.test

chisq.test(East,`Observed Values`)
# p-value = 0.6315 > 0.05  => Accept null hypothesis
# => All countries have equal proportions 



###################Proportional T Test(Fantaloons data)##########
library(readxl)
Fantaloons<-read_excel(file.choose())   
View(Fantaloons) 
attach(Fantaloons)
table1 <- table(Weekdays,Weekend)
View(table1)
table1
?prop.test
prop.test(x=c(47,66),n=c(167,233),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
# two. sided -> means checking for equal proportions and unequal proportions of Male and Female
# p-value = 0.9681 > 0.05 accept null hypothesis i.e.
#equal proportions

# Unequal proportions 

prop.test(x=c(47,66),n=c(167,233),conf.level = 0.95,correct = FALSE,alternative = "less")
# Ho -> Proportions of Children >equal Proportions of Adults
# Ha -> Proportions of Adults > Proportions of Children
# p-value = 6.559e-05 <0.05  donotaccept null hypothesis 
# so proportion of Children < proportion of adults 
# launch the drinks shop



#############Anova (Lab TAT )##########
Lab TAT<-read_excel(file.choose())   # Lab TAT(unstacked).xlsx
View(LabTAT)

as.numeric(Error Free =0)
?attach(LabTAT)
shapiro.test(Phillippines)
shapiro.test(Laboratory.2)
shapiro.test(Laboratory.3)
shapiro.test(Laboratory.4)
Stacked_Data <- stack(LabTAT)
View(Stacked_Data)
attach(Stacked_Data)
?bartlett.test
?aov
bartlett.test(values~ind, data=Stacked_Data) #test for homogeneity of variances
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
# p-value = 0.1069 > 0.05 accept null hypothesis 
#Null hypothesis: All means are equal
#Alternate hypothesis: means are not equal

boxplot(Laboratory.1)
boxplot(Laboratory.2)

