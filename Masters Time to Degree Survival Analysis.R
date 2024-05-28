library(tidyverse) # for data manipulation and plots
library(jtools) #for transforming model summaries
library(caret)
library(readxl) #for importing data from an Excel file
library(dplyr)
library(sqldf) #for manipulating dataframes using sql
library(pscl)
library(splines)
library(glmtoolbox)
library(pROC)
library("glue")
library(corrplot)
library("ResourceSelection")



#Import data -----
#For guidance on data structure, review the Masters Time to Degree-Dictionary file
Masters_TTD<-read_excel("C:/Users/angela.rockwell/University of Northern Colorado/IRE Team - Documents/Angela/Archive/AIR Forum 2024/Masters Time to Degree.xlsx")


#1. Review the dataset -----
#1a. Check that there is no missing data and that the range and distribution of each variable is reasonable-----
colSums(is.na(Masters_TTD)) #counts missing values by column

time_invariant<-sqldf('select distinct GraduatedOutcome, WithdrawnOutcome, InitialTermI, Woman, AgeAtBegin, CoachingParticipant, EmployerSponsored
  from Masters_TTD
  ;') #subsets just the time invariant variables and unduplicates by student

time_varying<-sqldf('select WithdrawnStatus, GraduatedStatus, EnrollmentTermI, StuTermI, TermCredits, TermGPA, NeedBasedScholarship, Working
  from Masters_TTD
  ;') #subsets the time varying variables

summary(time_invariant) #distribution of each time invariant variable
summary(time_varying) #distribution of each time varying variable

#1b. Check the correlations between the independent and dependent variables -----
time_invar_corr<-cor(time_invariant, use = "pairwise.complete.obs") #correlations between the time invariant variables and the graduation and withdrawal outcomes
for_full_cor<-sqldf('select WithdrawnStatus, GraduatedStatus, InitialTermI, EnrollmentTermI,
  StuTermI, TermCredits, TermGPA, Woman, AgeAtBegin, CoachingParticipant, EmployerSponsored, NeedBasedScholarship, Working
  from Masters_TTD
  ;') #subsets the quantitative variables for calculating bivariate correlations
full_corr<-cor(for_full_cor, use = "pairwise.complete.obs")

UNCcorcolor <- colorRampPalette(c("#F6b000", "#F2d383", "#FFFFFF", "#4f748b", "#013c65")) #UNC's colors

#graphical representation of time invariant variables correlations
corrplot(time_invar_corr, 
         method="color",
         col=UNCcorcolor(200), 
         diag=FALSE, 
         # tl.pos="d", 
         type="lower", 
         tl.cex = 1.1,
         order="hclust", 
         title= "Correlations Outcomes and Time Invariant Variables", 
         cex.main = 1.6,
         addCoef.col = "black", 
         tl.col = 'black',
         tl.srt = 45,
         sig.level = 0.05, 
         insig = "blank", 
         mar=c(0,0,1,0)) 

#graphical representation of time varying variables correlations
corrplot(full_corr, 
         method="color",
         col=UNCcorcolor(200), 
         diag=FALSE, 
         # tl.pos="d", 
         type="lower", 
         order="hclust", 
         title= "Correlations Among Outcomes and Time Varying Variables", 
         addCoef.col = "black", 
         tl.col = 'black',
         sig.level = 0.05, 
         insig = "blank", 
         mar=c(0,0,1,0)) 

#2. Checking Assumptions -----
#2a. split data into 80% training and 20% testing datasets -----
set.seed(80639)
Student_list<-sqldf("select distinct StudentID
                     from Masters_TTD;")
row_count<-nrow(Student_list)
rows_80pct<-round(row_count*.8,0)
Student_Train_list <-sample_n(Student_list, rows_80pct)
Masters_TTD_Train <- Masters_TTD[Masters_TTD$StudentID %in% Student_Train_list$StudentID,]
Masters_TTD_Test <- Masters_TTD[!Masters_TTD$StudentID %in% Student_Train_list$StudentID,]

#2b. Review baseline hazard curves for linearity -----
With_Baseline_Hazards<-Masters_TTD_Train %>%
  group_by(StuTermI) %>%
  summarise(WithdrawnStatus = sum(WithdrawnStatus),
            total = n()) %>%
  mutate(hazard = WithdrawnStatus/total) 

With_Baseline_Hazards%>%
  ggplot(aes(x = StuTermI, y = log(-log(1-hazard)))) +
  geom_point() +
  geom_smooth() +
  ggtitle("Baseline Withdrawl Hazard Curve") +
  theme_bw() +
  theme(plot.title = element_text(, face="bold", , size=20,hjust = 0.5), axis.title.x = element_text(, "bold", , size=16), axis.title.y = element_text(, "bold", , size=16)) 

Grad_Baseline_Hazards<-Masters_TTD_Train %>%
  group_by(StuTermI) %>%
  summarise(GraduatedStatus = sum(GraduatedStatus),
            total = n()) %>%
  mutate(hazard = GraduatedStatus/total) 

Grad_Baseline_Hazards%>%
  ggplot(aes(x = StuTermI, y = log(-log(1-hazard)))) +
  geom_point() +
  geom_smooth() +
  ggtitle("Baseline Graduation Hazard Curve") +
  theme_bw() +
  theme(plot.title = element_text(, face="bold", , size=20,hjust = 0.5), axis.title.x = element_text(, "bold", , size=16), axis.title.y = element_text(, "bold", , size=16)) 


#2c. Create one set of splines for each outcome so that the proportional hazards assumption holds for each spline -----
Masters_TTD$StuTerm1_ind <- ifelse (Masters_TTD$StuTermI == 1, 1, 0)
Masters_TTD$StuTerm2_ind <- ifelse (Masters_TTD$StuTermI == 2, 1, 0)
Masters_TTD$StuTerm3_ind <- ifelse (Masters_TTD$StuTermI == 3, 1, 0)
Masters_TTD$StuTerm4_ind <- ifelse (Masters_TTD$StuTermI == 4, 1, 0)
Masters_TTD$StuTerm5_ind <- ifelse (Masters_TTD$StuTermI == 5, 1, 0)
Masters_TTD$StuTerm6_ind <- ifelse (Masters_TTD$StuTermI == 6, 1, 0)
Masters_TTD$StuTerm7_ind <- ifelse (Masters_TTD$StuTermI == 7, 1, 0)
Masters_TTD$StuTerm8_ind <- ifelse (Masters_TTD$StuTermI == 8, 1, 0)
#The graduation hazard curve looks to be non-linear, but there is insufficient evidence of it's non-linearity to justify making splines
#The withdrawal hazard has a clear knot at StuTermI = 4, so two separate regression lines are required. The estimation procedure will 
  #already estimate one intercept, so you only need to create one variable for the slope for each of the two lines (the time dependent 
  #component of the splines) and a second intercept variable. Which intercept you leave for the procedure to estimate will effect the 
  #values of the estimated parameters and their interpretations
Masters_TTD_Train$With_Time_Spline1_slope <- ifelse (Masters_TTD_Train$StuTermI <= 2, Masters_TTD_Train$StuTermI, 0)

Masters_TTD_Train$With_Time_Spline2_int <- ifelse (Masters_TTD_Train$StuTermI > 2, 1, 0)
Masters_TTD_Train$With_Time_Spline2_slope <- ifelse (Masters_TTD_Train$StuTermI > 2, Masters_TTD_Train$StuTermI, 0)

Masters_TTD_Test$With_Time_Spline1_slope <- ifelse (Masters_TTD_Test$StuTermI <= 2, Masters_TTD_Test$StuTermI, 0)

Masters_TTD_Test$With_Time_Spline2_int <- ifelse (Masters_TTD_Test$StuTermI > 2, 1, 0)
Masters_TTD_Test$With_Time_Spline2_slope <- ifelse (Masters_TTD_Test$StuTermI > 2, Masters_TTD_Test$StuTermI, 0)
#2d. Run Gompertz regression for each outcome to get baseline hazard curves -----
Baseline_Grad <- glm(formula = GraduatedStatus ~ StuTermI ,
                               family = binomial(link = "cloglog"),
                               data = Masters_TTD_Train) 
summary(Baseline_Grad)


Baseline_With <- glm(formula = WithdrawnStatus ~ With_Time_Spline1_slope + With_Time_Spline2_int + With_Time_Spline2_slope,
                     family = binomial(link = "cloglog"),
                     data = Masters_TTD_Train) 
summary(Baseline_With)

#Since the second intercept is not significant, remove that variable and re-run the Gompertz regression. With a larger sample size (more 
  #observed withdrawals) the second intercept may become significant.
Baseline_With <- glm(formula = WithdrawnStatus ~ With_Time_Spline1_slope + With_Time_Spline2_slope,
                     family = binomial(link = "cloglog"),
                     data = Masters_TTD_Train)

summary(Baseline_With)

#2e. Check for linearity between "continuous" variables and the probability of the outcomes using the Hosmer Lemishow test -----

#Grad Outcome-Term GPA
  #If you consider GPA to be a continuous variable, it generally needs to be transformed. Try quadratic, log, and cube root transformations.
  #Here I choose to treat it as continuous to save degrees of freedom, so the severe left skew needs to be addressed
hist(Masters_TTD_Train$TermGPA)
GPA_Grad <- glm(formula = GraduatedStatus ~ StuTermI + TermGPA,
                     family = binomial(link = "cloglog"),
                     data = Masters_TTD_Train) 
hoslem.test(Masters_TTD_Train$GraduatedStatus,GPA_Grad$fitted.values, g=10) #Use at least 10 bins: g = 10, but you'll pick up more noise the more bins you use

#Transform until the test is not significant, or as close as you can get
#Quadratic
Masters_TTD_Train$TermGPA_2Power <- Masters_TTD_Train$TermGPA^2
GPA2_Grad <- glm(formula = GraduatedStatus ~ StuTermI + TermGPA_2Power,
                family = binomial(link = "cloglog"),
                data = Masters_TTD_Train) 
hoslem.test(Masters_TTD_Train$GraduatedStatus,GPA2_Grad$fitted.values, g=10) 

#Log
Masters_TTD_Train$LogTermGPA <- log(Masters_TTD_Train$TermGPA+.00001)
LogGPA_Grad <- glm(formula = GraduatedStatus ~ StuTermI + LogTermGPA,
                        family = binomial(link = "cloglog"),
                        data = Masters_TTD_Train) 
hoslem.test(Masters_TTD_Train$GraduatedStatus,LogGPA_Grad$fitted.values, g=10)

#Cube root
Masters_TTD_Train$TermGPA_cube_root <- Masters_TTD_Train$TermGPA^(1/3)
GPA_cube_root_Grad <- glm(formula = GraduatedStatus ~ StuTermI + TermGPA_cube_root,
                 family = binomial(link = "cloglog"),
                 data = Masters_TTD_Train) 
hoslem.test(Masters_TTD_Train$GraduatedStatus,GPA_cube_root_Grad$fitted.values, g=10) 

#None of the transformations resolve the non-linearity, so I'll leave it as it is for the graduation outcome analysis


#Withdrawn Outcome-TermGPA
GPA_With <- glm(formula = WithdrawnStatus ~ With_Time_Spline1_slope + With_Time_Spline2_slope + TermGPA,
                     family = binomial(link = "cloglog"),
                     data = Masters_TTD_Train) 
hoslem.test(Masters_TTD_Train$WithdrawnStatus,GPA_With$fitted.values, g=10)
#There is insufficient evidence of non-linearity to warrant transforming the TermGPA for the withdrawn outcome analysis

#Grad Outcome-AgeAtBegin
hist(Masters_TTD_Train$AgeAtBegin)

Age_Grad <- glm(formula = GraduatedStatus ~ StuTermI + AgeAtBegin,
                family = binomial(link = "cloglog"),
                data = Masters_TTD_Train) 
hoslem.test(Masters_TTD_Train$GraduatedStatus,Age_Grad$fitted.values, g=10) 

#Square Root
Masters_TTD_Train$SQRT_AgeAtBegin <- Masters_TTD_Train$AgeAtBegin^(1/2)
SQRT_Age_Grad <- glm(formula = GraduatedStatus ~ StuTermI + SQRT_AgeAtBegin,
                 family = binomial(link = "cloglog"),
                 data = Masters_TTD_Train) 
hoslem.test(Masters_TTD_Train$GraduatedStatus,SQRT_Age_Grad$fitted.values, g=10) 

#Log
Masters_TTD_Train$LogAgeAtBegin <- log(Masters_TTD_Train$AgeAtBegin+.00001)
Log_Age_Grad <- glm(formula = GraduatedStatus ~ StuTermI + LogAgeAtBegin,
                   family = binomial(link = "cloglog"),
                   data = Masters_TTD_Train) 
hoslem.test(Masters_TTD_Train$GraduatedStatus,Log_Age_Grad$fitted.values, g=10)

#Cube root
Masters_TTD_Train$TermGPA_cube_root <- Masters_TTD_Train$AgeAtBegin^(1/3)
Age_cube_root_Grad <- glm(formula = GraduatedStatus ~ StuTermI + TermGPA_cube_root,
                          family = binomial(link = "cloglog"),
                          data = Masters_TTD_Train) 
hoslem.test(Masters_TTD_Train$GraduatedStatus,Age_cube_root_Grad$fitted.values, g=10) 

#None of the transformations resolve the non-linearity, so I'll leave it as it is for the graduation outcome analysis

#Withdrawn Outcome
Age_With <- glm(formula = WithdrawnStatus ~ With_Time_Spline1_slope + With_Time_Spline2_slope + AgeAtBegin,
                family = binomial(link = "cloglog"),
                data = Masters_TTD_Train) 
hoslem.test(Masters_TTD_Train$WithdrawnStatus,Age_With$fitted.values, g=10)
#There is insufficient evidence of non-linearity to warrant transforming the AgeAtBegin for the withdrawn outcome analysis

#2f. Check for proportional hazards assumption between independent variables and outcomes -----
#Lines should be roughly parallel and not cross, unless the number of total observations is very small for that time period is very low i.e. <5
#Visual inspection of proportional hazards assumption for TermCredits for the graduated outcome
Masters_TTD_Train %>%
  group_by(StuTermI, TermCredits) %>%
  summarise(event = sum(GraduatedStatus),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = StuTermI, group =TermCredits,
             y = log(-log(1-hazard)),
             col = TermCredits)) +
  geom_point() +
  geom_line(aes(group = TermCredits)) +
  ggtitle("Graduation Hazard Curve by TermCredits") +
  theme_bw() +
  theme(plot.title = element_text(, face="bold", , size=20,hjust = 0.5), axis.title.x = element_text(, "bold", , size=16), axis.title.y = element_text(, "bold", , size=16)) 

#Visual inspection of proportional hazards assumption for TermCredits for the withdrawn outcome
Masters_TTD_Train %>%
  group_by(StuTermI, TermCredits) %>%
  summarise(event = sum(WithdrawnStatus),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = StuTermI, group =TermCredits,
             y = log(-log(1-hazard)),
             col = TermCredits)) +
  geom_point() +
  geom_line(aes(group = TermCredits))+
  ggtitle("Withdrawal Hazard Curve by TermCredits") +
  theme_bw() +
  theme(plot.title = element_text(, face="bold", , size=20,hjust = 0.5), axis.title.x = element_text(, "bold", , size=16), axis.title.y = element_text(, "bold", , size=16)) 

#Visual inspection of proportional hazards assumption for Woman for the graduated outcome
Masters_TTD_Train %>%
  group_by(StuTermI, Woman) %>%
  summarise(event = sum(GraduatedStatus),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = StuTermI, group =Woman,
             y = log(-log(1-hazard)),
             col = Woman)) +
  geom_point() +
  geom_line(aes(group = Woman)) +
  ggtitle("Graduation Hazard Curve by Woman") +
  theme_bw() +
  theme(plot.title = element_text(, face="bold", , size=20,hjust = 0.5), axis.title.x = element_text(, "bold", , size=16), axis.title.y = element_text(, "bold", , size=16)) 

#Visual inspection of proportional hazards assumption for Woman for the withdrawn outcome
Masters_TTD_Train %>%
  group_by(StuTermI, Woman) %>%
  summarise(event = sum(WithdrawnStatus),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = StuTermI, group =Woman,
             y = log(-log(1-hazard)),
             col = Woman)) +
  geom_point() +
  geom_line(aes(group = Woman))+
  ggtitle("Withdrawal Hazard Curve by Woman") +
  theme_bw() +
  theme(plot.title = element_text(, face="bold", , size=20,hjust = 0.5), axis.title.x = element_text(, "bold", , size=16), axis.title.y = element_text(, "bold", , size=16)) 

#Visual inspection of proportional hazards assumption for CoachingParticipant for the graduated outcome
Masters_TTD_Train %>%
  group_by(StuTermI, CoachingParticipant) %>%
  summarise(event = sum(GraduatedStatus),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = StuTermI, group =CoachingParticipant,
             y = log(-log(1-hazard)),
             col = CoachingParticipant)) +
  geom_point() +
  geom_line(aes(group = CoachingParticipant))+
  ggtitle("Graduation Hazard Curve by CoachingParticipant") +
  theme_bw() +
  theme(plot.title = element_text(, face="bold", , size=20,hjust = 0.5), axis.title.x = element_text(, "bold", , size=16), axis.title.y = element_text(, "bold", , size=16)) 

#Visual inspection of proportional hazards assumption for CoachingParticipant for the withdrawn outcome
Masters_TTD_Train %>%
  group_by(StuTermI, CoachingParticipant) %>%
  summarise(event = sum(WithdrawnStatus),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = StuTermI, group =CoachingParticipant,
             y = log(-log(1-hazard)),
             col = CoachingParticipant)) +
  geom_point() +
  geom_line(aes(group = CoachingParticipant))+
  ggtitle("Withdrawal Hazard Curve by CoachingParticipant") +
  theme_bw() +
  theme(plot.title = element_text(, face="bold", , size=20,hjust = 0.5), axis.title.x = element_text(, "bold", , size=16), axis.title.y = element_text(, "bold", , size=16)) 

#Visual inspection of proportional hazards assumption for EmployerSponsored for the graduated outcome
Masters_TTD_Train %>%
  group_by(StuTermI, EmployerSponsored) %>%
  summarise(event = sum(GraduatedStatus),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = StuTermI, group =EmployerSponsored,
             y = log(-log(1-hazard)),
             col = EmployerSponsored)) +
  geom_point() +
  geom_line(aes(group = EmployerSponsored))+
  ggtitle("Graduation Hazard Curve by EmployerSponsored") +
  theme_bw() +
  theme(plot.title = element_text(, face="bold", , size=20,hjust = 0.5), axis.title.x = element_text(, "bold", , size=16), axis.title.y = element_text(, "bold", , size=16)) 

#Visual inspection of proportional hazards assumption for EmployerSponsored for the withdrawn outcome
Masters_TTD_Train %>%
  group_by(StuTermI, EmployerSponsored) %>%
  summarise(event = sum(WithdrawnStatus),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = StuTermI, group =EmployerSponsored,
             y = log(-log(1-hazard)),
             col = EmployerSponsored)) +
  geom_point() +
  geom_line(aes(group = EmployerSponsored))+
  ggtitle("Withdrawal Hazard Curve by EmployerSponsored") +
  theme_bw() +
  theme(plot.title = element_text(, face="bold", , size=20,hjust = 0.5), axis.title.x = element_text(, "bold", , size=16), axis.title.y = element_text(, "bold", , size=16)) 

#Visual inspection of proportional hazards assumption for NeedBasedScholarship for the graduated outcome
Masters_TTD_Train %>%
  group_by(StuTermI, NeedBasedScholarship) %>%
  summarise(event = sum(GraduatedStatus),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = StuTermI, group =NeedBasedScholarship,
             y = log(-log(1-hazard)),
             col = NeedBasedScholarship)) +
  geom_point() +
  geom_line(aes(group = NeedBasedScholarship))+
  ggtitle("Graduation Hazard Curve by NeedBasedScholarship") +
  theme_bw() +
  theme(plot.title = element_text(, face="bold", , size=20,hjust = 0.5), axis.title.x = element_text(, "bold", , size=16), axis.title.y = element_text(, "bold", , size=16)) 

#Visual inspection of proportional hazards assumption for NeedBasedScholarship for the withdrawn outcome
Masters_TTD_Train %>%
  group_by(StuTermI, NeedBasedScholarship) %>%
  summarise(event = sum(WithdrawnStatus),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = StuTermI, group =NeedBasedScholarship,
             y = log(-log(1-hazard)),
             col = NeedBasedScholarship)) +
  geom_point() +
  geom_line(aes(group = NeedBasedScholarship))+
  ggtitle("Withdrawal Hazard Curve by NeedBasedScholarship") +
  theme_bw() +
  theme(plot.title = element_text(, face="bold", , size=20,hjust = 0.5), axis.title.x = element_text(, "bold", , size=16), axis.title.y = element_text(, "bold", , size=16)) 

#Visual inspection of proportional hazards assumption for Working for the graduated outcome
Masters_TTD_Train %>%
  group_by(StuTermI, Working) %>%
  summarise(event = sum(GraduatedStatus),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = StuTermI, group =Working,
             y = log(-log(1-hazard)),
             col = Working)) +
  geom_point() +
  geom_line(aes(group = Working))+
  ggtitle("Graduation Hazard Curve by Working") +
  theme_bw() +
  theme(plot.title = element_text(, face="bold", , size=20,hjust = 0.5), axis.title.x = element_text(, "bold", , size=16), axis.title.y = element_text(, "bold", , size=16)) 

#Visual inspection of proportional hazards assumption for Working for the withdrawn outcome
Masters_TTD_Train %>%
  group_by(StuTermI, Working) %>%
  summarise(event = sum(WithdrawnStatus),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = StuTermI, group =Working,
             y = log(-log(1-hazard)),
             col = Working)) +
  geom_point() +
  geom_line(aes(group = Working))+
  ggtitle("Withdrawal Hazard Curve by Working") +
  theme_bw() +
  theme(plot.title = element_text(, face="bold", , size=20,hjust = 0.5), axis.title.x = element_text(, "bold", , size=16), axis.title.y = element_text(, "bold", , size=16)) 

#3 Model Building ----
#3a. Build final model using experience, theory, and signigicance to guide variable selection -----
#Full Graduation Outcome Model
Graduation_Model_Full <- glm(formula = GraduatedStatus ~ StuTermI + TermCredits + TermGPA + Woman + AgeAtBegin + CoachingParticipant + EmployerSponsored + NeedBasedScholarship + Working,
                           family = binomial(link = "cloglog"),
                           data = Masters_TTD_Train)
summary(Graduation_Model_Full)

#Full Withdrawal Outcome Model
Withdrawal_Model_Full <- glm(formula = WithdrawnStatus ~ With_Time_Spline1_slope + With_Time_Spline2_slope + TermCredits + TermGPA + Woman + AgeAtBegin + CoachingParticipant + EmployerSponsored + NeedBasedScholarship + Working,
                             family = binomial(link = "cloglog"),
                             data = Masters_TTD_Train)
summary(Withdrawal_Model_Full)


#Remove variables or add interactions one at a time until you achieve your final model. Use theory AND personal experience along with statistical 
    #significance to guide modifications
Graduation_Model_Final <- glm(formula = GraduatedStatus ~ StuTermI + TermCredits + TermGPA + EmployerSponsored ,
                             family = binomial(link = "cloglog"),
                             data = Masters_TTD_Train)
summary(Graduation_Model_Final)

Withdrawal_Model_Final <- glm(formula = WithdrawnStatus ~ With_Time_Spline1_slope + With_Time_Spline2_slope + TermCredits + TermGPA + Woman + AgeAtBegin + CoachingParticipant + EmployerSponsored + Working,
                             family = binomial(link = "cloglog"),
                             data = Masters_TTD_Train)
summary(Withdrawal_Model_Final)


#3b. Test model fit on training data -----
#Calculate McFadden's Pseudo R-squared
pR2(Graduation_Model_Final)
hoslem.test(Masters_TTD_Train$GraduatedStatus,Graduation_Model_Final$fitted.values, g=10)

pR2(Withdrawal_Model_Final)
hoslem.test(Masters_TTD_Train$WithdrawnStatus,Withdrawal_Model_Final$fitted.values, g=10)

#3c. Test model fit on testing data
#create df of predicted values and DV for assessing fit and accuracy
#Graduation outcome
Grad_test_fit_us <- data.frame (PRED  = predict(Graduation_Model_Final, newdata = Masters_TTD_Test, type="response"),
                           DV = Masters_TTD_Test$GraduatedStatus
)

Grad_test_fit <-Grad_test_fit_us[order(Grad_test_fit_us$PRED),]
# create roc curve and calculate area under the curve (AUC)
roc(Grad_test_fit$DV,Grad_test_fit$PRED,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main="ROC Curve for Graduation Outcome")

#Withdrawal outcome
With_test_fit_us <- data.frame (PRED  = predict(Withdrawal_Model_Final, newdata = Masters_TTD_Test, type="response"),
                                DV = Masters_TTD_Test$WithdrawnStatus
)
With_test_fit <-With_test_fit_us[order(With_test_fit_us$PRED),]
# create roc curve and calculate area under the curve (AUC)
roc(With_test_fit$DV,With_test_fit$PRED,
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE, main="ROC Curve for Withdrawal Outcome")
