######################################################################
##                                                                  ## 
##      PROJECT TITLE:    CESS/ESSEX SUMMER SCHOOL                  ##
##                        ML WORKSHOP                               ##
##                                                                  ## 
##      CODE AUTHORS:     THOMAS S. ROBINSON (JUNE 2019)            ##
##                        DENISE LAROZE (FEB 2017)                  ## 
##                                                                  ## 
##      EMAIL:            thomas.robinson@politics.ox.ac.uk         ##
##                                                                  ##
##      DESCRIPTION:      DATA SET UP, MODELS AND ENSEMBLE METHODS  ##
##                                                                  ##
######################################################################

#### 0. Pre-requisites ####

library(ggpubr)
library(plyr)
library(FindIt)
library(BayesTree)
library(dplyr)
library(wesanderson)


colours <- wes_palette("Royal1")
colours2 <- wes_palette(name = "Royal1", type = "continuous", n=21)

#### 1. Functions ####

## Risk consistency function
consist.risk<-function(df,name.risk){
  col_idx <- grep(name.risk, names(df))
  m1 <- as.matrix(df[,col_idx])
  m1<-as.data.frame(m1)
  m1$Consistent<-NA
  m1$risk.pref.consist<-NA
  for(i in 1:nrow(m1)){
    a<-m1[i,]
    vt<-rep(NA, ncol(a) )
    for(j in 2:ncol(a)){
      vt[j-1]<-ifelse(a[j-1]>a[j], "inconsistent", "OK")
      df$Consistent[i]<-ifelse("inconsistent" %in% vt, "No" ,  "Yes") 
      df$risk.pref.consist[i]<-ifelse("inconsistent" %in% vt,  NA, df$risk.pref[i])
    }
  }
  return(df)
}

## Normalise distribution - used in ability calculations
redist.fun <- function(x){(x-min(x, na.rm = T))/diff(range(x, na.rm = T))}

## Calculate normalized ability ranking
abilityCalc <- function(df) {
  
  # Recover mean RET performance per participant
  df$ability <- as.numeric(lapply(df$muID, function(x) mean(df[df$muID == x,]$ncorrectret, na.rm=T)))
  abilityDF <- group_by(df,muID)
  newDF <- summarise(abilityDF,
                     ability = mean(ability))
  
  # Rank ability scores
  newDF$rank <- rank(newDF$ability, na.last = "keep")
  df$rank <- as.numeric(lapply(df$muID, function(x) mean(newDF[newDF$muID == x,]$rank, na.rm=T)))
  
  # Normalize ranking
  df$rank <- redist.fun(df$rank)
  
  # Format gender
  df$Gender <- ifelse(df$Gender == "F",1,0)
  return(df)
}

#### 2. Data Management ####

## NB: This code block must be run prior to analysis.

## Load files

mturk.ds<-read.csv("data/Mturk_DS_Sept2017.csv") #
cess.online.panel <- read.csv("data/CESS_Panel_DS_Feb2018.csv")
cess.online.panel<-cess.online.panel[cess.online.panel$correct>0, ]
lab.online.sync <- read.csv("data/lab_online_sync_edited.csv")
baseline.uk<-read.csv("data/baseline_uk.csv")
baseline.uk<-baseline.uk[baseline.uk$auditrate<30, ] # Only sessions with 0 and 20% audit

## Data cleaning

#Cleaning up round number in lab version of the experiment
baseline.uk$round<-ifelse(baseline.uk$auditrate>0, baseline.uk$period+10, baseline.uk$period )

# Eliminating missing values
lab.online.sync$DictGive[lab.online.sync$DictGive==-1]<-NA

# Editing Age from logical to numeric
mturk.ds$age2<-mturk.ds$age
mturk.ds$age2[mturk.ds$age=="false"]<-NA
mturk.ds$Age<-as.numeric(levels(mturk.ds$age2))[mturk.ds$age2]



# Indentifying and Eliminating non-consistent risk preferences
# Eliminating observations null risk and integrity ofvservations 
# caused by the way the online experiment is coded
cess.online.panel$risk.pref[cess.online.panel$risk.pref==0]<-NA
mturk.ds$risk.pref[mturk.ds$risk.pref==0]<-NA
lab.online.sync$risk.pref[lab.online.sync$risk.pref==0]<-NA

cess.online.panel$total.integrity[cess.online.panel$total.integrity==0]<-NA
mturk.ds$total.integrity[mturk.ds$total.integrity==0]<-NA
lab.online.sync$total.integrity[lab.online.sync$total.integrity==0]<-NA

#online.uk<-consist.risk(online.uk, "Risk_")
lab.online.sync<-consist.risk(lab.online.sync, "risk")
mturk.ds<-consist.risk(mturk.ds, "risk")
cess.online.panel<-consist.risk(cess.online.panel, "risk")


#rename variables
names(baseline.uk)[names(baseline.uk) == 'dec1'] <- 'Risk_1'
names(baseline.uk)[names(baseline.uk) == 'dec2'] <- 'Risk_2'
names(baseline.uk)[names(baseline.uk) == 'dec3'] <- 'Risk_3'
names(baseline.uk)[names(baseline.uk) == 'dec4'] <- 'Risk_4'
names(baseline.uk)[names(baseline.uk) == 'dec5'] <- 'Risk_5'
names(baseline.uk)[names(baseline.uk) == 'dec6'] <- 'Risk_6'
names(baseline.uk)[names(baseline.uk) == 'dec7'] <- 'Risk_7'
names(baseline.uk)[names(baseline.uk) == 'dec8'] <- 'Risk_8'
names(baseline.uk)[names(baseline.uk) == 'dec9'] <- 'Risk_9'
names(baseline.uk)[names(baseline.uk) == 'dec10'] <- 'Risk_10'

baseline.uk$risk.pref<-10-baseline.uk$safechoices
baseline.uk<-consist.risk(baseline.uk, "Risk_")  

cess.online.panel$risk.pref[cess.online.panel$risk.pref==0]<-NA
mturk.ds$risk.pref[mturk.ds$risk.pref==0]<-NA
lab.online.sync$risk.pref[lab.online.sync$risk.pref==0]<-NA

cess.online.panel$total.integrity[cess.online.panel$total.integrity==0]<-NA
mturk.ds$total.integrity[mturk.ds$total.integrity==0]<-NA
lab.online.sync$total.integrity[lab.online.sync$total.integrity==0]<-NA

## change the unit of variables
mturk.ds$DictGive.normal <- mturk.ds$DictGive/1000
mturk.ds$total.integrity.normal <- (mturk.ds$total.integrity)/40
mturk.ds$risk.pref.normal <- (mturk.ds$risk.pref.consist)/10

lab.online.sync$DictGive.normal <- lab.online.sync$DictGive/1000
lab.online.sync$total.integrity.normal <- (lab.online.sync$total.integrity)/40
lab.online.sync$risk.pref.normal <- (lab.online.sync$risk.pref.consist)/10

cess.online.panel$DictGive.normal <- cess.online.panel$DictGive/1000
cess.online.panel$total.integrity.normal <- (cess.online.panel$total.integrity)/40
cess.online.panel$risk.pref.normal <- (cess.online.panel$risk.pref.consist)/10 


### adapting Baseline uk data
m1 <- as.matrix(baseline.uk[, c("publictransport","taxes", "drivingfast", "moneyfound",                 
                                "lying", "accidentaldamage", "litter",                     
                                "drivingalcohol", "jobapplication", "buyingstolen")])
class(m1)<-"numeric"

baseline.uk$total.integrity<-rowSums(m1, na.rm = T)

baseline.uk$DictGive.normal <- baseline.uk$offerdg/1000   
baseline.uk$risk.pref.normal <- baseline.uk$risk.pref.consist/10
baseline.uk$total.integrity.normal <- (baseline.uk$total.integrity)/40
baseline.uk$report.rate <- baseline.uk$declared/baseline.uk$profitret

baseline.uk$treat<-NA
baseline.uk$treat[baseline.uk$auditrate==0]<-2
baseline.uk$treat[baseline.uk$auditrate==20]<-1

baseline.uk$Gender_lab[baseline.uk$gender==0]<-"F"
baseline.uk$Gender_lab[baseline.uk$gender==1]<-"M"

### Number of correct responses
lab.online.sync$ncorrectret <- lab.online.sync$correct
mturk.ds$ncorrectret<-mturk.ds$correct
cess.online.panel$ncorrectret<-cess.online.panel$correct

## Figures - Prep
baseline.uk$muID<-paste0("baseline", baseline.uk$subj_id)
names(baseline.uk)[names(baseline.uk)=="age_subject"] <- "Age"
names(baseline.uk)[names(baseline.uk)=="safechoices"] <- "risk.pref"
names(baseline.uk)[names(baseline.uk)=="profitret"] <- "prelimGain"
names(baseline.uk)[names(baseline.uk)=="offerdg"] <- "DictGive"
names(baseline.uk)[names(baseline.uk)=="Gender_lab"] <- "Gender"
names(baseline.uk)[names(baseline.uk)=="auditrate"] <- "auditRate"

names(lab.online.sync)[names(lab.online.sync)=="age"] <- "Age"
names(lab.online.sync)[names(lab.online.sync)=="gender"] <- "Gender"
names(lab.online.sync)[names(lab.online.sync)=="taxRate"] <- "taxrate"

names(mturk.ds)[names(mturk.ds)=="gender"] <- "Gender"
names(mturk.ds)[names(mturk.ds)=="taxRate"] <- "taxrate"

names(cess.online.panel)[names(cess.online.panel)=="age"] <- "Age"
names(cess.online.panel)[names(cess.online.panel)=="gender"] <- "Gender"
names(cess.online.panel)[names(cess.online.panel)=="taxRate"] <- "taxrate"

vars<-c( "muID", "ncorrectret" ,"Gender", "Age", "DictGive" ,"DictGive.normal", "total.integrity", "total.integrity.normal", 
         "risk.pref.normal", "risk.pref", "prelimGain", "report.rate", "treat", "taxrate", "round","auditRate"
)

# Gen. consistent set of dataframes
o.sync<-lab.online.sync[, vars]
o.sync$sample<-"Online Lab"
b.s<-baseline.uk[, vars]
b.s$sample<-"Lab"
mt.s<-mturk.ds[, vars]
mt.s$sample<-"Mturk"

cp.s<-cess.online.panel[, vars]
cp.s$sample<-"CESS Online UK"

p.data<-rbind(o.sync, b.s, mt.s, cp.s)

# Assign unique subject IDs
p.data$muID<-paste0(p.data$sample, p.data$muID)

# Remove temporary dataframes
rm(o.sync, b.s, mt.s, cp.s)

# Data set up including calculating ability rank
df <- p.data
df$treat.het <- ifelse(is.na(df$taxrate),NA,ifelse(df$taxrate > 10,1,0))
df$Gender <- ifelse(df$Gender == "F",1,0)
df$sample <- as.factor(df$sample)

ability <- df %>% group_by(muID) %>%
  summarise(ind_ability = mean(ncorrectret, na.rm=T)) %>%
  mutate(ability = rank(ind_ability, na.last = "keep")) %>%
  mutate(ability = redist.fun(ability)) %>%
  select(-ind_ability)

df <- left_join(df, ability, by = "muID")

rm(ability, baseline.uk, cess.online.panel, lab.online.sync, m1, mturk.ds)

#### 3. Replicate Figure 2 Duch et al. paper (BART model only) ####

library(BayesTree)

set.seed(89)

# Data set up including calculating ability rank
df <- p.data
df$treat.het <- ifelse(is.na(df$taxrate),NA,ifelse(df$taxrate > 10,1,0))
df$Gender <- ifelse(df$Gender == "F",1,0)
df$sample <- as.factor(df$sample)

ability <- df %>% dplyr::group_by(muID) %>%
  summarise(ind_ability = mean(ncorrectret, na.rm=T)) %>%
  mutate(ability = rank(ind_ability, na.last = "keep")) %>%
  mutate(ability = redist.fun(ability)) %>%
  select(-ind_ability)

df <- left_join(df, ability, by = "muID")

# Define model variables incl. outcome as column 1
vars <- c("report.rate","treat.het","ability","sample","Age","Gender")

df <- df[,vars]
df <- df[complete.cases(df),]

# Separate outcome and training data
y <- df$report.rate
train <- df[,-1]

# Gen. test data where those treated become untreated, for use in calculating ITT
test <- train
test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates and format into dataframe
# Logic: take those where treatment is true in training data and deduct corresponding value in test
#        and vice versa for test dataset
CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
          bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])

CATE_df <- data.frame(CATE = CATE)

# Add back in covariate values, making sure to take correct ordering
covars <- rbind(train[train$treat.het == 1,c(2:5)], test[test$treat.het==1,c(2:5)])
CATE_df <- cbind(CATE_df,covars)

# Now we can order variables (this is just so we can show the results in a nice visual way)
CATE_df <- CATE_df[order(CATE_df$CATE),]
CATE_df$id <- c(1:length(CATE))

# Descriptive results reported in main text:
mean(CATE_df$CATE)
summary(CATE_df$CATE)

# Proportion of CATEs that are negative:
sum(CATE_df$CATE < 0)/nrow(CATE_df)
sum(CATE_df$CATE < -0.07)/nrow(CATE_df)

# Lab participant: prop. below -0.07
sum(CATE_df$CATE < -0.07 & CATE_df$sample %in% c("Lab", "Online Lab"))/sum(CATE_df$sample %in% c("Lab", "Online Lab"))

# Online participants: prop. above -0.07 and prop. above 0
sum(CATE_df$CATE > -0.07 & CATE_df$sample %in% c("CESS Online UK", "Mturk"))/sum(CATE_df$sample %in% c("CESS Online UK", "Mturk"))
sum(CATE_df$CATE > 0 & CATE_df$sample %in% c("CESS Online UK", "Mturk"))/sum(CATE_df$sample %in% c("CESS Online UK", "Mturk"))

# CATE Heterogeneity plot
hist <- CATE_df

effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$CATE), color = "blue") +
  labs(x="Individual",y = "CATE") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,5220))

# Mode histogram 
modePlot <- ggplot(hist, aes(x=id, fill=sample)) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  scale_fill_manual(name="Mode", values=colours) +
  labs(y = "Count", x = "Individual") +
  scale_x_continuous(limits = c(0,5220))

# Combine all plots into one chart
figure_2 <- ggarrange(effectsPlot, modePlot,
                      ncol = 1, nrow = 2, heights = c(2,2))

plot(figure_2)

#### 4. ML Estimations #### 

## BART [already run  above] ##

# Let's use previous BART estimate, and get predicted values of observed and synthetic observations

pred_bart <- bart.out$yhat.train.mean
pred_bart_synth <- bart.out$yhat.test.mean

# BART MSE:
mean((y - bart.out$yhat.train.mean)^2)

## Random Forest ##
library(randomForest)

set.seed(89)
model_rf <- randomForest(y ~ train$treat.het + train$ability + train$sample + train$Age + train$Gender,ntree = 200)

pred_rf <- as.vector(model_rf$predicted)
pred_rf_synth <- as.vector(predict(model_rf, test))

# Random forest MSE:
mean((y -  pred_rf)^2)

## LASSO ##
library(glmnet)
 
set.seed(89)

## Create numeric matrix equivalent, glmnet LASSO cannot handle factor variables:
matrix_train <- train

# Create sample dummy variables
matrix_train$onlinelab <- ifelse(is.na(matrix_train$sample),NA,
                                 ifelse(matrix_train$sample == "Online Lab",1,0))
matrix_train$mturk <- ifelse(is.na(matrix_train$sample),NA,
                             ifelse(matrix_train$sample == "Mturk",1,0))
matrix_train$cess <- ifelse(is.na(matrix_train$sample),NA,
                            ifelse(matrix_train$sample == "CESS Online UK",1,0))
matrix_train$sample <- NULL

matrix_test <- matrix_train
matrix_test$treat.het <- ifelse(matrix_test$treat.het == 1,0,
                                ifelse(matrix_test$treat.het == 0,1,NA))

# Convert to model matrices for LASSO estimation
matrix_train <- model.matrix(~., matrix_train)
matrix_test <- model.matrix(~., matrix_test)

# Generate sequence of lambda values
grid <- 10^seq(10, -2, length = 100)

# Basic lasso-model fitted over lambda sequence
lasso_mod = glmnet(matrix_train, 
                   y, 
                   alpha = 1, 
                   lambda = grid)

# Predict outcomes based on lasso_model
pred_lasso <- predict(lasso_mod, newx = matrix_train)
pred_lasso_synth <- predict(lasso_mod, newx = matrix_test)

# LASSO MSE:
mean((pred_lasso - y)^2)

#### 5. Basic ensemble methods procedure ####

# Level 1  data
# Create new dataset where each row is observation, and predicted outcomes for each of 3 ML strategies
ensemble <- data.frame(y,
                       bart = pred_bart, 
                       rf = pred_rf, 
                       lasso = pred_lasso[,1])

# Counterfactual using synthetic predictions
ensemble_test <- data.frame(bart = pred_bart_synth, 
                            rf = pred_rf_synth, 
                            lasso = pred_lasso_synth[,1])

# Meta-algorithm - estimate Y based on ML predictions
model_ensemble <- glm(y ~ bart + rf + lasso, data = ensemble)

# Recover newly predicted outcomes
ensemble_pred <- as.vector(predict(model_ensemble))
ensemble_pred_synth <- as.vector(predict(model_ensemble, newdata = ensemble_test))

# Ensemble MSE:
mean((y - ensemble_pred)^2)

#### 6. Estimate CATEs and plot results  ####

# Calculate CATE for each observation:
CATE_ensemble <- c(ensemble_pred[train$treat.het == 1] - ensemble_pred_synth[train$treat.het == 1],
                   ensemble_pred_synth[train$treat.het == 0] - ensemble_pred[train$treat.het == 0])

# Convert to a dataframe for plotting using ggplot
CATE_df <- data.frame(CATE = CATE_ensemble)

# Add back in covariate information for plot
covars <- rbind(train[train$treat.het == 1,c(2:5)], test[test$treat.het==1,c(2:5)])
CATE_df <- cbind(CATE_df,covars)
CATE_df <- CATE_df[order(CATE_df$CATE),]
CATE_df$id <- c(1:length(CATE_ensemble)) # ID variable so we can plot along x axis

# Mode reduced plot with deduction rate as treatment
hist <- CATE_df

# CATE Heterogeneity plot

effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$CATE), color = "blue") +
  labs(x="Individual",y = "CATE") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,5220))

# Mode histogram 

modePlot <- ggplot(hist, aes(x=id, fill=sample)) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  scale_fill_manual(name="Mode", values=colours) +
  labs(y = "Count", x = "Individual") +
  scale_x_continuous(limits = c(0,5220))

# Combine all plots into one chart
figure <- ggarrange(effectsPlot, modePlot,
                    ncol = 1, nrow = 2, heights = c(2,2))

plot(figure)

