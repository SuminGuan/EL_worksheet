#============================================================================
#============================================================================
# Adapt this script to your needs to analyze variationist datasets
# My PhD students and postdocs tell me I won't win any programming elegance
# awards with this script -- but I like it because the code is fairly
# transparent.
#============================================================================
#============================================================================

#============================================================================
# Section 1: Packages etc. to load before we get going
# note: you need to install these packages before you 
# can use them (RStudio: Tools > install packages)
#============================================================================

library(readxl)      # to load Excel files
library(car)         # to calculate VIFs
library(Hmisc)       # to calculate C values
library(party)       # for ctrees and CRF
library(gmodels)     # for crosstables
library(lme4)        # for mixed-effects regression
library(MuMIn)       # for PSeudo R2 measures
library(effects)     # for partial effects plot
library(report)      # for description of various objects
library(parameters)  # to examine model effects
library(performance) # to assess and compare model performance
# you will also need to install the "see", "patchwork" and "merDeriv" packages

#============================================================================
# Section 2: load the dataset into R
#============================================================================

# So the datset we will be analyzing during the workshop is in .csv (comma spearated values spreadhseet) format.
# Excel and other spreadsheet applications can generate this format without problems  (-> "save as").
# There are two ways to open .csv files in R (in both cases, R-internally the name of the dataset is specified as 'gen'):

# ====== via a dialogue box ====== 
myFile <- file.choose()                
gen <- read.csv(myFile, 
                header=TRUE, # this line specifies that the first row contains the column labels
                sep=",",     # this line specifies that the column separator is a comma (attention: on csv generated on continental European computers, it's often a semicolon!)
                dec=".")     # this line specifies that the decimal separator is a dot

# ====== via specifying the path/location directly in R ====== 
gen <- read.csv("genitives-finalized.csv", 
                header=TRUE, # this line specifies that the first row contains the column labels
                sep=",",     # this line specifies that the column separator is a comma (attention: on csv generated on continental European computers, it's often a semicolon!)
                dec=".")     # this line specifies that the decimal separator is a dot

# it is also possible to read in .xls(x) files (Excel-format directly) via the readxl package -- check out https://readxl.tidyverse.org/


#====================================================================================================
# Section 3: Some housekeeping -- drop useless columns and rows, tell R which factors are categeorical
#====================================================================================================

# drop stuff -- this is fairly specific to the dataset we analyze here
gen <- droplevels(subset(gen, Inclusion == "y")) # restrict attention to subset
gen <- droplevels(subset(gen, Speaker.sex == "F" | Speaker.sex == "M")) # drop empty speaker sex category
gen <- na.omit(gen) # omit all rows that contain NA values (missing cases, such as missing age information)
gen$Possessor.definiteness[gen$Possessor.definiteness=="definite"] <- "Definite" # recode labels
gen <- droplevels(gen) # drop unused levels

# tell R which variables/columns are numeric, and which are categorical -- has to be done when importing .csv files
gen$Response.variable = as.factor(gen$Response.variable)
gen$Variety = as.factor(gen$Variety)
gen$Speaker.sex = as.factor(gen$Speaker.sex)
gen$Speaker.age = as.numeric(gen$Speaker.age)
gen$Final.sibilancy = as.factor(gen$Final.sibilancy)
gen$Semantic.relation = as.factor(gen$Semantic.relation)
gen$Possessor.animacy = as.factor(gen$Possessor.animacy)
gen$Possessor.definiteness = as.factor(gen$Possessor.definiteness)
gen$Possessor.length = as.numeric(gen$Possessor.length)
gen$Possessum.length = as.numeric(gen$Possessum.length)
gen$Corpus = as.factor(gen$Corpus)
gen$Speaker = as.factor(gen$Speaker)
gen$Persistence = as.factor(gen$Persistence)
gen$Inclusion = as.factor(gen$Inclusion)


# how to recode variables
gen$VarietyNew <- car::recode(gen$Variety, " ## this code creates a new variable (column) 'VarietyNew' that simplifies the original variable 'Variety'
                                  #'original string'  = 'replacement'; 
                                  'CAN'  = 'NorthernHemisphere'; 
                                  'US'   = 'NorthernHemisphere'; 
                                  'UK'   = 'NorthernHemisphere'; 
                                  'NZ'   = 'SouthernHemisphere'
                                  ")

# finally, we can use the following code to retain only those columns that we want/need in the dataframe
gen = subset(gen, select = c("Response.variable",
                             "Variety",
                             "VarietyNew",
                             "Speaker.sex",
                             "Speaker.age",
                             "Final.sibilancy",
                             "Semantic.relation",
                             "Possessor.animacy",
                             "Possessor.definiteness",
                             "Possessor.length",
                             "Possessum.length") )



#====================================================================================================
# Section 4: Summarizing the dataset
#====================================================================================================

summary(gen) # shows a summary
dim(gen)     # shows overall N and number of columns


#============================================================================================
# Section 5: CrossTable -- easy descriptive (yet informative) stats for categorical variables
#============================================================================================

### the following code crosstabulates the response (s-gen versus of-gen) against speaker sex
CrossTable(
	gen$Response.variable,       # specify here what's in the rows
	gen$Speaker.sex,             # specify here what'S in the columns
	digits=1,
	expected=FALSE,
	prop.r=FALSE,
	prop.c=TRUE,                 # specfies that we want column-wise percentages
	prop.t=FALSE,
	prop.chisq = FALSE,
	chisq=FALSE,
	format="SPSS"
	)

#=====================================================================================================
# Section 6: boxPlot & mean values -- easy descriptive (yet informative) stats for numerical variables
#=====================================================================================================

### calculate mean values
aggregate(Speaker.age ~ Response.variable, gen, mean)

### box plot -- see https://en.wikipedia.org/wiki/Box_plot
boxplot(gen$Speaker.age ~ gen$Response.variable, # specify y-axis and x-axis (categories)
        data=gen
)

#============================================================================
# Section 7: conditional inference tree -- easy to interpret
#============================================================================

tree = ctree(Response.variable ~           # specificy the response variable here
               Variety +                   # specify the predictors here
               Speaker.sex +
               Speaker.age +
               Final.sibilancy +
               Semantic.relation +
               Possessor.animacy +
               Possessor.definiteness +
               Possessor.length +
               Possessum.length
             , data = gen,control=ctree_control(maxdepth=3)) # maxdepth specifies the height of the tree. Reduce to reduce complexity.
plot(tree)
#plot(tree, inner_panel = node_barplot) # with additional bars in each node

#### model C index
#### C ranges between .5 and 1; the closer to 1, the better the fit of the model
your.ctree.pred <- unlist(treeresponse(tree))[c(FALSE, TRUE)]
somers2(your.ctree.pred, as.numeric(gen$Response.variable) - 1)

#============================================================================
# Section 8: Exploratory Conditional Random Forest analysis: 
# what are the most important predictors?
#============================================================================

set.seed(123) # esnures that we can reproduce results
forest = cforest(Response.variable ~          # specificy the response variable here
                   Variety +                  # specify the predictors here
                   Speaker.sex +
                   Speaker.age +
                   Final.sibilancy +
                   Semantic.relation +
                   Possessor.animacy +
                   Possessor.definiteness +
                   Possessor.length +
                   Possessum.length
                 , data = gen)

#### calculate variable importance ranking, takes some time
forest.varimp = varimp(forest, conditional = FALSE) 

#### model C index
#### C ranges between 0.5 an 1; the closer to 1, the better the model
prob2.rf <- unlist(treeresponse(forest))[c(FALSE, TRUE)]
somerssmallcrf <- somers2(prob2.rf, as.numeric(gen$Response.variable) - 1)
somerssmallcrf["C"]

### the following code creates a dot plot visualizing the variable importance ranking
dotplot(sort(forest.varimp), xlab="Variable Importance", panel = function(x,y){
  panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1)
  panel.abline(v=abs(min(forest.varimp)), col='red',
               lty='longdash', lwd=2)
}
) 

#=============================================================================================
# Section 9: Fixed-effects logstic regression analysis using glm (will do for your term paper)
#=============================================================================================

# specify predicted outcome
gen$Response.variable <- relevel(gen$Response.variable, ref="of-genitive") # reference level is of-genitive, thus predicted odds are for the s-genitive

glmmodel = glm(Response.variable ~             # specify response variables here
                 Final.sibilancy +             # specify oredictors in what follow
                 Semantic.relation +
                 Possessor.animacy +
                 Possessor.definiteness +
                 Possessor.length +
                 Possessum.length +
                 Speaker.sex +
                 Speaker.age
               
               , family=binomial, data=gen)

summary(glmmodel) # this line summarizes the model calculated above

# the following code calculates odds ratios, which some people find
# easier to interpret than the coefficients (log odds) in the original glm output
# see https://doi.org/10.1017/S1360674307002341 for a linguistics paper that reports odds ratios
exp(coef(glmmodel))

# show fitted probabilities (just for fun)
gen$fitted <- fitted(glmmodel)
gen$fitted 

# Using the somers2() function from Hmisc on the fitted values, we obtain the C value for the model. 
# C ranges between 0.5 an 1; the closer to 1, the better the model
somers2(binomial()$linkinv(fitted(glmmodel)), as.numeric(gen$Response.variable) -1)

## VIFs to check whether the model suffers from collinearity
# Levshina (2015: 272): values > 5-10 are worrisome
vif(glmmodel)

# The proportion of correctly predicted values can be calculated 
# by cross tabulating the observed and predicted values.
fitted <- fitted(glmmodel)
predicted <- ifelse(fitted >= .5, 1,0)
a <- data.frame(gen, predicted)
CrossTable(gen$Response.variable, a$predicted)

# partial effects plot (see https://data.library.virginia.edu/visualizing-the-effects-of-logistic-regression/)
# vertical axes plot probability of the predicted outcome (s-genitive)
plot(allEffects(glmmodel))

#============================================================================
# Section 10: Mixed-effects binary logistic regression analysis using lme4
# This is advanced stuff. Normally, you won't need this for your project.
# see https://doi.org/10.1017/S0954394512000129 for a linguistics paper that 
# uses mixed-effects logistic regression
#============================================================================

# specify predicted outcome
gen$Response.variable <- relevel(gen$Response.variable, ref="of-genitive") # reference level is of-genitive, thus predicted odds are for the s-genitive

lme4model <- glmer(Response.variable ~ 
                     Final.sibilancy +
                     Semantic.relation +

                     #(1|Speaker)+
                     (1|Variety), # random effect (intercept adjustment)
                     
                     data = gen,
                     family=binomial
)

# print the estimates and p-values
print(summary(lme4model), corr = F)

# Using the somers2() function from Hmisc on the fitted values, we obtain the C value for the model. 
somers2(binomial()$linkinv(fitted(lme4model)), as.numeric(gen$Response.variable) -1)

# The proportion of correctly predicted values is calculated by cross tabulating the observed and predicted values.
fitted <- fitted(lme4model)
predicted <- ifelse(fitted >= .5, 1,0)
a <- data.frame(gen, predicted)
CrossTable(gen$Response.variable, a$predicted)

# visually depict intercept adjusments for 'Variety'
ranef(lme4model)$Variety
nms <- rownames(ranef(lme4model)$Variety)
intercepts <- ranef(lme4model)$Variety[,1]
support <- tapply(gen$Variety, gen$Variety,length)
labels <- paste(nms)
barplot(intercepts[order(intercepts)],names.arg=labels[order(intercepts)],horiz=FALSE, las=2, cex.names=0.8) 

# Pseudo R2 measures (see https://www.rdocumentation.org/packages/MuMIn/versions/1.40.4/topics/r.squaredGLMM)
# R2m: Pseudo-R2 (marginal) -- represents the % variance explained by fixed factors
# R2c: Pseudo-R2 (conditional) -- is interpreted as variance explained by both fixed and random factors (i.e. the entire model),
r.squaredGLMM(lme4model)

# partial effects plot (see https://data.library.virginia.edu/visualizing-the-effects-of-logistic-regression/)
# vertical axes plot probability of the predicted outcome
plot(allEffects(lme4model))

#============================================================================
# Section 11: Advanced stuff -- use easystats to report and 
# evaluate regression models
# For now you can ignore the warnings
#============================================================================

## ------- On the glm model -------
# description of the model results
report(glmmodel) 
# check out also report_model(glmmodel) and report_performance(glmmodel) for shorter versions

# show various plots to check multiple aspects
check_model(glmmodel) 
# Groups plots such as:
glm_collinearity <- check_collinearity(glmmodel)
glm_collinearity # prints message
plot(glm_collinearity) # plots

# plot outliers
glm_outliers <- check_outliers(glmmodel)
glm_outliers # prints message
plot(glm_outliers) # plots

# plot residuals
glm_residuals <- binned_residuals(glmmodel)
glm_residuals # prints message
plot(glm_residuals) # plots
glm_residuals

# plot log odds
class(glmmodel) <- "glm" # This is a workaround; it won't be necessary in the future
glm_params <- model_parameters(glmmodel)
glm_params # nice table with odds, confidence interval, p-value...
plot(glm_params) # plot of coefficients

# check various performance metrics
model_performance(glmmodel) 

## ------- On the glmer model ---------------
# description of the model results
report(lme4model) 
# check out also report_model(lme4model) and report_performance(lme4model) for shorter versions

# show various plots to check multiple aspects
check_model(lme4model) # show various plots to check multiple aspects
# Groups plots such as:
glm_collinearity <- check_collinearity(lme4model)
glm_collinearity # prints message
plot(glm_collinearity) # plots

# plot outliers
glm_outliers <- check_outliers(lme4model)
glm_outliers
plot(glm_outliers)

# plot residuals
glm_residuals <- binned_residuals(lme4model)
glm_residuals
plot(glm_residuals)

# plot log odds
glm_params <- model_parameters(lme4model, effects = "fixed")
glm_params # nice table with odds, confidence interval, p-value...
plot(glm_params)

# check various performance metrics
model_performance(lme4model)

## ------- Compare models ----
comparison <- compare_performance(glmmodel, lme4model, metrics = "common")
comparison # note that some metrics are different because the kinds of models are different
plot(comparison) # taking only shared metrics into account
# Unfortunately conditional trees and random forests are not supported yet :(