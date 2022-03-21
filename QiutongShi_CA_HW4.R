library(tableone) 
library(MatchIt)
library(lattice)

setwd("~/Desktop/luan/Customer Analytics/Week 5 matching analysis/HW4")# set working directory
rm(list = ls()) # clear workspace 
ds <- read.csv("D5.2 Mashable.csv",stringsAsFactor=TRUE)  # load data 
ds$if_videos <- ifelse(ds$num_videos>0,1,0)
summary(ds) 


#simple linear regression of shares and treatment
slr = lm(shares~if_videos, data=ds)
summary(slr)

drop = c("ur","x2")
ds = ds[,!(names(ds) %in% drop)]

# Checking overlap
lm1 <- glm(if_videos~., family = binomial, data = subset(ds,select=-c(num_videos,url,timedelta,shares)))
summary(lm1)
ds$pscore = predict(lm1,type="response") # generate pscore for each oberservation in the entire dataset
hist(ds$pscore)
#genearte a side by side histogram for control and treatment group
histogram(~ pscore|if_videos, data = ds) # Note: this is a lattice package command, not the same as "hist"
#take a look at whether if one group does not have enough mass


# Perform matching with pre-focus on matching
matched <- matchit(if_videos~.-url-num_videos-timedelta-shares-pscore,method = "nearest",
                   data = ds[ds$pscore<=0.8,],distance='glm')
matched 

# Create matched data set 
ds_matched = match.data(matched)#convert matched object to macthed data set
dim(ds_matched)
table(ds_matched$if_videos)

#create a new dataset from ds_matched only for assessment of balance

# Post-matching assessment of balance  
print(CreateTableOne(vars = xvars, data = ds_matched, strata = "if_videos", smd = TRUE))

# Estimate ATE 
summary(lm(shares ~ if_videos, data = ds_matched))  
summary(lm1)


