rm(list = ls())
setwd("C:\\Users\\chris\\Box Sync\\COURSEWORK\\MIDS\\IDS 702 Modelling\\Final Project")

# Load packages:
library(lme4)
library(lattice)
library(ggplot2)
library(rms)
library(dplyr)
library(stargazer)

# Set up the dataset:
d <- read.csv("facebook_ad_data.csv", header=TRUE, stringsAsFactors = TRUE)
names(d)[names(d) == "ï..ad_count"] <- "ad_count"
d$year <- factor(d$year)
d$special <- ifelse(d$special == 1, 1, 0)
d$special <- factor(d$special)
d$party <- factor(d$party, levels = c("Democratic","Republican","Libertarian"))
d$party2 <- factor(d$party, levels = c("Republican","Democratic","Libertarian"))
d$rating <- factor(d$rating, levels = c("Solid Democratic","Solid Republican","Likely Democratic","Likely Republican","Lean Democratic","Lean Republican","Tossup"))
d$incumbent <- factor(d$incumbent, levels = c("Incumbent","Challenger","Open"))
d$rating_np <- factor(d$rating_np, levels = c("Safe", "Lean","Tossup"))
d$outcome <- factor(d$outcome, levels = c("Lost","Won","Runoff"))
d$region <- factor(d$region, levels = c("Northeast","Midwest","Southeast","Southwest", "West"))



### EXPLORATORY DATA ANALYSIS ###

## Response variable:

# Check the response variable for normal distribution:
ggplot(d,aes(x=dollar_spent)) + 
  geom_histogram(aes(y=..count..), binwidth=300000) + 
  labs(title="Distribution of Candidate Spending on Facebook Ads", 
       x="Spending on Facebook Ads") + theme(text = element_text(size = 14))

# Transform the response variable into the log:
d$dollar_log <- log(d$dollar_spent)

# Check the normal distribution again:
ggplot(d,aes(x=dollar_log)) + 
  geom_histogram(aes(y=..count..), binwidth=0.3) + 
  labs(title="Distribution of Candidate Spending on Facebook Ads", 
       x="Log of Spending on Facebook Ads") + theme(text = element_text(size = 14))


## Predictor variables: 

# Amount Spent vs Competitiveness
ggplot(d,aes(x=rating_np, y=dollar_log, fill=rating_np)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Pastel1") +
  labs(title="$ Amount Spent vs Competitiveness",
       x="Competitiveness Rating",
       y="Log($ Amount Spent on Facebook Ads)") + 
  theme(legend.position="none") + theme(text = element_text(size = 14))

# Amount Spent vs Incumbency
ggplot(d,aes(x=incumbent, y=dollar_log, fill=incumbent)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Set3") +
  labs(title="$ Amount Spent vs Incumbency Status",
       x="Incumbency Status",
       y="Log($ Amount Spent on Facebook Ads)") + 
  theme(legend.position="none") + theme(text = element_text(size = 14)) 

# Amount Spent vs Party
ggplot(d,aes(x=party2, y=dollar_log, fill=party2)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Set1") +
  labs(title="$ Amount Spent vs Political Party",
       x="Political Party",
       y="Log($ Amount Spent on Facebook Ads)") + 
  theme(legend.position="none") + theme(text = element_text(size = 14))

# Amount Spent vs Year
ggplot(d,aes(x=year, y=dollar_log, fill=year)) +
  geom_boxplot() + #coord_flip() +
  labs(title="$ Amount Spent vs Year",
       x="Year",
       y="Log($ Amount Spent on Facebook Ads)") + 
  theme(legend.position="none") + theme(text = element_text(size = 14)) +
  scale_fill_brewer(palette="Dark2")

# Amount Spent vs Special Election
ggplot(d,aes(x=special, y=dollar_log, color=special)) +
  geom_boxplot() + #coord_flip() +
  labs(title="$ Amount Spent vs Special Election",
       x="Special Election",
       y="Log($ Amount Spent on Facebook Ads)") + 
  theme(legend.position="none") + theme(text = element_text(size = 14)) +
  scale_fill_brewer(palette="Dark2")

# Amount Spent vs Election Outcome
ggplot(d,aes(x=outcome, y=dollar_log, color=outcome)) +
  geom_boxplot() + #coord_flip() +
  labs(title="$ Amount Spent vs Election Outcome",
       x="Election Outcome",
       y="Log($ Amount Spent on Facebook Ads)") + 
  theme(legend.position="none") + theme(text = element_text(size = 14))

# Amount Spent vs Region
ggplot(d,aes(x=region, y=dollar_log, fill=region)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Dark2") +
  labs(title="$ Amount Spent vs US Region",
       x="Region",
       y="Log($ Amount Spent on Facebook Ads)") + 
  theme(legend.position="none") + theme(text = element_text(size = 14))

## Interactions 

# vs Incumbency

ggplot(d, aes(x=incumbent, y=dollar_log, fill=incumbent)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Accent") +
  labs(title="Incumbency Status x Seat Competitiveness",
       x="Incumbency Status",y="Log($ Amount Spent on Facebook Ads)") +
  #theme_classic() + 
  theme(legend.position="none") +
  facet_wrap( ~ rating_np,ncol=4) + theme(text = element_text(size = 13)) -> bp5

ggplot(d, aes(x=incumbent, y=dollar_log, fill=incumbent)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Accent") +
  labs(title="Incumbency Status x Political Party",
       x="Incumbency Status",y="Log($ Amount Spent on Facebook Ads)") + 
  #theme_classic() + 
  theme(legend.position="none") +
  facet_wrap( ~ party,ncol=4) + theme(text = element_text(size = 13)) -> bp6

ggplot(d, aes(x=incumbent, y=dollar_log, fill=incumbent)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Set3") +
  labs(title="",
       x="",y="Log($ Amount Spent on Facebook Ads)") +
  #theme_classic() + 
  theme(legend.position="none") +
  facet_wrap( ~ year,ncol=4)

ggplot(d, aes(x=incumbent, y=dollar_log, fill=incumbent)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Set3") +
  labs(title="",
       x="Incumbency Status",y="Log($ Amount Spent on Facebook Ads)") +
  #theme_classic() + 
  theme(legend.position="none") +
  facet_wrap( ~ outcome,ncol=4)

ggplot(d, aes(x=incumbent, y=dollar_log, fill=incumbent)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Set3") +
  labs(title="",
       x="",y="Log($ Amount Spent on Facebook Ads)") +
  #theme_classic() + 
  theme(legend.position="none") +
  facet_wrap( ~ special,ncol=4)

# vs Competitiveness

ggplot(d, aes(x=rating_np, y=dollar_log, fill=rating_np)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Pastel1") +
  labs(title="",
       x="",y="Log($ Amount Spent on Facebook Ads)") +
  #theme_classic() + 
  theme(legend.position="none") +
  facet_wrap( ~ party,ncol=4)

ggplot(d, aes(x=rating_np, y=dollar_log, fill=rating_np)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Pastel1") +
  labs(title="",
       x="",y="Log($ Amount Spent on Facebook Ads)") +
  #theme_classic() + 
  theme(legend.position="none") +
  facet_wrap( ~ year,ncol=4)

ggplot(d, aes(x=rating_np, y=dollar_log, fill=rating_np)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Pastel1") +
  labs(title="",
       x="",y="Log($ Amount Spent on Facebook Ads)") +
  #theme_classic() + 
  theme(legend.position="none") +
  facet_wrap( ~ outcome,ncol=4)

# vs Party

ggplot(d, aes(x=party, y=dollar_log, fill=party)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Pastel1") +
  labs(title="",
       x="",y="Log($ Amount Spent on Facebook Ads)") +
  #theme_classic() + 
  theme(legend.position="none") +
  facet_wrap( ~ outcome,ncol=4)

# vs Year
ggplot(d, aes(x=year, y=dollar_log, fill=year)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Pastel1") +
  labs(title="",
       x="",y="Log($ Amount Spent on Facebook Ads)") +
  #theme_classic() + 
  theme(legend.position="none") +
  facet_wrap( ~ special,ncol=4)


### MODELLING ###

#Initial model
reg1 <- lm(dollar_log ~ party + incumbent + rating_np + year + special + outcome + region, data=d)
summary(reg1)

#Model with interactions
reg2 <- lm(dollar_log ~ party + incumbent + rating_np + year + special + outcome + region + party*incumbent + incumbent*rating_np, data=d)
summary(reg2)

#F-test
anova(reg1, reg2)

#AIC
AIC(reg1)
AIC(reg2)

#Stepwise selection
NullModel <- lm(dollar_log ~ party + incumbent + rating_np, data=d)
FullModel <- lm(dollar_log ~ year + party + special + incumbent + rating_np + outcome + region + party*incumbent + incumbent*rating_np + incumbent*outcome, data=d)
reg_AIC <- step(NullModel, scope = formula(FullModel),direction="both",trace=0)
reg_AIC$call
summary(reg_AIC)

#Final Model
regFinal <- lm(dollar_log ~ party2 + incumbent + rating_np + year, data = d)


## Model Assessment

#multicollinearity
vif(regFinal)

#independence, equal variance, normality
plot(regFinal,which=1,col=c("blue4"))
plot(regFinal,which=2,col=c("blue4"))

#leverage scores
n <- nrow(model.matrix(regFinal)); p <- ncol(model.matrix(regFinal))
lev_scores <- hatvalues(regFinal) 
plot(lev_scores,col=ifelse(lev_scores > (2*p/n), 'red2', 'navy'),type="h",
     ylab="Leverage score",xlab="Index",main="Leverage Scores for all observations")
text(x=c(1:n)[lev_scores > (2*p/n)]+c(rep(2,4),-2,2),y=lev_scores[lev_scores > (2*p/n)],
     labels=c(1:n)[lev_scores > (2*p/n)])

#Cook's distance
plot(regFinal,which=4,col=c("blue4"))
plot(regFinal,which=5,col=c("blue4"))


## Table output: 

stargazer(regFinal,
          type = "latex",
          header = FALSE,
          title = "Multiple Linear Regression Model for Predicting Facebook Ad Spending by US Senate Candidates",
          dep.var.caption  = "Dollar Amount Spent on Facebook Ads",
          digits = 2,
          ci = TRUE, # display 95% CIs instead of standard error
          covariate.labels = c("Democratic", "Challenger", "Open Seat",
                               "`Lean' Race", "`Tossup' Race",
                               "Year"), # provide nicer-looking predictor labels
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))
