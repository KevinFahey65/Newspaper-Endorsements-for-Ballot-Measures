########################################
## Newspaper Endorsements R Script    ##
## Prepared by Kevin Fahey            ##
## For SPPQ Replication Materials     ##
## August 1, 2017                     ##
########################################

######################
## Clear Everything ##
######################

rm(list=ls())


#############################################
## Set Working Directory and Load Packages ##
#############################################

# set your working directory to where #
# on your computer you have put       #
# our datasets (place all together)   #

setwd("/Backup/Dropbox/Newspaper Endorsements Project")

library(foreign)
library(sandwich)
library(xtable)
library(lme4)
library(effects)
library(Matching)
library(rgenoud)
library(car)
library(cem)
library(arm)
library(lattice)
library(plm)
library(stargazer)
library(aod)
library(ggplot2)
library(compactr)
library(MASS)
library(stats)
library(xtable)
library(RColorBrewer)
library(systemfit)
library(texreg)
library(lmtest)
options(scipen=7)
options(digits=3)


##################################################
## Use Custom Clustered Standard Error Function ##
##################################################

clusterMod<-function(model, cluster)
{
  require(multiwayvcov)
  require(lmtest)
  vcovCL<-cluster.vcov(model, cluster)
  
  coef<-coeftest(model, vcovCL)
  #w<-waldtest(model, vcov = vcovCL, test = "F")
  get_confint<-function(model, vcovCL){
    t<-qt(.975, model$df.residual)
    ct<-coeftest(model, vcovCL)
    cse<-sqrt(diag(vcovCL))
    est<-cbind(ct[,1], cse,ct[,1]-t*ct[,2], ct[,1]+t*ct[,2])
    colnames(est)<-c("Estimate", "Clustered SE","LowerCI","UpperCI")
    return(est)
  }
  ci<-round(get_confint(model, vcovCL),4)
  return(list(coef, ci))
}


##################
## LOAD IN DATA ##
## Don't forget ##
## To Update    ##
## Working Dir. ##
##################

dat <- read.dta("./2017-08-01 Newspaper Endorsements Main Dataset.dta")


##############################
## Remove All pre-1994 Data ##
##############################

dat<-dat[dat$year>=1994,]


###############################
###############################
## Generate New Variables    ##
###############################
###############################

###############################
## Generate roll-off measure ##
## And Pct No votes measure  ##
###############################

rolloff<-(1 - (dat$totalvotes/dat$turnout))
pctno<-(1 - dat$pctyes)


#############################
## For time series portion ##
## generate a              ##
## dummy for pre-post 2000 ##
#############################

post2000 <- ifelse(dat$year >= 2000, 1, 0)


################################
## Create Midterm Variable    ##
################################

dat$midterm <- ifelse(dat$year == 1994 | dat$year == 1998
                      |dat$year == 2002 | dat$year == 2006
                      |dat$year == 2010 | dat$year == 2014, 1, 0)



#####################################
## Executive Endorsements Analysis ##
#####################################

############################################
## Read in executive endorsements dataset ##
############################################

execdat <- read.csv("Executive Endorsments 8-24-2016.csv")


###########################
## Merge Data Together   ##
## And ID Duplicate Vars ##
###########################

dat2 <- merge(dat, execdat, by.x = "countyid",
              by.y = "countyid",
              all.x = T) ## no duplicate vars ##


#######################################
## Create measure of like-mindedness ##
## First, identify if county         ##
## voted for winner                  ##
#######################################

dat2$countypickwinner <- ifelse((dat2$demvotes >= dat2$gopvotes & dat2$year == 1994) |
                                  (dat2$demvotes >= dat2$gopvotes & dat2$year == 1996) |
                                  (dat2$demvotes >= dat2$gopvotes & dat2$year == 1998) | 
                                  (dat2$gopvotes >= dat2$demvotes & dat2$year == 2000) | 
                                  (dat2$gopvotes >= dat2$demvotes & dat2$year == 2002) | 
                                  (dat2$gopvotes >= dat2$demvotes & dat2$year == 2004) | 
                                  (dat2$gopvotes >= dat2$demvotes & dat2$year == 2006) | 
                                  (dat2$demvotes >= dat2$gopvotes & dat2$year == 2008) | 
                                  (dat2$gopvotes >= dat2$demvotes & dat2$year == 2010) | 
                                  (dat2$demvotes >= dat2$gopvotes & dat2$year == 2012) | 
                                  (dat2$gopvotes >= dat2$demvotes & dat2$year == 2014),
                                1, 0)

dat2$likeminded <- ifelse(dat2$countypickwinner == dat2$endorsewin, 1, 0) ## If county and newspaper both supported the winner or the loser that year ##


######################
## Circulation Data ##
## Merge in to dat2 ##
######################


#################################
## Read in Circulation Dataset ##
#################################

circulation <- read.csv("Newspaper Circulation Dataset.csv")


################################################
## Create Unique Identifiers in Both Datasets ##
################################################

circulation$yearmarket <- (circulation$mediamarket * 10000) + circulation$Year
dat2$yearmarket <- (dat2$mediamarket * 10000) + dat2$year


###########################
## Merge Data Together   ##
## And ID Duplicate Vars ##
###########################

dat3 <- merge(dat2, circulation, by.x = "yearmarket", by.y = "yearmarket", all.x = T) ## two duplicates - mediamarket and newspaper name ##



###################################################
## Tampa Bay Times and Combined Salience Measure ##
## Read in and Merge TBT Data With Rest of Data  ##
###################################################

newsalience <- read.csv("2017-4-24 Salience_Measure.csv")


################################################
## Create Unique Identifiers in Both Datasets ##
################################################

newsalience$Year <- as.numeric(newsalience$Year) * 10000 ## recode year var ##
newsalience$RaceCode <- as.numeric(newsalience$RaceCode) ## recode ballot measure var ##
newsalience$countynum <- as.numeric(newsalience$countynum) * 100 ## recode county number var ## 

newsalience$uniqueid <- newsalience$Year + newsalience$countynum + newsalience$RaceCode

truncated.sal <- newsalience[,c("uniqueid", "NYT.Mention", "TBT.Mention", "Salience")] ## truncate to avoid having unnecessary duplicate vars ##


###########################
## Merge Data Together   ##
## And ID Duplicate Vars ##
###########################

dat3$uniqueid <- (dat3$year * 10000) + (dat3$countynum * 100) + dat3$racecode ## mediamarket, newspaper duplicate vars ##

dat4 <- merge(dat3, truncated.sal, by.x = "uniqueid", by.y = "uniqueid", all.x = T)


#############################################
## Codebook Discrepancy With Final Dataset ##
## vars 'midterm', 'countypickwinner',     ##
## 'likeminded', 'yearspent', and          ##
## 'newspent' not in codebook but are      ##
## created in R Script (see above)         ##
#############################################

##################
##################
#### ANALYSIS ####
##################
##################


##########################
## SPPQ Article Table 1 ##
## Table of Covariates  ##
##########################

table.1 <- matrix(NA, ncol = 4, nrow = 12)

dat.table.1 <- cbind(rolloff, pctno, 
                     dat4[,c("endorsement", "legspons", "cmsnspons",
                             "TBT.Mention", "wordcountamend",
                             "racecode", "totamends", "pctcollege",
                             "totalvotes", "Total.Circulation")])

dat.table.1$Total.Circulation <- dat.table.1$Total.Circulation/100000


for(i in 1:12){
  
  table.1[i,1] <- mean(na.omit(dat.table.1[,i]))
  table.1[i,2] <- median(na.omit(dat.table.1[,i]))
  table.1[i,3] <- sd(na.omit(dat.table.1[,i]))
  table.1[i,4] <- length(as.vector(na.omit(dat.table.1[,i])))
  
}


rownames(table.1) <- c("Rolloff", "PCtNo", "Endorsements", "LegSpons", "CmsnSpons", "Salience (TBT)", "Word Count (100s)", "Ballot Position", "Num. Amends/Yr", "Pct College", "Turnout", "Circulation")

colnames(table.1) <- c("Mean", "Median", "Standard Deviation", "Obs")

table.1


##########################
## Re-Scale Circulation ##
## To Hundred Thousands ##
##########################

dat4$Circulation.HunThou <- dat4$Total.Circulation/100000

##########################
## SPPQ Article Table 2 ##
## SUR Model Spec       ##
##########################

equation.t2 <- list(rolledoff = (rolloff ~ pctno
                                 + dat4$endorsement
                                 + dat4$TBT.Mention
                                 + dat4$legspons
                                 + dat4$cmsnspons
                                 + dat4$pctturnout
                                 + dat4$pctcollege
                                 + dat4$racecode
                                 + dat4$totamends
                                 + dat4$wordcountamend
                                 + dat4$Circulation.HunThou
                                 + dat4$midterm),
                    pctyes = (pctno ~ rolloff
                              + dat4$endorsement
                              + dat4$TBT.Mention
                              + dat4$legspons
                              + dat4$cmsnspons
                              + dat4$pctturnout
                              + dat4$pctcollege
                              + dat4$racecode
                              + dat4$totamends
                              + dat4$wordcountamend
                              + dat4$Circulation.HunThou
                              + dat4$midterm)
)

surm.t2<-systemfit(equation.t2, "SUR")
summary(surm.t2)


#####################
## Display Table 2 ##
#####################

texreg(surm.t2, digits = 3)



##################
## Table 3, SUR ##
## Three Models ##
## Rolloff 1st  ##
##################

equation1.t3 <- list(roll.off = (rolloff ~ pctno
                                 + dat4$endorsement
                                 + dat4$TBT.Mention
                                 + dat4$legspons
                                 + dat4$cmsnspons
                                 + dat4$pctturnout
                                 + dat4$pctcollege
                                 + dat4$racecode
                                 + dat4$totamends
                                 + dat4$wordcountamend
                                 + dat4$Circulation.HunThou
                                 + dat4$midterm
                                 + dat4$endorsement * dat4$TBT.Mention)
                     , pctyes = (pctno ~ rolloff
                                 + dat4$endorsement
                                 + dat4$TBT.Mention
                                 + dat4$legspons
                                 + dat4$cmsnspons
                                 + dat4$pctturnout
                                 + dat4$pctcollege
                                 + dat4$racecode
                                 + dat4$totamends
                                 + dat4$wordcountamend
                                 + dat4$Circulation.HunThou
                                 + dat4$midterm
                                 + dat4$endorsement * dat4$TBT.Mention)
)

surm.t3p1 <- systemfit(equation1.t3, "SUR")
summary(surm.t3p1)


#####

equation2.t3 <- list(roll.off = (rolloff ~ pctno
                                 + dat4$endorsement
                                 + dat4$TBT.Mention
                                 + dat4$legspons
                                 + dat4$cmsnspons
                                 + dat4$pctturnout
                                 + dat4$pctcollege
                                 + dat4$racecode
                                 + dat4$totamends
                                 + dat4$wordcountamend
                                 + dat4$Circulation.HunThou
                                 + dat4$midterm
                                 + dat4$endorsement * dat4$legspons)
                     , pctyes = (pctno ~ rolloff
                                 + dat4$endorsement
                                 + dat4$TBT.Mention
                                 + dat4$legspons
                                 + dat4$cmsnspons
                                 + dat4$pctturnout
                                 + dat4$pctcollege
                                 + dat4$racecode
                                 + dat4$totamends
                                 + dat4$wordcountamend
                                 + dat4$Circulation.HunThou
                                 + dat4$midterm
                                 + dat4$endorsement * dat4$legspons)
)

surm.t3p2 <- systemfit(equation2.t3, "SUR")
summary(surm.t3p2)


#####

equation3.t3 <- list(roll.off = (rolloff ~ pctno
                                 + dat4$endorsement
                                 + dat4$TBT.Mention
                                 + dat4$legspons
                                 + dat4$cmsnspons
                                 + dat4$pctturnout
                                 + dat4$pctcollege
                                 + dat4$racecode
                                 + dat4$totamends
                                 + dat4$wordcountamend
                                 + dat4$Circulation.HunThou
                                 + dat4$midterm
                                 + dat4$endorsement * dat4$cmsnspons)
                     , pctyes = (pctno ~ rolloff
                                 + dat4$endorsement
                                 + dat4$TBT.Mention
                                 + dat4$legspons
                                 + dat4$cmsnspons
                                 + dat4$pctturnout
                                 + dat4$pctcollege
                                 + dat4$racecode
                                 + dat4$totamends
                                 + dat4$wordcountamend
                                 + dat4$Circulation.HunThou
                                 + dat4$midterm
                                 + dat4$endorsement * dat4$cmsnspons)
)

surm.t3p3 <- systemfit(equation3.t3, "SUR")
summary(surm.t3p3)


###################
## Graph Table 3 ##
## Using Texreg  ##
###################

texreg( list(surm.t3p1, surm.t3p2, surm.t3p3), digits = 3)


#############################
## Like-Mindedness Table 4 ##
## Rolloff Models 1st, SUR ##
#############################

equation.1.t4 <- list(roll.off = (rolloff ~ pctno
                                  + dat4$endorsement
                                  + dat4$TBT.Mention
                                  + dat4$legspons
                                  + dat4$cmsnspons
                                  + dat4$pctturnout
                                  + dat4$pctcollege
                                  + dat4$racecode
                                  + dat4$totamends
                                  + dat4$wordcountamend
                                  + dat4$Circulation.HunThou
                                  + dat4$midterm
                                  + dat4$likeminded)
                      , pctyes = (pctno ~ rolloff
                                  + dat4$endorsement
                                  + dat4$TBT.Mention
                                  + dat4$legspons
                                  + dat4$cmsnspons
                                  + dat4$pctturnout
                                  + dat4$pctcollege
                                  + dat4$racecode
                                  + dat4$totamends
                                  + dat4$wordcountamend
                                  + dat4$Circulation.HunThou
                                  + dat4$midterm
                                  + dat4$likeminded)
)

surm.1.t4 <- systemfit(equation.1.t4, "SUR")
summary(surm.1.t4)


######################################
## Interact Like-Minded and Endorse ##
######################################

equation.2.t4 <- list(roll.off = (rolloff ~ pctno
                                  + dat4$endorsement
                                  + dat4$TBT.Mention
                                  + dat4$legspons
                                  + dat4$cmsnspons
                                  + dat4$pctturnout
                                  + dat4$pctcollege
                                  + dat4$racecode
                                  + dat4$totamends
                                  + dat4$wordcountamend
                                  + dat4$Circulation.HunThou
                                  + dat4$midterm
                                  + dat4$endorsement * dat4$likeminded)
                      , pctyes = (pctno ~ rolloff
                                  + dat4$endorsement
                                  + dat4$TBT.Mention
                                  + dat4$legspons
                                  + dat4$cmsnspons
                                  + dat4$pctturnout
                                  + dat4$pctcollege
                                  + dat4$racecode
                                  + dat4$totamends
                                  + dat4$wordcountamend
                                  + dat4$Circulation.HunThou
                                  + dat4$midterm
                                  + dat4$endorsement * dat4$likeminded)
)

surm.2.t4 <- systemfit(equation.2.t4, "SUR")
summary(surm.2.t4)


#############################
## Report Output in Texreg ##
#############################

texreg( list(surm.1.t4, surm.2.t4), digits = 3)




#####################
#####################
## APPENDIX MODELS ##
#####################
#####################


##################
## Table 1, OLS ##
##################

ols.r.t2 <- lm(rolloff ~ pctno + endorsement + TBT.Mention + legspons
               + cmsnspons + pctturnout + pctcollege + racecode
               + totamends + wordcountamend + Circulation.HunThou + midterm,
               data = dat4)
summary(ols.r.t2)
se.ols.r.t2 <- sqrt(diag(vcovHC(ols.r.t2)))

ols.p.t2 <- lm(pctno ~ rolloff + endorsement + TBT.Mention + legspons
               + cmsnspons + pctturnout + pctcollege + racecode
               + totamends + wordcountamend + Circulation.HunThou + midterm,
               data = dat4)
summary(ols.p.t2)
se.ols.p.t2 <- sqrt(diag(vcovHC(ols.p.t2)))


#####################
## Display Table 1 ##
## Using Stargazer ##
#####################

stargazer(ols.r.t2, ols.p.t2,
          se = list(se.ols.r.t2, se.ols.p.t2),
          no.space = T,
          omit = c("year", "countynum"),
          omit.stat = c("rsq", "ser"),
          covariate.labels = c("% No Vote", "Rolloff",
                               "Newspaper Endorsement",
                               "Salience", "Legislature Sponsor",
                               "Commission Sponsor", "% County Turnout",
                               "% College Educated", "Ballot Position",
                               "Total Amendments", "Word Counts (100s)",
                               "Circulation", "Midterm Election", "Intercept"))


##########################
## Table 2, OLS Instead ##
## Rolloff Models 1st   ##
##########################

ols.r1.t3 <- lm(rolloff ~ pctno + endorsement + TBT.Mention + legspons
                + cmsnspons + pctturnout + pctcollege + racecode
                + totamends + wordcountamend + Circulation.HunThou + midterm
                + endorsement * TBT.Mention,
                data = dat4)
summary(ols.r1.t3)
se.ols.r1.t3 <- sqrt(diag(vcovHC(ols.r1.t3)))

ols.p1.t3 <- lm(pctno ~ rolloff + endorsement + TBT.Mention + legspons
                + cmsnspons + pctturnout + pctcollege + racecode
                + totamends + wordcountamend + Circulation.HunThou + midterm
                + endorsement * TBT.Mention,
                data = dat4)
summary(ols.p1.t3)
se.ols.p1.t3 <- sqrt(diag(vcovHC(ols.p1.t3)))

#####

ols.r2.t3 <- lm(rolloff ~ pctno + endorsement + TBT.Mention + legspons
                + cmsnspons + pctturnout + pctcollege + racecode
                + totamends + wordcountamend + Circulation.HunThou + midterm
                + endorsement * legspons,
                data = dat4)
summary(ols.r2.t3)
se.ols.r2.t3 <- sqrt(diag(vcovHC(ols.r2.t3)))

ols.p2.t3 <- lm(pctno ~ rolloff + endorsement + TBT.Mention + legspons
                + cmsnspons + pctturnout + pctcollege + racecode
                + totamends + wordcountamend + Circulation.HunThou + midterm
                + endorsement * legspons,
                data = dat4)
summary(ols.p2.t3)
se.ols.p2.t3 <- sqrt(diag(vcovHC(ols.p2.t3)))

#####

ols.r3.t3 <- lm(rolloff ~ pctno + endorsement + TBT.Mention + legspons
                + cmsnspons + pctturnout + pctcollege + racecode
                + totamends + wordcountamend + Circulation.HunThou + midterm
                + endorsement * legspons,
                data = dat4)
summary(ols.r3.t3)
se.ols.r3.t3 <- sqrt(diag(vcovHC(ols.r3.t3)))

ols.p3.t3 <- lm(pctno ~ rolloff + endorsement + TBT.Mention + legspons
                + cmsnspons + pctturnout + pctcollege + racecode
                + totamends + wordcountamend + Circulation.HunThou + midterm
                + endorsement * legspons,
                data = dat4)
summary(ols.p3.t3)
se.ols.p3.t3 <- sqrt(diag(vcovHC(ols.p3.t3)))



#######################
## Graph Table 2 OLS ##
## Using Stargazer   ##
#######################

stargazer(ols.r1.t3, ols.r2.t3, ols.r3.t3, ols.p1.t3, ols.p2.t3, ols.p3.t3,
          se = list(se.ols.r1.t3, se.ols.r2.t3, se.ols.r3.t3,
                    se.ols.p1.t3, se.ols.p2.t3, se.ols.p3.t3),
          no.space = T,
          omit = c("year", "countynum"),
          omit.stat = c("rsq", "ser"),
          covariate.labels = c("% No Vote", "Rolloff",
                               "Newspaper Endorsement",
                               "Salience", "Legislature Sponsor",
                               "Commission Sponsor", "% County Turnout",
                               "% College Educated", "Ballot Position",
                               "Total Amendments", "Word Counts (100s)",
                               "Circulation", "Midterm Election",
                               "Endorse x Salience", "Endorse x Leg. Sponsor",
                               "Endorse x Cmsn. Sponsor", "Intercept")
)


##################
## Table 3, OLS ##
##################


ols.r1.t4 <- lm(rolloff ~ pctno + endorsement + TBT.Mention + legspons
                + cmsnspons + pctturnout + pctcollege + racecode
                + totamends + wordcountamend + Circulation.HunThou + midterm + likeminded,
                data = dat4)
summary(ols.r1.t4)
se.ols.r1.t4 <- sqrt(diag(vcovHC(ols.r1.t4)))

ols.p1.t4 <- lm(pctno ~ rolloff + endorsement + TBT.Mention + legspons
                + cmsnspons + pctturnout + pctcollege + racecode
                + totamends + wordcountamend + Circulation.HunThou 
                + midterm + likeminded,
                data = dat4)
summary(ols.p1.t4)
se.ols.p1.t4 <- sqrt(diag(vcovHC(ols.p1.t4)))


##################
## Table 3, OLS ##
## Interaction  ##
##################


ols.r2.t4 <- lm(rolloff ~ pctno + endorsement + TBT.Mention + legspons
                + cmsnspons + pctturnout + pctcollege + racecode
                + totamends + wordcountamend + Circulation.HunThou + midterm 
                + endorsement * likeminded,
                data = dat4)
summary(ols.r2.t4)
se.ols.r2.t4 <- sqrt(diag(vcovHC(ols.r2.t4)))

ols.p2.t4 <- lm(pctno ~ rolloff + endorsement + TBT.Mention + legspons
                + cmsnspons + pctturnout + pctcollege + racecode
                + totamends + wordcountamend + Circulation.HunThou + midterm 
                + endorsement * likeminded,
                data = dat4)
summary(ols.p2.t4)
se.ols.p2.t4 <- sqrt(diag(vcovHC(ols.p2.t4)))


#########################
## Output in Stargazer ##
## All four models     ##
#########################

stargazer(ols.r1.t4, ols.r2.t4, ols.p1.t4, ols.p2.t4,
          se = list(se.ols.r1.t4, se.ols.r2.t4, se.ols.p1.t4, se.ols.p2.t4),
          no.space = T,
          omit = c("year", "countynum"),
          omit.stat = c("rsq", "ser"),
          covariate.labels = c("% No Vote", "Rolloff",
                               "Newspaper Endorsement",
                               "Salience", "Legislature Sponsor",
                               "Commission Sponsor", "% County Turnout",
                               "% College Educated", "Ballot Position",
                               "Total Amendments", "Word Counts (100s)",
                               "Circulation", "Midterm Election",
                               "Like-Mindedness", "Endorse x Like-Mindedness", "Intercept")
)


#########################
## Table 4, OLS Models ##
## Circulation Omitted ##
#########################

ols.t4p1 <- lm(rolloff ~ pctno + endorsement
               + TBT.Mention + legspons + cmsnspons
               + pctturnout + pctcollege + racecode
               + totamends + wordcountamend
               + midterm
               + as.factor(year)
               + as.factor(countynum),
               data = dat4)
summary(ols.t4p1)
se.ols.t4p1 <- sqrt(diag(vcovHC(ols.t4p1)))

#####

ols.t4p2 <- lm(pctno ~ rolloff + endorsement
               + TBT.Mention + legspons + cmsnspons
               + pctturnout + pctcollege + racecode
               + totamends + wordcountamend + midterm
               + as.factor(year)
               + as.factor(countynum),
               data = dat4)
summary(ols.t4p2)
se.ols.t4p2 <- sqrt(diag(vcovHC(ols.t4p2)))


####################
## Report Table 5 ##
## With stargazer ##
####################

stargazer(ols.t4p1, ols.t4p2, 
          se = list(se.ols.t4p1, se.ols.t4p2),
          no.space = T,
          omit = c("year", "countynum"),
          omit.stat = c("rsq", "ser"))


#############################################
## Table 5, Year Controls and Interactions ##
#############################################

ols.t5p1 <- lm(rolloff ~ pctno + endorsement
               + TBT.Mention + legspons + cmsnspons
               + pctturnout + pctcollege + racecode
               + totamends + wordcountamend
               + midterm
               + endorsement * as.factor(year),
               data = dat4)
summary(ols.t5p1)
se.ols.t5p1 <- sqrt(diag(vcovHC(ols.t5p1)))

ols.t5p2 <- lm(pctno ~ rolloff + endorsement
               + TBT.Mention + legspons + cmsnspons
               + pctturnout + pctcollege + racecode
               + totamends + wordcountamend
               + midterm
               + endorsement * as.factor(year),
               data = dat4)
summary(ols.t5p2)
se.ols.t5p2 <- sqrt(diag(vcovHC(ols.t5p2)))


####################
## Report Table 5 ##
## With Stargazer ##
####################

stargazer(ols.t5p1, ols.t5p2, se = list(se.ols.t5p1, se.ols.t5p2),
          omit.stat = c("rsq", "ser"),
          no.space = T)


######################
## SUR, Table 6     ##
## No Circulation   ##
######################

equation.nocirc<-list(roll.off = (rolloff~pctno+dat4$endorsement
                                  + dat4$TBT.Mention
                                  + dat4$legspons
                                  + dat4$cmsnspons
                                  + dat4$pctturnout
                                  + dat4$pctcollege
                                  + dat4$racecode
                                  + dat4$totamends
                                  + dat4$wordcountamend
                                  + dat4$midterm)
                      , pctyes = (pctno~rolloff+dat4$endorsement
                                  + dat4$TBT.Mention
                                  + dat4$legspons
                                  + dat4$cmsnspons
                                  + dat4$pctturnout
                                  + dat4$pctcollege
                                  + dat4$racecode
                                  + dat4$totamends
                                  + dat4$wordcountamend
                                  + dat4$midterm))

surm.t6.nocirc <- systemfit(equation.nocirc, "SUR")
summary(surm.t6.nocirc)


####################################
## No Circulation, Table 6 Report ##
####################################

texreg(surm.t6.nocirc, digits = 3)


#########################################################
## SUR Model of Interactions, Table 7, No Circulation  ##
#########################################################

equation1.nocirc<-list(roll.off = (rolloff~pctno+dat4$endorsement
                                   + dat4$TBT.Mention
                                   + dat4$legspons
                                   + dat4$cmsnspons
                                   + dat4$pctturnout
                                   + dat4$pctcollege
                                   + dat4$racecode
                                   + dat4$totamends
                                   + dat4$wordcountamend
                                   + dat4$midterm
                                   + dat4$endorsement * dat4$TBT.Mention)
                       , pctyes = (pctno~rolloff+dat4$endorsement
                                   + dat4$TBT.Mention
                                   + dat4$legspons
                                   + dat4$cmsnspons
                                   + dat4$pctturnout
                                   + dat4$pctcollege
                                   + dat4$racecode
                                   + dat4$totamends
                                   + dat4$wordcountamend
                                   + dat4$midterm
                                   + dat4$endorsement * dat4$TBT.Mention))

surm.t7p1.nocirc <- systemfit(equation1.nocirc, "SUR")
summary(surm.t7p1.nocirc)


#####

equation2.nocirc<-list(roll.off = (rolloff~pctno+dat4$endorsement
                                   + dat4$TBT.Mention
                                   + dat4$legspons
                                   + dat4$cmsnspons
                                   + dat4$pctturnout
                                   + dat4$pctcollege
                                   + dat4$racecode
                                   + dat4$totamends
                                   + dat4$wordcountamend
                                   + dat4$midterm
                                   + dat4$endorsement * dat4$legspons)
                       , pctyes = (pctno~rolloff+dat4$endorsement
                                   + dat4$TBT.Mention
                                   + dat4$legspons
                                   + dat4$cmsnspons
                                   + dat4$pctturnout
                                   + dat4$pctcollege
                                   + dat4$racecode
                                   + dat4$totamends
                                   + dat4$wordcountamend
                                   + dat4$midterm
                                   + dat4$endorsement * dat4$legspons))

surm.t7p2.nocirc <- systemfit(equation2.nocirc, "SUR")
summary(surm.t7p2.nocirc)


#####

equation3.nocirc<-list(roll.off = (rolloff~pctno+dat4$endorsement
                                   + dat4$TBT.Mention
                                   + dat4$legspons
                                   + dat4$cmsnspons
                                   + dat4$pctturnout
                                   + dat4$pctcollege
                                   + dat4$racecode
                                   + dat4$totamends
                                   + dat4$wordcountamend
                                   + dat4$midterm
                                   + dat4$endorsement * dat4$cmsnspons)
                       , pctyes = (pctno~rolloff+dat4$endorsement
                                   + dat4$TBT.Mention
                                   + dat4$legspons
                                   + dat4$cmsnspons
                                   + dat4$pctturnout
                                   + dat4$pctcollege
                                   + dat4$racecode
                                   + dat4$totamends
                                   + dat4$wordcountamend
                                   + dat4$midterm
                                   + dat4$endorsement * dat4$cmsnspons))

surm.t7p3.nocirc <- systemfit(equation3.nocirc, "SUR")
summary(surm.t7p3.nocirc)


#####################################
## Display Table 7, No Circulation ##
## Using texreg                    ##
#####################################

texreg( list(surm.t7p1.nocirc, surm.t7p2.nocirc, surm.t7p3.nocirc), digits = 3)



##################################################
## Like-Mindedness SUR, Table 8, No Circulation ##
## Interaction Models Also Included             ##
##################################################

equation4.nocirc<-list(roll.off = (rolloff~pctno+dat4$endorsement
                                   + dat4$TBT.Mention
                                   + dat4$legspons
                                   + dat4$cmsnspons
                                   + dat4$pctturnout
                                   + dat4$pctcollege
                                   + dat4$racecode
                                   + dat4$totamends
                                   + dat4$wordcountamend
                                   + dat4$midterm
                                   + dat4$likeminded)
                       , pctyes = (pctno~rolloff+dat4$endorsement
                                   + dat4$TBT.Mention
                                   + dat4$legspons
                                   + dat4$cmsnspons
                                   + dat4$pctturnout
                                   + dat4$pctcollege
                                   + dat4$racecode
                                   + dat4$totamends
                                   + dat4$wordcountamend
                                   + dat4$midterm
                                   + dat4$likeminded))

surm.t8p1.nocirc <- systemfit(equation4.nocirc, "SUR")
summary(surm.t8p1.nocirc)


#####

equation5.nocirc<-list(roll.off = (rolloff~pctno+dat4$endorsement
                                   + dat4$TBT.Mention
                                   + dat4$legspons
                                   + dat4$cmsnspons
                                   + dat4$pctturnout
                                   + dat4$pctcollege
                                   + dat4$racecode
                                   + dat4$totamends
                                   + dat4$wordcountamend
                                   + dat4$midterm
                                   + dat4$likeminded * dat4$endorsement)
                       , pctyes = (pctno~rolloff+dat4$endorsement
                                   + dat4$TBT.Mention
                                   + dat4$legspons
                                   + dat4$cmsnspons
                                   + dat4$pctturnout
                                   + dat4$pctcollege
                                   + dat4$racecode
                                   + dat4$totamends
                                   + dat4$wordcountamend
                                   + dat4$midterm
                                   + dat4$likeminded  * dat4$endorsement))

surm.t8p2.nocirc <- systemfit(equation5.nocirc, "SUR")
summary(surm.t8p2.nocirc)


#############################
## Table 8, No Circulation ##
#############################

texreg(list(surm.t8p1.nocirc, surm.t8p2.nocirc), digits = 3)


##################################
## Add in Campaign Finance Data ##
##################################

dat4$newspent <- dat4$spent/100000


#############
## Table 9 ##
#############

ols.t9p1 <- lm(rolloff ~ pctno + endorsement
               + TBT.Mention + legspons + cmsnspons
               + pctturnout + pctcollege + racecode
               + totamends + wordcountamend
               + midterm
               + newspent,
               data = dat4)
summary(ols.t9p1)
se.ols.t9p1 <- sqrt(diag(vcovHC(ols.t9p1)))

ols.t9p2 <- lm(pctno ~ rolloff + endorsement
               + TBT.Mention + legspons + cmsnspons
               + pctturnout + pctcollege + racecode
               + totamends + wordcountamend
               + midterm
               + newspent,
               data = dat4)
summary(ols.t9p2)
se.ols.t9p2 <- sqrt(diag(vcovHC(ols.t9p2)))


#####################################################
## Report Campaign Finance Coefficients, Stargazer ##
#####################################################

stargazer(ols.t9p1, ols.t9p2, se = list(se.ols.t9p1, se.ols.t9p2),
          omit.stat = c("rsq", "ser"),
          no.space = T)