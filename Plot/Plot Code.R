###****************************plot 1*******************************
mccr1 = read.table("mccr_ein.csv",sep=",",head=TRUE)
install.packages("reshape2")
library(reshape2)
mccr1.2 = melt(mccr1, measure.vars = c("LDA.SMOTE","Rpart.Entropy","Rpart.Gini","SVM",
                                        "J48","GLM"),
                 variable.name = "Algorithm")

###define function to calculate CI range using correction factor
#1
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#2
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}


#3
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}
#----------------------Add Function to the table--------------------
mccr1.3 = summarySEwithin (mccr1.2, measurevar="value", withinvars="Algorithm",
                        na.rm=FALSE, conf.interval=.95)
mccr1.3
library(ggplot2)
ggplot(mccr1.3, aes(x=Algorithm, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci),) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0.4,0.55)

###*******************************plot 2*************************************

mccr2 = read.table("mccr_zwei.csv",sep=",",head=TRUE)
mccr2.2 = melt(mccr2, measure.vars = c("Random.Forest","Neural.Network","Bagging"),
               variable.name = "Algorithm")
mccr2.3 = summarySEwithin (mccr2.2, measurevar="value", withinvars="Algorithm",
                           na.rm=FALSE, conf.interval=.95)
mccr2.3
library(ggplot2)
ggplot(mccr2.3, aes(x=Algorithm, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci),) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0.45,0.6)

###*******************************LDA*************************************
lda = read.table("LDA.csv",sep=",",head=TRUE)
str(lda)
lda.2 = melt(lda, measure.vars = c("LDA.Original","LDA.Dummy","LDA.Prescale",
                                   "LDA.SMOTE","LDA.SMOTE.Prescale","LDA.Upsample"),
               variable.name = "Algorithm")
lda.3 = summarySEwithin (lda.2, measurevar="value", withinvars="Algorithm",
                           na.rm=FALSE, conf.interval=.95)
lda.3
library(ggplot2)
ggplot(lda.3, aes(x=Algorithm, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci),) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0.3,0.55)

###*****************LDA 1******************
lda1.2 = melt(lda, measure.vars = c("LDA.Original"),
             variable.name = "Algorithm")
lda1.3 = summarySEwithin (lda1.2, measurevar="value", withinvars="Algorithm",
                         na.rm=FALSE, conf.interval=.95)
lda1.3
library(ggplot2)
ggplot(lda1.3, aes(x=Algorithm, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci),) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0.3,0.45)

###*****************LDA 2******************
lda2.2 = melt(lda, measure.vars = c("LDA.Original","LDA.Dummy"),
             variable.name = "Algorithm")
lda2.3 = summarySEwithin (lda2.2, measurevar="value", withinvars="Algorithm",
                         na.rm=FALSE, conf.interval=.95)
lda2.3
library(ggplot2)
ggplot(lda2.3, aes(x=Algorithm, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci),) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0.3,0.5)

###*****************LDA 3******************
lda3.2 = melt(lda, measure.vars = c("LDA.Original","LDA.Dummy","LDA.Prescale"),
             variable.name = "Algorithm")
lda3.3 = summarySEwithin (lda3.2, measurevar="value", withinvars="Algorithm",
                         na.rm=FALSE, conf.interval=.95)
lda3.3
library(ggplot2)
ggplot(lda3.3, aes(x=Algorithm, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci),) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0.3,0.55)

###*****************LDA 4******************
lda4.2 = melt(lda, measure.vars = c("LDA.Original","LDA.Dummy","LDA.Prescale",
                                   "LDA.SMOTE"),
             variable.name = "Algorithm")
lda4.3 = summarySEwithin (lda4.2, measurevar="value", withinvars="Algorithm",
                         na.rm=FALSE, conf.interval=.95)
lda4.3
library(ggplot2)
ggplot(lda4.3, aes(x=Algorithm, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci),) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0.3,0.55)

###*****************LDA 5******************
lda5.2 = melt(lda, measure.vars = c("LDA.Original","LDA.Dummy","LDA.Prescale",
                                   "LDA.SMOTE","LDA.SMOTE.Prescale"),
             variable.name = "Algorithm")
lda5.3 = summarySEwithin (lda5.2, measurevar="value", withinvars="Algorithm",
                         na.rm=FALSE, conf.interval=.95)
lda5.3
library(ggplot2)
ggplot(lda5.3, aes(x=Algorithm, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci),) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0.3,0.55)


###*****************Tune RF******************
Tune.RF = read.table("Tune_RF.csv",sep=",",head=TRUE)
str(Tune.RF)
Tune.RF.2 = melt(Tune.RF, measure.vars = c("Original.RF","Tuned.RF"),
              variable.name = "Algorithm")
Tune.RF.3 = summarySEwithin (Tune.RF.2, measurevar="value", withinvars="Algorithm",
                          na.rm=FALSE, conf.interval=.95)
Tune.RF.3
library(ggplot2)
ggplot(Tune.RF.3, aes(x=Algorithm, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci),) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0.5,0.6)

###*******************DP1***************************
DP1 = read.table("DP1.csv",sep=",",head=TRUE)
str(DP1)
DP1.2 = melt(DP1, measure.vars = c("Bench.Mark.Random.Forest","Deep.Learning.1"),
                 variable.name = "Algorithm")
DP1.3 = summarySEwithin (DP1.2, measurevar="value", withinvars="Algorithm",
                             na.rm=FALSE, conf.interval=.95)
DP1.3
library(ggplot2)
ggplot(DP1.3, aes(x=Algorithm, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci),) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0.5,0.75)

###*******************DP2***************************
DP2 = read.table("DP2.csv",sep=",",head=TRUE)
str(DP2)
DP2.2 = melt(DP2, measure.vars = c("Bench.Mark.Random.Forest","Deep.Learning.1",
                                   "Deep.Learning.2"),
             variable.name = "Algorithm")
DP2.3 = summarySEwithin (DP2.2, measurevar="value", withinvars="Algorithm",
                         na.rm=FALSE, conf.interval=.95)
DP2.3
library(ggplot2)
ggplot(DP2.3, aes(x=Algorithm, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci),) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0.5,0.75)
