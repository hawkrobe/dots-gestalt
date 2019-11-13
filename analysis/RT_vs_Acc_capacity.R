# Used to generate Fig. 5, 7, 10

rm(list=ls())
library(sft)
library(gplots)
library(BayesFactor)
source("processDots_functions.R")
graphics.off()
setEPS()


for (expNumber in 1:3) {
   # Read in data from csv
   datafile <- paste("Experiment", expNumber, "_data.txt", sep="")
   expData <- removeIncomplete(read.csv(datafile), expNumber)
   sftData <- sftize(expData, expNumber)

   subjects <- unique(sftData$Control$Subject) 
   subjects <- sort(levels(subjects)[subjects])
   nsubjects <- length(subjects)

   # Calculate RT and accuracy capacity for control trials
   capControl <- capacityGroup(sftData$Control, stopping.rule="OR", acc.cutoff=.6, plotCt=F)
   cpControl <- cpGroup(sftData$Control, stopping.rule="OR")

   # Calculate RT and accuracy capacity for configural trials
   capConfigural <- capacityGroup(sftData$Configural, stopping.rule="OR", acc.cutoff=.6, plotCt=F)
   cpConfigural <- cpGroup(sftData$Configural, stopping.rule="OR")


   # Aggregate capacity data into a single data frame
   CzMat <- data.frame(Subject=rep(subjects, 4), 
                       Configural=rep(c(TRUE,FALSE),each=2*nsubjects), 
                       Lines=TRUE )
   CzMat$Lines[grep("NO", cpConfigural$Condition)] <- FALSE
   CzMat$Lines[nsubjects + grep("NO", cpControl$Condition)] <- FALSE
   
   for (i in 1:(2*nsubjects)) { 
     if(!is.null(capConfigural$capacity[[i]]) ) {
       CzMat$Cz[i] <- capConfigural$capacity[[i]]$Ctest$statistic
     }
     if(!is.null(capControl$capacity[[i]]) ) {
       CzMat$Cz[i+(2*nsubjects)] <- capControl$capacity[[i]]$Ctest$statistic
     }
   }
   CzMat$Cp <- c(cpConfigural$Cp, cpControl$Cp)


   # Plot RT capacity against Accuracy capacity 
   postscript(file=paste("CpCz_Exp", expNumber, ".eps", sep=""), width=7.40, height=3.7)
      par(mfrow=c(1,2))
      attach(CzMat)
      plot(Cz[Configural & Lines], Cp[Configural & Lines], xlim=range(Cz), ylim=range(Cp),
            main="Configural", xlab="Cz (RT-based)", ylab="Cp (Accuracy-based)")
      points(Cz[Configural & !Lines], Cp[Configural & !Lines], pch=2)
      abline(h=0)
      abline(v=0)
   
      legend("bottomleft", c("Lines", "No Lines"), pch=c(1,2))
      plot(Cz[!Configural & Lines], Cp[!Configural & Lines], xlim=range(Cz), ylim=range(Cp),
            main="Control", xlab="Cz", ylab="Cp")
      points(Cz[!Configural & !Lines], Cp[!Configural & !Lines], pch=2)
      abline(h=0)
      abline(v=0)
      detach(CzMat)
   dev.off()
}
