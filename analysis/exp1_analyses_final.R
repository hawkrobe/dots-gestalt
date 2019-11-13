# Used to generate Fig. 4 and all Exp. 1 results

rm(list=ls())
library(sft)
library(BayesFactor)
#graphics.off()


#allData <- read.csv("Experiment1_data.txt")
allData <- read.csv("~/Box Sync/Dots/Analysis/experiment1_data.txt")
# Remove all participants that did not complete all trials
allData <- subset(allData, allData$Subject != 702 & allData$Subject != 705 & allData$Subject != 712 & 
                           allData$Subject != 714 & allData$Subject != 715 & allData$Subject != 720)
allData$Subject <- as.factor(allData$Subject)
allData$Item <- as.factor(allData$Item)
subject_codes<-unique(allData$Subject)
print(levels(subject_codes))
sftData <- allData[,c(1,2)]
sftData$Channel1 <- NA
sftData$Channel1[with(allData, Stim1!="None" & Stim1!="Absent")] <- 1
sftData$Channel1[with(allData, Stim1=="None")] <- -1
sftData$Channel1[with(allData, Stim1=="Absent")] <- 0

sftData$Channel2 <- NA
sftData$Channel2[with(allData, Stim2!="None" & Stim2!="Absent")] <- 1
sftData$Channel2[with(allData, Stim2=="None")] <- -1
sftData$Channel2[with(allData, Stim2=="Absent")] <- 0

sftData$RT <- allData$RT
sftData$Correct <- allData$Correct
sftData$Condition <- allData$Condition
                                                                
sftData_configural_lines <- subset(sftData, sftData$Condition == 'Orientation_Config_Lines')
sftData_configural_nolines <- subset(sftData, sftData$Condition == 'Orientation_Config_NO_Lines')
sftData_control_lines <- subset(sftData, sftData$Condition == 'Orientation_Control_Lines')
sftData_control_nolines <- subset(sftData, sftData$Condition == 'Orientation_Control_NO_Lines')

sftData_configural <- subset(sftData, sftData$Condition == 'Orientation_Config_NO_Lines' | sftData$Condition == 'Orientation_Config_Lines')
sftData_control <- subset(sftData, sftData$Condition == 'Orientation_Control_NO_Lines' | sftData$Condition == 'Orientation_Control_Lines')

sftData_singleDot <- with(sftData, 
                          subset(sftData, (Channel1 == 0 & Channel2 == 1)
                                       | (Channel1 == 1 & Channel2 == 0)))
# This cutoff is rather low, so we may want to consider assessment functions or the Eidels et al
#  LBA based capacity measure (or even the integration index!)
capoutall <- capacityGroup(sftData, stopping.rule="OR", acc.cutoff=.6, plotCt=F)
capControl <- capacityGroup(sftData_control, stopping.rule="OR", acc.cutoff=.6, plotCt=F)
capConfigural <- capacityGroup(sftData_configural, stopping.rule="OR", acc.cutoff=.6, plotCt=F)

# Gives individual and group level data with respect to limited--unlimited--super capacity
print(capoutall$overview)

subjects <- unique(sftData$Subject) 
subjects <- sort(levels(subjects)[subjects])
conditions <- unique(sftData$Condition) 
conditions <- sort(levels(conditions)[conditions])

nsubjects <- length(subjects)
nCz <-  nsubjects * 4
CzMat <- data.frame(Subject=rep(subjects, 4), 
                    Configural=rep(c(TRUE,FALSE),each=2*length(subjects)), 
                    Lines=rep(rep(c(TRUE,FALSE),each=length(subjects)),2) )

for (i in 1:(2*nsubjects)) { 
  CzMat$Cz[i] <- capConfigural$capacity[[i]]$Ctest$statistic
  CzMat$Cz[i+(2*nsubjects)] <- capControl$capacity[[i]]$Ctest$statistic
}

CzMat$Lines <- factor(CzMat$Lines, labels=c("NoLines", "Lines"))
CzMat$Configural <- factor(CzMat$Configural, labels=c("Control", "Configural"))

print(CzMat)
accuracies <- rep(0, length(subject_codes)*4)
for (i in 1:length(subject_codes)) { accuracies[i] = with(sftData_configural_lines, mean(Correct[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
for (i in 1:length(subject_codes)) { accuracies[nsubjects+i] = with(sftData_configural_nolines, mean(Correct[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
for (i in 1:length(subject_codes)) { accuracies[2*nsubjects+i] = with(sftData_control_lines, mean(Correct[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
for (i in 1:length(subject_codes)) { accuracies[3*nsubjects+i] = with(sftData_control_nolines, mean(Correct[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
print(accuracies)

# Compute RTs on CORRECT Configural trials
RTs <- rep(0, length(subject_codes)*4)
for (i in 1:length(subject_codes)) { RTs[i] = with(sftData_configural_lines, mean(RT[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0 & Correct == 1]))}
for (i in 1:length(subject_codes)) { RTs[nsubjects+i] = with(sftData_configural_nolines, mean(RT[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0 & Correct == 1]))}
for (i in 1:length(subject_codes)) { RTs[2*nsubjects+i] = with(sftData_control_lines, mean(RT[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0 & Correct == 1]))}
for (i in 1:length(subject_codes)) { RTs[3*nsubjects+i] = with(sftData_control_nolines, mean(RT[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0 & Correct == 1]))}
print(RTs)
table1 <- data.frame(P=1:nsubjects)
for ( i in 1:nsubjects ) {
  table1$b111[i] <- "&"
  table1$Zfig_l[i] <- round(CzMat$Cz[i])
  table1$b[i] <- "&"
  table1$Accfig_l[i] <- round(accuracies[i], 2)
  table1$b2[i] <- "&"
  table1$RTfig_l[i] <- round(RTs[i])
  table1$b3[i] <- "&"
  table1$Zfig_no_l[i] <- round(CzMat$Cz[nsubjects+i],2)
  table1$b4[i] <- "&"
  table1$Accfig_no_l[i] <- round(accuracies[nsubjects+i], 2)
  table1$b5[i] <- "&"
  table1$RTfig_no_l[i] <- round(RTs[nsubjects+i])
  table1$b6[i] <- "&"
  table1$Ztrol_l[i] <- round(CzMat$Cz[2*nsubjects+i],2)
  table1$b7[i] <- "&"
  table1$Acctrol_l[i] <- round(accuracies[2*nsubjects+i], 2)
  table1$b8[i] <- "&"
  table1$RTtrol_l[i] <- round(RTs[2*nsubjects+i])
  table1$b9[i] <- "&"
  table1$Ztrol_no_l[i] <- round(CzMat$Cz[3*nsubjects+i],2)
  table1$b10[i] <- "&"
  table1$Acctrol_no_l[i] <- round(accuracies[3*nsubjects+i], 2)
  table1$b11[i] <- "&"
  table1$RTtrol_no_l[i] <- round(RTs[3*nsubjects+i])
  table1$b12[i] <- "&"
  table1$Acc_single[i] <- mean(with(sftData_singleDot, Correct[Subject == subject_codes[i]]))
  table1$b13[i] <- "&"
  table1$RT_single[i] <- mean(with(sftData_singleDot, RT[Subject == subject_codes[i] & Correct == 1]))
}

print(table1[,2:29])
CzMat$Lines <- factor(CzMat$Lines, labels=c("NoLines", "Lines"))
CzMat$Configural <- factor(CzMat$Configural, labels=c("Control", "Configural"))

library(ez)
# Here we use ANOVA to test for effects of configurality and lines.
#print(ezANOVA(data=CzMat, dv=Cz, wid=Subject, within=.(Lines,Configural)))

bfCt <- anovaBF(Cz ~ Lines * Configural + Subject, CzMat, whichRandom="Subject")
postCt <- posterior(bfCt[4], iterations=1000)
mean(postCt[,3]-postCt[,2])
quantile(postCt[,3]-postCt[,2], c(.025,.975))
mean(postCt[,5]-postCt[,4])
quantile(postCt[,5]-postCt[,4], c(.025,.975))

## Get means and HDI for Cz bar plot
Configural.Lines.mn <- with(CzMat, mean(Cz[Configural=="Configural"  & Lines=="Lines"]))
Configural.Lines.ci <- 
      quantile( postCt[,"mu"] + postCt[,"Configural-Configural"] + postCt[,"Lines-Lines"] +
      postCt[,"Configural:Lines-Configural.&.Lines"],
      c(.025,.975) )
Configural.NoLines.mn <- with(CzMat, mean(Cz[Configural=="Configural"  & Lines=="NoLines"]))
Configural.NoLines.ci <- 
      quantile( postCt[,"mu"] + postCt[,"Configural-Configural"] + postCt[,"Lines-NoLines"] +
      postCt[,"Configural:Lines-Configural.&.NoLines"],
      c(.025,.975) )

Control.Lines.mn <- with(CzMat, mean(Cz[Configural=="Control"  & Lines=="Lines"]))
Control.Lines.ci <- 
      quantile( postCt[,"mu"] + postCt[,"Configural-Control"] + postCt[,"Lines-Lines"] +
      postCt[,"Configural:Lines-Control.&.Lines"],
      c(.025,.975) )
Control.NoLines.mn <- with(CzMat, mean(Cz[Configural=="Control"  & Lines=="NoLines"]))
Control.NoLines.ci <- 
      quantile( postCt[,"mu"] + postCt[,"Configural-Control"] + postCt[,"Lines-NoLines"] +
      postCt[,"Configural:Lines-Control.&.NoLines"],
      c(.025,.975) )

czs <- cbind(c(Configural.NoLines.mn, Control.NoLines.mn),
             c(Configural.Lines.mn, Control.Lines.mn))
uppercz <- cbind(c(Configural.NoLines.ci[2], Control.NoLines.ci[2]),
             c(Configural.Lines.ci[2], Control.Lines.ci[2]))
lowercz <- cbind(c(Configural.NoLines.ci[1], Control.NoLines.ci[1]),
             c(Configural.Lines.ci[1], Control.Lines.ci[1]))




# Basic RT/Acc analysis
allData$Configural[allData$Condition=="Orientation_Config_Lines" | allData$Condition=="Orientation_Config_NO_Lines" ] <- "Configural"
allData$Configural[allData$Condition=="Orientation_Control_Lines" | allData$Condition=="Orientation_Control_NO_Lines" ] <- "Control"
allData$Configural[allData$Stim1=="Absent" | allData$Stim2=="Absent"] <- "Single"
allData$Configural[allData$Stim1=="None" & allData$Stim2=="None"] <- "Distractor"
allData$Configural <- as.factor(allData$Configural)

allData$Lines <- "NoLines"
allData$Lines[allData$Condition=="Orientation_Config_Lines" | allData$Condition=="Orientation_Control_Lines"] <- "Lines"
allData$Lines <- as.factor(allData$Lines)

allDataCorrect <- subset(allData, Correct==1)

allData2 <- subset(allData, Configural=="Configural" | Configural=="Control" | Configural=="Distractor")
bfCR2 <- anovaBF(Correct ~ Lines * Configural + Subject, allData2, whichRandom="Subject")
postCorrect <- posterior(bfCR2[4], iterations=1000)

allData2Correct <- subset(allData2, Correct==1)
bfRT2 <- anovaBF(RT ~ Lines * Configural + Subject, allData2Correct, whichRandom="Subject")
postRT <- posterior(bfRT2[4], iterations=1000)

## Get means and HDI for RT bar plot
Configural.Lines.mn <- with(allData2Correct, mean(RT[Configural=="Configural"  & Lines=="Lines"]))
Configural.Lines.ci <- 
      quantile( postRT[,"mu"] + postRT[,"Configural-Configural"] + postRT[,"Lines-Lines"] +
      postRT[,"Configural:Lines-Configural.&.Lines"],
      c(.025,.975) )
Configural.NoLines.mn <- with(allData2Correct, mean(RT[Configural=="Configural"  & Lines=="NoLines"]))
Configural.NoLines.ci <- 
      quantile( postRT[,"mu"] + postRT[,"Configural-Configural"] + postRT[,"Lines-NoLines"] +
      postRT[,"Configural:Lines-Configural.&.NoLines"],
      c(.025,.975) )

Control.Lines.mn <- with(allData2Correct, mean(RT[Configural=="Control"  & Lines=="Lines"]))
Control.Lines.ci <- 
      quantile( postRT[,"mu"] + postRT[,"Configural-Control"] + postRT[,"Lines-Lines"] +
      postRT[,"Configural:Lines-Control.&.Lines"],
      c(.025,.975) )
Control.NoLines.mn <- with(allData2Correct, mean(RT[Configural=="Control"  & Lines=="NoLines"]))
Control.NoLines.ci <- 
      quantile( postRT[,"mu"] + postRT[,"Configural-Control"] + postRT[,"Lines-NoLines"] +
      postRT[,"Configural:Lines-Control.&.NoLines"],
      c(.025,.975) )

Distractor.Lines.mn <- with(allData2Correct, mean(RT[Configural=="Distractor"  & Lines=="Lines"]))
Distractor.Lines.ci <- 
      quantile( postRT[,"mu"] + postRT[,"Configural-Distractor"] + postRT[,"Lines-Lines"] +
      postRT[,"Configural:Lines-Distractor.&.Lines"],
      c(.025,.975) )
Distractor.NoLines.mn <- with(allData2Correct, mean(RT[Configural=="Distractor"  & Lines=="NoLines"]))
Distractor.NoLines.ci <- 
      quantile( postRT[,"mu"] + postRT[,"Configural-Distractor"] + postRT[,"Lines-NoLines"] +
      postRT[,"Configural:Lines-Distractor.&.NoLines"],
      c(.025,.975) )

rts <- cbind(c(Configural.NoLines.mn, Control.NoLines.mn, Distractor.NoLines.mn),
             c(Configural.Lines.mn, Control.Lines.mn, Distractor.Lines.mn))
upperrt <- cbind(c(Configural.NoLines.ci[2], Control.NoLines.ci[2], Distractor.NoLines.ci[2]),
             c(Configural.Lines.ci[2], Control.Lines.ci[2], Distractor.Lines.ci[2]))
lowerrt <- cbind(c(Configural.NoLines.ci[1], Control.NoLines.ci[1], Distractor.NoLines.ci[1]),
             c(Configural.Lines.ci[1], Control.Lines.ci[1], Distractor.Lines.ci[1]))

## Get means and HDI for Acc bar plot
Configural.Lines.mn <- with(allData2, mean(Correct[Configural=="Configural"  & Lines=="Lines"]))
Configural.Lines.ci <- 
      quantile( postCorrect[,"mu"] + postCorrect[,"Configural-Configural"] + postCorrect[,"Lines-Lines"] +
      postCorrect[,"Configural:Lines-Configural.&.Lines"],
      c(.025,.975) )
Configural.NoLines.mn <- with(allData2, mean(Correct[Configural=="Configural"  & Lines=="NoLines"]))
Configural.NoLines.ci <- 
      quantile( postCorrect[,"mu"] + postCorrect[,"Configural-Configural"] + postCorrect[,"Lines-NoLines"] +
      postCorrect[,"Configural:Lines-Configural.&.NoLines"],
      c(.025,.975) )

Control.Lines.mn <- with(allData2, mean(Correct[Configural=="Control"  & Lines=="Lines"]))
Control.Lines.ci <- 
      quantile( postCorrect[,"mu"] + postCorrect[,"Configural-Control"] + postCorrect[,"Lines-Lines"] +
      postCorrect[,"Configural:Lines-Control.&.Lines"],
      c(.025,.975) )
Control.NoLines.mn <- with(allData2, mean(Correct[Configural=="Control"  & Lines=="NoLines"]))
Control.NoLines.ci <- 
      quantile( postCorrect[,"mu"] + postCorrect[,"Configural-Control"] + postCorrect[,"Lines-NoLines"] +
      postCorrect[,"Configural:Lines-Control.&.NoLines"],
      c(.025,.975) )

Distractor.Lines.mn <- with(allData2, mean(Correct[Configural=="Distractor"  & Lines=="Lines"]))
Distractor.Lines.ci <- 
      quantile( postCorrect[,"mu"] + postCorrect[,"Configural-Distractor"] + postCorrect[,"Lines-Lines"] +
      postCorrect[,"Configural:Lines-Distractor.&.Lines"],
      c(.025,.975) )
Distractor.NoLines.mn <- with(allData2, mean(Correct[Configural=="Distractor"  & Lines=="NoLines"]))
Distractor.NoLines.ci <- 
      quantile( postCorrect[,"mu"] + postCorrect[,"Configural-Distractor"] + postCorrect[,"Lines-NoLines"] +
      postCorrect[,"Configural:Lines-Distractor.&.NoLines"],
      c(.025,.975) )

crs <- cbind(c(Configural.NoLines.mn, Control.NoLines.mn, Distractor.NoLines.mn),
             c(Configural.Lines.mn, Control.Lines.mn, Distractor.Lines.mn))
uppercr <- cbind(c(Configural.NoLines.ci[2], Control.NoLines.ci[2], Distractor.NoLines.ci[2]),
             c(Configural.Lines.ci[2], Control.Lines.ci[2], Distractor.Lines.ci[2]))
lowercr <- cbind(c(Configural.NoLines.ci[1], Control.NoLines.ci[1], Distractor.NoLines.ci[1]),
             c(Configural.Lines.ci[1], Control.Lines.ci[1], Distractor.Lines.ci[1]))


uppercr[uppercr > 1] <- 1


library(gplots)

postscript("~/Box Sync/Dots/VisionResearch_Paper/images/Exp1_Results.eps", width=7.40, height=2.5,
    horizontal=FALSE, onefile=FALSE, paper="special")
par(mar=c(2.1,4.1,2.8,1.6), mfrow=c(1,3))
barplot2(rts, pch=4, col=c('lightblue','lightgreen', 'pink1'), xpd=FALSE, beside=TRUE, 
  ci.u=upperrt, ci.l=lowerrt,plot.ci=TRUE, names=c("No Lines", "Lines"),
  main="Two Dot Response Times", ylim=c(350,700), ylab="RT", xlab="")
legend('topright', legend=c("Configural", "Control", "Distractor"), fill=c("lightblue", "lightgreen", "pink"))
barplot2(crs, pch=4, col=c('lightblue','lightgreen', 'pink1'), xpd=FALSE, beside=TRUE, 
  ci.u=uppercr, ci.l=lowercr,plot.ci=TRUE, names=c("No Lines", "Lines"),
  main="Two Dot Accuracy", ylim=c(.8,1), ylab="Mean Accuracy", xlab="")

print(czs)
plot(c(-1,1),  czs[1,], type='l', col="blue", xlab="",
  lwd=2, xaxt="n", main="Capacity Z-Scores", xlim=c(-2,2), ylim=c(-10,3), ylab="Cz")
arrows(-1, uppercz[1,1], -1, lowercz[1,1], col="blue", code=3, angle=90)
arrows(1, uppercz[1,2], 1, lowercz[1,2], col="blue", code=3, angle=90)
lines(c(-1,1), czs[2,], col="green", lwd=2)
arrows(-1, uppercz[2,1], -1, lowercz[2,1], col="green", code=3, angle=90)
arrows(1, uppercz[2,2], 1, lowercz[2,2], col="green", code=3, angle=90)
abline(h=0, lty=2)
legend('topright', legend=c("Configural", "Control"), fill=c("blue", "green"))
axis(side=1, at=c(-1,1), labels=c("No Lines", "Lines"))

#barplot2(czs, pch=4, col=c('lightblue','lightgreen'), xpd=FALSE, beside=TRUE, 
#  ci.u=uppercz, ci.l=lowercz,plot.ci=TRUE, names=c("No Lines", "Lines"),
#  main="Capacity Z-Scores", ylim=c(-7,7), ylab="Cz", xlab="")
#legend('topright', legend=c("Configural", "Control"), fill=c("lightblue", "lightgreen"))
dev.off()

