rm(list=ls())
library(sft)
graphics.off()


allData <- read.csv("~/Box Sync/Dots/Analysis/Experiment3_data.txt")
# Remove all participants that did not complete all trials
allData <- subset(allData, allData$Subject != 5 & allData$Subject != 7 & allData$Subject !=8)
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

sftData_configural <- subset(sftData, sftData$Condition == 'Prox_Config_Lines' | sftData$Condition == 'Prox_Config_NO_Lines')
sftData_control <- subset(sftData, sftData$Condition == 'Prox_Control_Lines' | sftData$Condition == 'Prox_Control_NO_Lines')
sftData_configural_lines <- subset(sftData, sftData$Condition == 'Prox_Config_Lines')
sftData_configural_nolines <- subset(sftData, sftData$Condition == 'Prox_Config_NO_Lines')
sftData_control_nolines <- subset(sftData, sftData$Condition == 'Prox_Control_NO_Lines')
sftData_control_lines <- subset(sftData, sftData$Condition == 'Prox_Control_Lines')

sftData_singleDot <- with(sftData, 
                          subset(sftData, (Channel1 == 0 & Channel2 == 1)
                                 | (Channel1 == 1 & Channel2 == 0)))


sftData_configural$Condition <- factor(sftData_configural$Condition)
sftData_control$Condition <- factor(sftData_control$Condition)
# This cutoff is rather low, so we may want to consider assessment functions or the Eidels et al
#  LBA based capacity measure (or even the integration index!)
capoutall <- capacityGroup(sftData, acc.cutoff=.6, plotCt=F)

subjects <- unique(sftData$Subject) 
subjects <- sort(levels(subjects)[subjects])
conditions <- unique(sftData$Condition) 
conditions <- sort(levels(conditions)[conditions])

nsubjects <- length(subjects)
nCz <-  nsubjects * 4
CzMat <- data.frame(Subject=rep(subjects, length(conditions)), 
                    Configural=rep(c(TRUE,FALSE),each=2*length(subjects)), 
                    Lines=rep(rep(c(TRUE,FALSE),each=length(subjects)),2) )
CzMat$Cz <- NA
for (i in 1:nCz) { 
  CzMat$Cz[i] <- capoutall$capacity[[i]]$Ctest$statistic
}
CzMat$Lines <- factor(CzMat$Lines, labels=c("NoLines", "Lines"))
CzMat$Configural <- factor(CzMat$Configural, labels=c("Control", "Configural"))
accuracies <- rep(0, length(subject_codes)*4)
for (i in 1:length(subject_codes)) { accuracies[i] = with(sftData_configural_lines, mean(Correct[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
for (i in 1:length(subject_codes)) { accuracies[13+i] = with(sftData_configural_nolines, mean(Correct[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
for (i in 1:length(subject_codes)) { accuracies[26+i] = with(sftData_control_lines, mean(Correct[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
for (i in 1:length(subject_codes)) { accuracies[39+i] = with(sftData_control_nolines, mean(Correct[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
print(accuracies)
RTs <- rep(0, length(subject_codes)*4)
for (i in 1:length(subject_codes)) { RTs[i] = with(sftData_configural_lines, mean(RT[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0 & Correct == 1]))}
for (i in 1:length(subject_codes)) { RTs[13+i] = with(sftData_configural_nolines, mean(RT[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0 & Correct == 1]))}
for (i in 1:length(subject_codes)) { RTs[26+i] = with(sftData_control_lines, mean(RT[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0 & Correct == 1]))}
for (i in 1:length(subject_codes)) { RTs[39+i] = with(sftData_control_nolines, mean(RT[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0 & Correct == 1]))}
print(RTs)
table3 <- data.frame(P=1:13)
for ( i in 1:nsubjects ) {
  table3$b111[i] <- "&"
  table3$Zfig_l[i] <- round(CzMat$Cz[i],2)
  table3$b[i] <- "&"
  table3$Accfig_l[i] <- round(accuracies[i], 2)
  table3$b2[i] <- "&"
  table3$RTfig_l[i] <- round(RTs[i])
  table3$b3[i] <- "&"
  table3$Zfig_no_l[i] <- round(CzMat$Cz[nsubjects+i],2)
  table3$b4[i] <- "&"
  table3$Accfig_no_l[i] <- round(accuracies[nsubjects+i], 2)
  table3$b5[i] <- "&"
  table3$RTfig_no_l[i] <- round(RTs[nsubjects+i])
  table3$b6[i] <- "&"
  table3$Ztrol_l[i] <- round(CzMat$Cz[2*nsubjects+i],2)
  table3$b7[i] <- "&"
  table3$Acctrol_l[i] <- round(accuracies[2*nsubjects+i], 2)
  table3$b8[i] <- "&"
  table3$RTtrol_l[i] <- round(RTs[2*nsubjects+i])
  table3$b9[i] <- "&"
  table3$Ztrol_no_l[i] <- round(CzMat$Cz[3*nsubjects+i],2)
  table3$b10[i] <- "&"
  table3$Acctrol_no_l[i] <- round(accuracies[3*nsubjects+i], 2)
  table3$b11[i] <- "&"
  table3$RTtrol_no_l[i] <- round(RTs[3*nsubjects+i])
  table3$b12[i] <- "&"
  table3$Acc_single[i] <- mean(with(sftData_singleDot, Correct[Subject == subject_codes[i]]))
  table3$b13[i] <- "&"
  table3$RT_single[i] <- mean(with(sftData_singleDot, RT[Subject == subject_codes[i] & Correct == 1]))
}

print(table3[,2:29])
CzMat$Lines <- factor(CzMat$Lines, labels=c("NoLines", "Lines"))
CzMat$Configural <- factor(CzMat$Configural, labels=c("Control", "Configural"))

#library(ez)
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
allData$Configural[allData$Condition=="Prox_Config_Lines" | allData$Condition=="Prox_Config_NO_Lines" ] <- "Configural"
allData$Configural[allData$Condition=="Prox_Control_Lines" | allData$Condition=="Prox_Control_NO_Lines" ] <- "Control"
allData$Configural[allData$Stim1=="Absent" | allData$Stim2=="Absent"] <- "Single"
allData$Configural[allData$Stim1=="None" & allData$Stim2=="None"] <- "Distractor"
allData$Configural <- as.factor(allData$Configural)

allData$Lines <- "NoLines"
allData$Lines[allData$Condition=="Prox_Config_Lines" | allData$Condition=="Prox_Control_Lines"] <- "Lines"
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




postscript("Box Sync/Dots/VisionResearch_Paper/images/Exp3_Results.eps", width=7.40, height=2.5,
    horizontal=FALSE, onefile=FALSE, paper="special")
par(mar=c(2.1,4.1,2.8,1.6), mfrow=c(1,3))
barplot2(rts, pch=4, col=c('lightblue','lightgreen', 'pink1'), xpd=FALSE, beside=TRUE, 
  ci.u=upperrt, ci.l=lowerrt,plot.ci=TRUE, names=c("No Lines", "Lines"),
  main="Two Dot Response Times", ylim=c(350,700), ylab="RT", xlab="")
legend('topright', legend=c("Configural", "Control", "Distractor"), fill=c("lightblue", "lightgreen", "pink"))
barplot2(crs, pch=4, col=c('lightblue','lightgreen', 'pink1'), xpd=FALSE, beside=TRUE, 
  ci.u=uppercr, ci.l=lowercr,plot.ci=TRUE, names=c("No Lines", "Lines"),
  main="Two Dot Accuracy", ylim=c(.8,1), ylab="Mean Accuracy", xlab="")


plot(c(-1,1),  czs[1,], type='l', col="blue", xlab="",
  lwd=2, xaxt="n", main="Capacity Z-Scores", xlim=c(-2,2), ylim=c(-6,6), ylab="Cz")
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






if(FALSE) {
   accuracies <- rep(0, length(subject_codes)*4)
   for (i in 1:length(subject_codes)) { accuracies[i] = with(sftData_configural_lines, mean(Correct[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
   for (i in 1:length(subject_codes)) { accuracies[13+i] = with(sftData_configural_nolines, mean(Correct[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
   for (i in 1:length(subject_codes)) { accuracies[26+i] = with(sftData_control_lines, mean(Correct[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
   for (i in 1:length(subject_codes)) { accuracies[39+i] = with(sftData_control_nolines, mean(Correct[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
   print(accuracies)
   RTs <- rep(0, length(subject_codes)*4)
   for (i in 1:length(subject_codes)) { RTs[i] = with(sftData_configural_lines, mean(RT[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
   for (i in 1:length(subject_codes)) { RTs[13+i] = with(sftData_configural_nolines, mean(RT[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
   for (i in 1:length(subject_codes)) { RTs[26+i] = with(sftData_control_lines, mean(RT[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
   for (i in 1:length(subject_codes)) { RTs[39+i] = with(sftData_control_nolines, mean(RT[Subject == subject_codes[i] & Channel1 > 0 & Channel2 > 0]))}
   print(RTs)
   for ( i in 1:nCz ) {
     CzMat$Cz[i] <- round(capoutall$capacity.or[[i]]$Ctest$statistic, 2)
     CzMat$blah[i] <- "&"
     CzMat$Acc[i] <- round(accuracies[i], 2)
     CzMat$blah2[i] <- "&"
     CzMat$RT[i] <- round(RTs[i])
     CzMat$blah3[i] <- "&"
   }
   CzMat$Configural <- c(rep("Configural", length(subjects)*2), rep("Control", length(subjects)*2))
   
   # Plot w/ errorbars
   library(ggplot2)
   df <- aggregate(CzMat$Cz, list(config = CzMat$Configural, lines = CzMat$Lines), mean)
   names(df)[3] = "Cz"
   df$se <- aggregate(CzMat$Cz, list(config = CzMat$Configural, lines = CzMat$Lines), 
                      function(x) sd(x)/sqrt(length(subjects)))[,3]
   gp <- ggplot(df, aes(x=config, y=Cz, colour = lines, group = lines))
   opar <- theme_update(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_rect(colour = "black"))
   theme_set(opar)
   gp + geom_line(aes(linetype=lines), size = .6) + 
     geom_point(aes(shape = lines), size = 3) + 
     geom_errorbar(aes(ymax=Cz+se,ymin=Cz-se), width=.1)
   #interaction.plot(CzMat$Configural, CzMat$Lines, CzMat$Cz, legend = FALSE, type = 'b', pch = 1, xlab="", ylab = "C(t)", main = "Proximity Results")
   #legend(1.65,1, c("No Lines", "Lines"), cex = 0.8, lty = 2:1)
   library(ez)
   # Here we use ANOVA to test for effects of configurality and lines.
   aov_Cz <- ezANOVA(data=CzMat, dv=Cz, wid=Subject, within=.(Lines,Configural), return_aov=TRUE)
   aov_Acc <- ezANOVA(data=CzMat, dv=Acc, wid=Subject, within=.(Lines,Configural), return_aov=TRUE)
   # ges is generalized eta squared, a measure of effect size.  The author who defines ges
   #  uses .02 as a small effect, .13 as medium, and .26 as large.  We have .346 as the 
   #  effect size for configural!!!
}
