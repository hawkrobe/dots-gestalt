tester <- function(itemrange, left, right) {
  for (cnd in c("Orientation2", "Orientation2Lines") ) {
    for (i in itemrange) {
      sm <- sum(allData$Item == i & allData$Stim1==left & allData$Stim2 ==right & allData$Condition==cnd)
      if (sm != 16) {
        cat (i, "  \t", sm, "\t", cnd, "\n")
      }
    }
  }
}

removeIncomplete <- function(inData, expNumber) {
   inData$Subject <- factor(inData$Subject)
   outData <- inData
   nSubjects <- length(unique(inData$Subject))
   incomplete <- rep(FALSE, nSubjects)

   conditions <- sort(unique(inData$Condition) ) 
   for (cd in conditions) {
      if (expNumber!=2) {
         incomplete <- incomplete | ( with(inData, summary(Subject[Condition==cd]) ) != 480 )
      }
      if (expNumber==2) {
         incomplete <- incomplete | ( with(inData, summary(Subject[Condition==cd]) ) != 1152 )
      }
   }

   incompleteIDs <- names(summary(inData$Subject))[incomplete]
   for (sn in incompleteIDs) {
      outData <- subset(outData, Subject != sn)
   }
   outData$Subject <- factor(outData$Subject)
   return(outData)
}


sftize <- function(inData, expNumber) {
   attach(inData)

   sftData <- inData[,c(1,2)]
   sftData$Channel1 <- NA
   sftData$Channel1[Stim1!="None" & Stim1!="Absent"] <- 1
   sftData$Channel1[Stim1=="None"] <- -1
   sftData$Channel1[Stim1=="Absent"] <- 0
   
   sftData$Channel2 <- NA
   sftData$Channel2[Stim2!="None" & Stim2!="Absent"] <- 1
   sftData$Channel2[Stim2=="None"] <- -1
   sftData$Channel2[Stim2=="Absent"] <- 0
   
   sftData$RT <- inData$RT
   sftData$Correct <- inData$Correct

   if(expNumber ==1 )  {
      sft.Configural <- subset(sftData, Condition=="Orientation_Config_Lines" | Condition=="Orientation_Config_NO_Lines" )
      sft.Control    <- subset(sftData, Condition=="Orientation_Control_Lines" | Condition=="Orientation_Control_NO_Lines" )
      sft.Configural$Condition <- factor(sft.Configural$Condition)
      sft.Control$Condition <- factor(sft.Control$Condition)
   }

   if (expNumber ==2) {
      ControlTrials <- Stim1!="None" & Stim1 == Stim2
      ControlLeft  <- Stim1!="None" & Stim1 !="Absent" & Stim2=="Absent"
      ControlRight <- Stim2!="None" & Stim2 !="Absent" & Stim1=="Absent"
      
      ConfiguralTrials <- Stim1!="None" & Stim1!="Absent" & Stim2!="None" & Stim2 != "Absent" & !ControlTrials
      ConfiguralLeft   <- (Stim1=="DownRight" | Stim1=="UpRight") & Stim2=="Absent"
      ConfiguralRight  <- (Stim2=="DownLeft"  | Stim2=="UpLeft")  & Stim1=="Absent"

      sft.Control    <- sftData[ControlTrials | ControlLeft | ControlRight,]
      sft.Configural <- sftData[ConfiguralTrials | ConfiguralLeft | ConfiguralRight,]
   }

   if (expNumber ==3) {
      sft.Configural <- subset(sftData, Condition=="Prox_Config_Lines" | Condition=="Prox_Config_NO_Lines" )
      sft.Control    <- subset(sftData, Condition=="Prox_Control_Lines" | Condition=="Prox_Control_NO_Lines" )
      sft.Configural$Condition <- factor(sft.Configural$Condition)
      sft.Control$Condition <- factor(sft.Control$Condition)
   }
   
   detach(inData)
   return(list(Control=sft.Control, Configural=sft.Configural))
}

cpGroup <- function(inData, ratio=TRUE, stopping.rule=c("OR", "AND", "STST")) {
  subjects <- sort(unique(inData$Subject))
  subjects <- factor(subjects)
  nsubjects <- length(subjects)

  conditions <- sort(unique(inData$Condition))
  conditions <- factor(conditions)
  nconditions <- length(conditions)

  channels <- grep("Channel", names(inData), value=T)
  nchannels <- length(channels)
  if(nchannels < 2) {
    stop("Not enough channels for capacity analysis.")
  }

  if (length(stopping.rule) != 1) {
   warning("Using OR stopping rule for cpGroup")
  }
  rule <- match.arg(stopping.rule, c("OR","AND","STST"))
  if(rule == "OR") {
    capacity <- capacity.or
  } else if (rule == "AND") {
    capacity <- capacity.and 
  } else if (rule == "STST"){
    capacity <- capacity.stst
  } else  {
    stop("Please choose a valid stopping rule for cpGroup")
  }


  subj.out <- c()
  cond.out <- c()
  cp.out <- c()

  RTlist <- vector("list", nchannels)
  CRlist <- vector("list", nchannels)

  for ( cn in 1:nconditions ) {
    if (is.factor(conditions)) {cond <- levels(conditions)[cn]} else {cond <- conditions[cn] }
    condsubjects <- factor(with(inData, sort(unique(Subject[Condition==cond]))))
    ncondsubjects <- length(condsubjects)

    for ( sn in 1:ncondsubjects ) {
      if (is.factor(condsubjects)) {subj <- levels(condsubjects)[sn]} else {subj <- condsubjects[sn] }
      subj.out <- c(subj.out, subj)
      cond.out <- c(cond.out, cond)

      ds <- inData$Subject==subj & inData$Condition==cond

      if(rule =="STST") {
        usechannel <- ds &  (apply(inData[,channels]>0, 1, sum)==1) & (apply(inData[,channels]<0, 1, sum)>0)
        CRlist[[1]] <- inData$Correct[usechannel]

        usechannel <- ds & apply(inData[,channels]>=0, 1, all) & (apply(inData[,channels]!=0, 1, sum)==1)
        CRlist[[2]] <- inData$Correct[usechannel]

        cp <- mean(CRlist[[1]]) - mean(CRlist[[2]])

      } else {
        usechannel <- ds & apply(inData[,channels]>0, 1, all)

        CRlist[[1]] <- inData$Correct[usechannel]

        cp <- 1
        for ( ch in 1:nchannels ) {
          usechannel <- ds & inData[,channels[ch]]>0 & 
                        apply(as.matrix(inData[,channels[-ch]]==0), 1, all)
          CRlist[[ch+1]] <- inData$Correct[usechannel]
          
          if (stopping.rule=="OR") { cp <- cp * (1-mean(CRlist[[ch+1]])) }
          if (stopping.rule=="AND") { cp <- cp * mean(CRlist[[ch+1]]) }
        }
        if (stopping.rule=="OR") {cp <- cp - ( 1 - mean(CRlist[[1]]) ) }
        if (stopping.rule=="AND") {cp <- mean(CRlist[[1]]) - cp }
        cp.out <- c(cp.out , cp)
      }
    }
  }
  cptable <- data.frame(Subject=subj.out, Condition=cond.out, Cp=cp.out)
}


