intervalScore <- function(scoreFile,win,step){
  score <- read.table(scoreFile)$V2
  len <- length(score)
  last <- floor((len-win)/step)
  result <- data.frame("Start"=seq(1,1+last*step,step)) %>% dplyr::mutate("End"=Start+win-1)
  result <- dplyr::mutate(result,"Score"=sapply(seq(1,1+last*step,step),function(i){sum(score[i:(i+win-1)])}))
  return(result)
}

nonZeroIntervals <- function(position,win,step) {
  p = 1
  pos = position[p]
  start = 1+step*ceiling((pos-win)/step)
  if (start<1) start=1
  startPosition <- c()
  endPosition <- c()
  startPosition <- c(startPosition,start)
  start = step*(floor((pos-1)/step))+win
  p <- p+1
  while (p<length(position)){
    pos = position[p];
    if (pos >= start){
      endPosition <- c(endPosition,start);
      startPosition <- c(startPosition,1+step*ceiling((pos-win)/step))
    }
    start = step*(floor((pos-1)/step))+win
    p <- p+1
  }
  endPosition <- c(endPosition,start);
  return(data.frame("Start"=startPosition,"End"=endPosition))
}

positionScore <- function(scoreFile,positionFile,win,step){
  score <- read.table(scoreFile)$V2
  position <- read.table(positionFile)
  chromosomesList <- unique(position$V1)
  result <- data.frame("Start"=c(),"End"=c(),"Chromosome"=c(),"Score"=c())
  for (chr in chromosomesList){
    idChr <- which(position$V1==chr)
    positionChr <- position[idChr,2]
    interval <- nonZeroIntervals(positionChr[order(positionChr)],win,step)
    startWin <- c()
    endWin <- c()
    for (i in 1:nrow(interval)){
      newStartWin <- seq(interval$Start[i],interval$End[i],step)
      startWin <- c(startWin,newStartWin)
      endWin <- c(endWin,newStartWin+win-1)
    }
    newScore <- data.frame("Start"=startWin,"End"=endWin,"Chromosome"=chr,"Score"=0)
    for (i in idChr){
      pos <- position$V2[i]
      winset <- intersect(which(newScore$Start<=pos),which(newScore$End>=pos))
      newScore$Score[winset] <- newScore$Score[winset]+score[i]
    }
    result <- rbind(result,newScore)
  }
  return(result)
}