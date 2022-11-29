### Full ROC

rocr.pred <- prediction(pred, test$Churn)
rocr.pred.df <- data.frame(fpr=slot(performance(rocr.pred, "tpr", "fpr"),"x.values")[[1]],tpr=slot(performance(rocr.pred, "tpr", "fpr"),"y.values")[[1]])

### Constructing the baseline ROC

info_baseline = NULL
for (p in c(.2, 0.5, .8)) {
  if (!is.null(info_baseline)) {
    info_baseline$new <- "no"
  }
  info_baseline <- rbind(info_baseline, data.frame(p=p,
                                                   tpr=p,
                                                   fpr=p,
                                                   new="yes"))
  print(ggplot() +
          geom_line(data=rocr.pred.df,aes(x=fpr,y=tpr),lwd=1) +
          geom_point (data=info_baseline, aes(x=fpr, y=tpr, col=new),cex=5) +
          xlab("False Positive Rate") +
          ylab("True Positive Rate") +
          theme_bw() +
          xlim(0, 1) +
          ylim(0, 1) +
          scale_color_manual(values=c("no" = "gray", "yes"="black"), guide=FALSE) +
          theme(axis.title=element_text(size=30), axis.text=element_text(size=20)))
Sys.sleep(1)
  
}

#Plotting the ROC
print(ggplot(rocr.pred.df,aes(x=fpr)) +
  geom_line(aes(y=tpr),lwd=1) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw() +
  xlim(0, 1) +
  ylim(0, 1) +
  theme(axis.title=element_text(size=30), axis.text=element_text(size=20)))

# ROC vs. baseline
base <- data.frame(x=c(0,1),y=c(0,1))
perfect <- data.frame(x=c(0,0,1),y=c(0,1,1))
print(ggplot() +
        geom_line(data=rocr.pred.df,aes(x=fpr,y=tpr,colour='LR'),lwd=1) +
        geom_line(data=base, aes(x=x, y=y,colour='B'),lwd=1) +
        geom_line(data=perfect, aes(x=x, y=y,colour='P'),lwd=1) +
        xlab("False Positive Rate") +
        ylab("True Positive Rate") +
        theme_bw() +
        xlim(0, 1) +
        ylim(0, 1) +
        scale_color_manual(values = c('LR'='black','B'='blue','P'='red'), name = "", labels = c('LR'="Logistic regression", 'B'="Baseline", "P"="Perfect prediction")) +
        theme(axis.title=element_text(size=18), axis.text=element_text(size=18), legend.text=element_text(size=18)))

# AUC
base <- data.frame(x=c(0,1),y=c(0,1))
perfect <- data.frame(x=c(0,0,1),y=c(0,1,1))
print(ggplot() +
        geom_line(data=rocr.pred.df,aes(x=fpr,y=tpr,colour='LR'),lwd=1) +
        geom_area(data=rocr.pred.df, aes(x=fpr,y=tpr),fill = "grey60",alpha=0.4,position="identity") +
        geom_line(data=base, aes(x=x, y=y,colour='B'),lwd=1) +
        geom_line(data=perfect, aes(x=x, y=y,colour='P'),lwd=1) +
        xlab("False Positive Rate") +
        ylab("True Positive Rate") +
        theme_bw() +
        xlim(0, 1) +
        ylim(0, 1) +
        scale_color_manual(values = c('LR'='black','B'='blue','P'='red'), name = "", labels = c('LR'="Logistic regression", 'B'="Baseline", "P"="Perfect prediction")) +
        theme(axis.title=element_text(size=18), axis.text=element_text(size=18), legend.text=element_text(size=18)))


#expected profit at 1/3 threshold
thresh = 1/3
sum(pred <= thresh & test$Churn == 0)*1000 +
  sum(pred >= thresh & test$Churn==0)*800 + 
  sum(pred >= thresh & test$Churn==1)*.5*800

### Loss computation, and sensitivity with threshold

cutoff.info <- data.frame(cutoff = seq(0, 1, .01))
cutoff.info$payout <- sapply(cutoff.info$cutoff, function(thresh) {
  
  sum(pred <= thresh & test$Churn == 0)*1000 +
    sum(pred >= thresh & test$Churn==0)*800 + 
    sum(pred >= thresh & test$Churn==1)*.5*800
})

cutoff.info$baseline_payout <- sum(test$Churn==0)*1000
print(ggplot(cutoff.info, aes(x=cutoff)) + 
        geom_line(aes(y=payout, colour='LR'), lwd=1) + 
        geom_line(aes(y=baseline_payout, colour='B'), lwd=1) + 
        xlab("Churn threshold") + 
        ylab("Total profit on test-set") + 
        scale_color_manual(values = c('LR'='black','B'='red'), name = "Method", labels = c('LR'="Logistic regression", 'B'="Baseline")) +
        theme(text=element_text(size=18),axis.text.x=element_text(size=18),axis.text.y=element_text(size=18)))
max(pred)
