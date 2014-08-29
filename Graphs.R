# This script is for producing visual output for the 'Sleep Study'. The data it uses variables from the global environment created in the 'processing' script. 

plot.dir <- "C:/Users/Renerts/Documents/Sleep_study/Paper/tables_graphs"

## Function definitions

StErr <- function(x) {  
        sqrt(var(x,na.rm=TRUE)/sum(!is.na(x)))
}

CI.t <- function(x) {
        n=sum(!is.na(x))
        qt(0.975,df=n-1)*sd(x,na.rm=TRUE)/sqrt(n)
}

CI.norm <- function(x) {
        n=sum(!is.na(x))
        qnorm(0.975)*sd(x,na.rm=TRUE)/sqrt(n)
}
        
table.plot <- function(x) {  #Takes array as input, returns data.frame ready for plotting
        
        means <- colMeans(x, na.rm=TRUE)
        StErr <- apply(x, 2, StErr)
        StErr.right <- means + StErr
        StErr.left <- means - StErr
        sub.x <- data.frame(means,StErr, StErr.left, StErr.right)
        return(sub.x)
}

## Dprimes

inter.dprime.sure.wake <- table.plot(main.memory[main.memory$Group=='w', c('dprime_sure_low', 'dprime_sure_high')])
rownames(inter.dprime.sure.wake) <- c('wake.sure.low', 'wake.sure.high')
inter.dprime.sure.sleep <- table.plot(main.memory[main.memory$Group=='s', c('dprime_sure_low', 'dprime_sure_high')])
rownames(inter.dprime.sure.sleep) <- c('sleep.sure.low', 'sleep.sure.high')

dprime.plot <- rbind(inter.dprime.sure.sleep, inter.dprime.sure.wake)

### Dprime plot

# png(filename=file.path(plot.dir,'dprime_all.png'),width=850, height=450, pointsize = 16)
# x.dprime.plot <- 1:nrow(dprime.plot)
# up <- round(max(dprime.plot$StErr.right))
# dn <- round(min(dprime.plot$StErr.left))
# plot(dprime.plot$means~x.dprime.plot,  # Memory performance as Rcorr values
#      cex=1.5, 
#      xaxt='n', 
#      yaxt='n',
#      ylim=c(dn,up), 
#      xlab='Group / Condition', 
#      ylab='Dprime value', 
#      main='Memory performance for "sure" responses',
#      col=c('darkgray', 'lightgray'), 
#      pch=16, 
#      bty='l')
# axis(1, at=x.dprime.plot, labels=FALSE)
# text(x = x.dprime.plot, par("usr")[3]-0.01, labels = rownames(dprime.plot), srt = 0, pos = 1, xpd = TRUE)
# axis(2, at=seq(dn, up, by=0.2), labels = FALSE)
# text(y = seq(dn, up, by=0.2),labels = seq(dn, up, by=0.2), par("usr")[1]-0.1, srt = 0, pos = 2, xpd = TRUE)
# arrows(x.dprime.plot, dprime.plot$StErr.right, x.dprime.plot,dprime.plot$StErr.left, code=3, length=0.2, angle=90,col=c(rep('darkgray',2), rep('darkblue',2), rep('black',2)))
# #legend("bottomright", paste(rownames(dprime.plot), ": mean", round(dprime.plot$means, digits=2), '±',round(dprime.plot$StErr, digits=3)),ncol=3,text.width=1.5)
# dev.off()

png(filename=file.path(plot.dir,'dprime_barplot.png'),width=500, height=450, pointsize = 16)
up <- ceiling(max(dprime.plot$StErr.right))
dn <- round(min(dprime.plot$StErr.left))
dprime.barplot <- barplot(dprime.plot$means,  # Memory performance as Rcorr values
#      cex=1.5, 
#      xaxt='n', 
#      yaxt='n',
        axes=F,
        space=rep(c(0.8, 0),2),
        ylim=c(0,up), 
        xlab='Reward context', 
        ylab=paste("Memory performance (", "d","')", sep=""),
        main='Memory performance for "sure" responses \n means and SEM',
        col=c('darkgray','darkgray', 'lightgray','lightgray'), 
        pch=16, 
        names.arg=rep(c('Low','High'),2))
axis(2, at=seq(0, up, by=0.2), labels = FALSE)
text(y = seq(0, up, by=0.2),labels = seq(0, up, by=0.2), par("usr")[1]-0.1, srt = 0, pos = 2, xpd = TRUE)
arrows(dprime.barplot, dprime.plot$StErr.right, dprime.barplot,dprime.plot$StErr.left, code=3, length=0.2, angle=90,col='black')
legend("topleft", legend=c('Sleep group','Wake group'), fill=c('darkgray', 'lightgray'),bty='n',cex=1.2)
lines(dprime.barplot[c(3,4), ], rep(1.6,2))
text(mean(dprime.barplot[c(3,4), ]), 1.65, labels='*', cex=1.5)
dev.off()
