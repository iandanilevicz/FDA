#------------------------------------------------------------------------------#
# Script name: 02_Functions.R
# 
# Author: Ian Danilevicz, INSERM, 2024
#
# Doing: functions 
# - plot_betat : this function plot a beta(t), where t is the grid of y(t).
# - plot_2betat : this function plot two beta(t), where t is the grid of y(t). For example, beta 1 and 2 may be two categories.
# - plot_3betat : this function plot three beta(t), where t is the grid of y(t).
#------------------------------------------------------------------------------#

plot_betat = function(t=NULL, beta, se, xlab="t", ylab=expression(beta(t)), alpha=0.05, shadow=TRUE, col=4){
  if(is.null(col)) col = 4
  n = length(beta)
  if(is.null(t)) t = 1:n
  lowerb = beta + qnorm(alpha/2)*se
  upperb = beta + qnorm(1-alpha/2)*se
  maxv = max(upperb)
  minv = min(lowerb)
  plot(c(t[1],t[n]),c(minv,maxv), type="n",xlab = xlab, ylab = ylab)
  if(shadow==TRUE){
    polygon(x=c(t,t[n:1]), y=c(upperb,lowerb[n:1]), col = "gray90", border=NA)
  }
  lines(t, lowerb, lty=3, col=col)
  lines(t, upperb, lty=3, col=col)
  lines(t, beta, lty=1, col=col)
}

plot_2betat = function(t=NULL, beta1, se1, beta2, se2, xlab="t", ylab=expression(beta(t)), alpha=0.05, cutpoint=NULL, shadow=TRUE, col=c(2,4)){
  if(is.null(col)) col = c(2,4)
  n = length(beta1)
  if(is.null(t)) t = 1:n
  lowerb1 = beta1 + qnorm(alpha/2)*se1
  upperb1 = beta1 + qnorm(1-alpha/2)*se1
  lowerb2 = beta2 + qnorm(alpha/2)*se2
  upperb2 = beta2 + qnorm(1-alpha/2)*se2
  maxv = max(c(upperb1,upperb2))
  minv = min(c(lowerb1,lowerb2))
  plot(c(t[1],t[n]),c(minv,maxv), type="n",xlab = xlab, ylab = ylab)
  if(shadow==TRUE){
    polygon(x=c(t,t[n:1]), y=c(upperb1,lowerb1[n:1]), col = "gray90", border=NA)
    polygon(x=c(t,t[n:1]), y=c(upperb2,lowerb2[n:1]), col = "gray90", border=NA)
  }
  if(!is.null(cutpoint)){
    m = length(cutpoint)
    for(i in 1:m){
      abline(v=cutpoint[i], lty=3)
    }
  } 
  lines(t, lowerb1, lty=3, col=col[1])
  lines(t, upperb1, lty=3, col=col[1])
  lines(t, beta1, lty=1, col=col[1], cex=1.2)
  lines(t, lowerb2, lty=3, col=col[2])
  lines(t, upperb2, lty=3, col=col[2])
  lines(t, beta2, lty=1, col=col[2], cex=1.2)
}

plot_3betat = function(t=NULL, beta1, se1, beta2, se2, beta3, se3, xlab="t", ylab=expression(beta(t)), alpha=0.05, cutpoint=NULL, shadow=TRUE, col=c(2,3,4)){
  if(is.null(col)) col = c(2,3,4)
  n = length(beta1)
  if(is.null(t)) t = 1:n
  lowerb1 = beta1 + qnorm(alpha/2)*se1
  upperb1 = beta1 + qnorm(1-alpha/2)*se1
  lowerb2 = beta2 + qnorm(alpha/2)*se2
  upperb2 = beta2 + qnorm(1-alpha/2)*se2
  lowerb3 = beta3 + qnorm(alpha/2)*se3
  upperb3 = beta3 + qnorm(1-alpha/2)*se3
  maxv = max(c(upperb1,upperb2,upperb3))
  minv = min(c(lowerb1,lowerb2,lowerb3))
  plot(c(t[1],t[n]),c(minv,maxv), type="n",xlab = xlab, ylab = ylab)
  if(shadow==TRUE){
    polygon(x=c(t,t[n:1]), y=c(upperb1,lowerb1[n:1]), col = "gray90", border=NA)
    polygon(x=c(t,t[n:1]), y=c(upperb2,lowerb2[n:1]), col = "gray90", border=NA)
    polygon(x=c(t,t[n:1]), y=c(upperb3,lowerb3[n:1]), col = "gray90", border=NA)
  }
  if(!is.null(cutpoint)){
    m = length(cutpoint)
    for(i in 1:m){
      abline(v=cutpoint[i], lty=3)
    }
  } 
  lines(t, lowerb1, lty=3, col=col[1])
  lines(t, upperb1, lty=3, col=col[1])
  lines(t, beta1, lty=1, col=col[1], cex=1.2)
  lines(t, lowerb2, lty=3, col=col[2])
  lines(t, upperb2, lty=3, col=col[2])
  lines(t, beta2, lty=1, col=col[2], cex=1.2)
  lines(t, lowerb3, lty=3, col=col[3])
  lines(t, upperb3, lty=3, col=col[3])
  lines(t, beta3, lty=1, col=col[3], cex=1.2)
}

