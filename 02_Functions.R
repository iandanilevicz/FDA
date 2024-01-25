#------------------------------------------------------------------------------#
# Script name: 02_Functions.R
# 
# Author: Ian Danilevicz, INSERM, 2024
#
# Doing: functions 
# - plot_betat : this function plot a beta(t), where t is the grid of y(t).
# - plot_2betat : this function plot two beta(t), where t is the grid of y(t). For example, beta 1 and 2 may be two categories.
# - plot_3betat : this function plot three beta(t), where t is the grid of y(t).
# - plot_4betat : this function plot four beta(t), where t is the grid of y(t).
#------------------------------------------------------------------------------#

plot_betat = function(beta, se, t=NULL, xlab="t", ylab=expression(beta(t)), alpha=0.05, cutpoint=NULL, shadow=TRUE, col="red",n1=1,n2=NULL){
  # beta is a vector of coefficients with the length n
  # se is the correspondent standard errors of the coefficients
  # t is the grid of y(t) and beta(t) 
  # xlab and ylab equal to plot function
  # alpha is 1- confidence level, eg, alpha = 0.05 means a confidence of 95%
  # cutpoint if you want to display some cutpoint as LIPA and MVPA
  # shadow = TRUE creates a CI with a gray shadow
  # col determines the betas' colors
  # n1 if you want to remove a part o the grid t at the left
  # n2 if you want to remove a part o the grid t at the right
  if(is.null(col)) col = c(2,3)
  if(is.null(n2)) {
    n2 = length(beta)
    }
  n = n2 - n1 + 1
  beta = beta[n1:n2]
  se = se[n1:n2]
  if(is.null(t)) {
    t = 1:n
  } else {
    t = t[n1:n2]
  }
  lowerb = beta + qnorm(alpha/2)*se
  upperb = beta + qnorm(1-alpha/2)*se
  maxv = max(c(upperb))
  minv = min(c(lowerb))
  plot(c(t[1],t[n]),c(minv,maxv), type="n",xlab = xlab, ylab = ylab)
  if(shadow==TRUE){
    polygon(x=c(t,t[n:1]), y=c(upperb,lowerb[n:1]), col = "gray90", border=NA)
  }
  if(!is.null(cutpoint)){
    m = length(cutpoint)
    for(i in 1:m){
      abline(v=cutpoint[i], lty=3)
    }
  } 
  lines(t, lowerb, lty=3, col=col[1])
  lines(t, upperb, lty=3, col=col[1])
  lines(t, beta, lty=1, col=col[1], cex=1.2)
}

plot_2betat = function(beta1, beta2, se1, se2, t=NULL, xlab="t", ylab=expression(beta(t)), alpha=0.05, cutpoint=NULL, shadow=TRUE, col=c(2,3),n1=1,n2=NULL){
  # beta1, beta2 are vectors each set of coefficients with the same length n
  # se1, se2 are the correspondent standard errors of the coefficients
  # t is the grid of y(t) and each beta(t) 
  # xlab and ylab equal to plot function
  # alpha is 1- confidence level, eg, alpha = 0.05 means a confidence of 95%
  # cutpoint if you want to display some cutpoint as LIPA and MVPA
  # shadow = TRUE creates a CI with a gray shadow
  # col determines the betas' colors
  # n1 if you want to remove a part o the grid t at the left
  # n2 if you want to remove a part o the grid t at the right
  if(is.null(col)) col = c(2,3)
  if(is.null(n2)) {
    n2 = length(beta1)
    }
  n = n2 - n1 + 1
  beta1 = beta1[n1:n2]
  beta2 = beta2[n1:n2]
  se1 = se1[n1:n2]
  se2 = se2[n1:n2]
  if(is.null(t)) {
    t = 1:n
  } else {
    t = t[n1:n2]
  }
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

plot_3betat = function(beta1, beta2, beta3, se1, se2, se3, t=NULL, xlab="t", ylab=expression(beta(t)), alpha=0.05, cutpoint=NULL, shadow=TRUE, col=c(2,3,4),n1=1,n2=NULL){
  # beta1, beta2, beta3 are vectors each set of coefficients with the same length n
  # se1, se2, se3 are the correspondent standard errors of the coefficients
  # t is the grid of y(t) and each beta(t) 
  # xlab and ylab equal to plot function
  # alpha is 1- confidence level, eg, alpha = 0.05 means a confidence of 95%
  # cutpoint if you want to display some cutpoint as LIPA and MVPA
  # shadow = TRUE creates a CI with a gray shadow
  # col determines the betas' colors
  # n1 if you want to remove a part o the grid t at the left
  # n2 if you want to remove a part o the grid t at the right
  if(is.null(col)) col = c(2,3,4)
  if(is.null(n2)) {
    n2 = length(beta1)
    }
  n = n2 - n1 + 1
  beta1 = beta1[n1:n2]
  beta2 = beta2[n1:n2]
  beta3 = beta3[n1:n2]
  se1 = se1[n1:n2]
  se2 = se2[n1:n2]
  se3 = se3[n1:n2]
  if(is.null(t)) {
    t = 1:n
  } else {
    t = t[n1:n2]
  }
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

plot_4betat = function(beta1, beta2, beta3, beta4, se1, se2, se3, se4, t=NULL, xlab="t", ylab=expression(beta(t)), alpha=0.05, cutpoint=NULL, shadow=TRUE, col=c(2,3,4,1),n1=1,n2=NULL){
  # beta1, beta2, beta3, beta4 are vectors each set of coefficients with the same length n
  # se1, se2, se3, se4 are the correspondent standard errors of the coefficients
  # t is the grid of y(t) and each beta(t) 
  # xlab and ylab equal to plot function
  # alpha is 1- confidence level, eg, alpha = 0.05 means a confidence of 95%
  # cutpoint if you want to display some cutpoint as LIPA and MVPA
  # shadow = TRUE creates a CI with a gray shadow
  # col determines the betas' colors
  # n1 if you want to remove a part o the grid t at the left
  # n2 if you want to remove a part o the grid t at the right
  if(is.null(col)) col = c(2,3,4,1)
  if(is.null(n2)) {
    n2 = length(beta1)
    }
  n = n2 - n1 + 1  
  beta1 = beta1[n1:n2]
  beta2 = beta2[n1:n2]
  beta3 = beta3[n1:n2]
  beta4 = beta4[n1:n2]
  se1 = se1[n1:n2]
  se2 = se2[n1:n2]
  se3 = se3[n1:n2]
  se4 = se4[n1:n2]
  if(is.null(t)) {
    t = 1:n
  } else {
    t = t[n1:n2]
  }
  lowerb1 = beta1 + qnorm(alpha/2)*se1
  upperb1 = beta1 + qnorm(1-alpha/2)*se1
  lowerb2 = beta2 + qnorm(alpha/2)*se2
  upperb2 = beta2 + qnorm(1-alpha/2)*se2
  lowerb3 = beta3 + qnorm(alpha/2)*se3
  upperb3 = beta3 + qnorm(1-alpha/2)*se3
  lowerb4 = beta4 + qnorm(alpha/2)*se4
  upperb4 = beta4 + qnorm(1-alpha/2)*se4
  maxv = max(c(upperb1,upperb2,upperb3,upperb4))
  minv = min(c(lowerb1,lowerb2,lowerb3,lowerb4))
  plot(c(t[1],t[n]),c(minv,maxv), type="n",xlab = xlab, ylab = ylab)
  if(shadow==TRUE){
    polygon(x=c(t,t[n:1]), y=c(upperb1,lowerb1[n:1]), col = "gray90", border=NA)
    polygon(x=c(t,t[n:1]), y=c(upperb2,lowerb2[n:1]), col = "gray90", border=NA)
    polygon(x=c(t,t[n:1]), y=c(upperb3,lowerb3[n:1]), col = "gray90", border=NA)
    polygon(x=c(t,t[n:1]), y=c(upperb4,lowerb4[n:1]), col = "gray90", border=NA)
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
  lines(t, lowerb4, lty=3, col=col[4])
  lines(t, upperb4, lty=3, col=col[4])
  lines(t, beta4, lty=1, col=col[4], cex=1.2) 
}

