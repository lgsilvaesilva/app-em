#load library for multivariate normal
library(mvtnorm)
#load Old Faithful data frame
data(faithful)

#setup grid for plotting
xpts <- seq(from=1,to=6,length.out=100)
ypts <- seq(from=40,to=100,length.out=100)

#initial parameter estimates (chosen to be deliberately bad)
theta <- list(
  tau=c(0.5,0.5),
  mu1=c(2.8,75),
  mu2=c(3.6,58),
  sigma1=matrix(c(0.8,7,7,70),ncol=2),
  sigma2=matrix(c(0.8,7,7,70),ncol=2)
)

#E step: calculates conditional probabilities for latent variables
E.step <- function(theta)
  t(apply(cbind(
    theta$tau[1] * dmvnorm(faithful,mean=theta$mu1,sigma=theta$sigma1),
    theta$tau[2] * dmvnorm(faithful,mean=theta$mu2,sigma=theta$sigma2)
  ),1,function(x) x/sum(x)))
#M step: calculates the parameter estimates which maximise Q
M.step <- function(T) list(
  tau= apply(T,2,mean),
  mu1= apply(faithful,2,weighted.mean,T[,1]),
  mu2= apply(faithful,2,weighted.mean,T[,2]),
  sigma1= cov.wt(faithful,T[,1])$cov,
  sigma2= cov.wt(faithful,T[,2])$cov)


#plot initial contours
iter <- 1

#run EM and plot
cores <- colorRamp(c('red', 'steelblue'))

runa <- function(iter=30){
  for (k in 2:iter){
    T <- E.step(theta)
    theta <- M.step(T)
    mixture.contour <- outer(xpts,ypts,function(x,y) {
      theta$tau[1]*dmvnorm(cbind(x,y),mean=theta$mu1,sigma=theta$sigma1) + theta$tau[2]*dmvnorm(cbind(x,y),mean=theta$mu2,sigma=theta$sigma2)
    })
    densdf <- data.frame(expand.grid(eruptions = xpts, waiting = ypts),z = as.vector(mixture.contour))
    mydat <- cbind(faithful, prob=T[,1])  
    cp <- ggplot(densdf, aes(x = waiting, y = eruptions)) + 
      geom_contour(aes(z=z), size=.6, data=densdf) + ylim(0.5, 6) + xlim(40, 110) + guides(size=F)
    plot.cont <- cp + geom_point(aes(colour=prob), size = 5, data = mydat)  + guides(colour=F) +
      scale_colour_gradient2(low="red", high="blue", midpoint=0.5) +
      labs(x="Eruption time (mins",y="Waiting time (mins)",title="Waiting time vs Eruption time of the Old Faithful geyser")+
      theme(axis.title = element_text(, colour="black", size=18),
            title = element_text(, colour="black", size=19))
    print(plot.cont)
  }
}

library(animation)
library(ggplot2)
ani.options(interval = 0.5, outdir = file.path(getwd(), "www"))
saveMovie(runa(), , movie.name = "faithful_ggplot22.gif", interval = 0.1, nmax = 30, ani.width = 700, 
          ani.height = 600)
