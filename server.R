library(shiny)
library(ggplot2)
library(mixtools)
library(reshape)
library(grid)
library(gridExtra)

source('normalEM.R')
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
geraMistura <- function(n, mu1, mu2, sigma1, sigma2, w){
  u <- runif(n)
  x <- ifelse(u < w, rnorm(n, mean = mu1, sd = sigma1), rnorm(n, mean = mu2, sd = sigma2))
  ind <- ifelse(u < w, "Amostra 1", "Amostra 2")
  data.frame(x, ind)
}
T1.fun <- function(w1.ini, mu1.ini, sigma1.ini, mu2.ini, sigma2.ini, x){
  ## Densidade condicional da variavel latente dado os dados observados e os parametros
  ## P(Z|X, theta_t)
  out <- w1.ini*dnorm(x, mu1.ini, sigma1.ini)/(w1.ini*dnorm(x, mu1.ini, sigma1.ini) + (1-w1.ini)*dnorm(x, mu2.ini, sigma2.ini))
  return(out)
}

em <- function(x, ini) {
  ## x: A vector of length n consisting of the data
  ## ini: Initial values
  ## Algoritmo EM para mistura de duas normais
  ## Os paramentros sao os valores iniciais, depois basta iterar.
  
  w1.est  <- ini[1]
  mu1.est <- ini[2]
  mu2.est <- ini[3]
  sigma1.est <- ini[4]
  sigma2.est <- ini[5]
  
  ## Passo E:
  T1.est     <- T1.fun(w1.est, mu1.est, sigma1.est, mu2.est, sigma2.est, x)
  
  ## Passo M:
  w1.est     <- mean(T1.est)
  w2.est     <- 1 - w1.est
  mu1.est    <- sum(T1.est*x)/sum(T1.est)
  mu2.est    <- sum((1-T1.est)*x)/sum(1 - T1.est)
  sigma1.est <- sqrt( sum(T1.est*(x - mu1.est)^2)/sum(T1.est) )
  sigma2.est <- sqrt( sum((1-T1.est)*(x - mu2.est)^2)/sum(1 - T1.est) )
  
  out.est <- c(w1 = w1.est, w2 = 1-w1.est, mu1 = mu1.est, mu2 = mu2.est, sigma1 = sigma1.est, sigma2 = sigma2.est)
  
  
  loglik<- sum( log(w1.est*dnorm(x, mu1.est, sigma2.est) + w2.est*dnorm(x, mu2.est, sigma2.est)) )
  
  return(list(estimate = out.est, loglik = loglik))
  
}

iter <- function(x, ini, niter = 10) {
  ## x: A vector of length n consisting of the data
  ## ini: Initial values
  ## eps: The convergence criterion. Convergence is declared when the change in the observed data log-likelihood increases by less than epsilon.
  esti.anterior <- list()
  esti.anterior$estimate <- ini
  loglik.start <- sum( log(ini[1]*dnorm(x, ini[1], ini[4]) + (1-ini[1])*dnorm(x, ini[3], ini[5])) )
  esti.anterior$loglik <- loglik.start
  inicial <- c( w1=ini[1], w2=1-ini[1], mu1 = ini[2], mu2 = ini[3], sigma1 = ini[4], sigma2 = ini[5])
  theta.hat <- c(inicial, loglik = loglik.start)
  iter <- 0
  while( iter < niter-1){
    # while( (iter < max.iter-1) ){
    w1     = esti.anterior$estimate[1]
    mu1    = esti.anterior$estimate[3]
    mu2    = esti.anterior$estimate[4]
    sigma1 = esti.anterior$estimate[5]
    sigma2 = esti.anterior$estimate[6]
    
    esti.atual <- em(x, ini)
    
    dif <- esti.atual$loglik - esti.anterior$loglik
    
    esti.anterior <- esti.atual
    iter <- iter + 1
    theta.hat <- as.data.frame(rbind(theta.hat, c(esti.atual$estimate, loglik = esti.atual$loglik)))
  }
  theta.hat$iter <- 1:nrow(theta.hat)
  theta.hat.melt <- melt(theta.hat, id = "iter")
  out <- theta.hat.melt
  out
}


shinyServer(function(input, output) {
  
  sliderValues <- reactive({
    set.seed(10)
    amostra    <- geraMistura(n = input$n, mu1 = input$media1, mu2 = input$media2, sigma1 = input$sd1, sigma2 = input$sd2, w = input$w)
    amostra$media <- ifelse(amostra$ind == "Amostra 1", input$media1, input$media2)
    amostra
  }) 
  
  output$distPlot <- renderPlot({
    set.seed(10)
    amostra <- sliderValues()
    plotDist <- ggplot(amostra, aes(x = x, y = ..density..)) + 
      geom_histogram(binwidth=1.5, colour="#333333", fill = "white", alpha=.8, position="identity")  +
      geom_density(fill="blue", colour="black", alpha=.1) +
      xlim(mean(amostra$x)- 3*sd(amostra$x), mean(amostra$x) + 3*sd(amostra$x)) + 
      geom_vline(data = amostra, aes(xintercept=media, color=ind) , linetype="dashed", size=1) +
      geom_rug(aes(y=0, colour = factor(ind)), alpha = 0.2, sides="b") +
      guides(colour=FALSE)
    print(plotDist)
  })
  
  output$fit <- renderTable({
    set.seed(10)
    amostra <- sliderValues()
    inicial <- c(input$w0,input$m10, input$m20, input$sd10, input$sd20)
    em.fit <- normalEM(amostra$x,k=2,maxit=100,epsilon=0.01, lambda = inicial[1], mu=c(inicial[2],inicial[3]),sigma=c(inicial[4],inicial[5]))
    df <- rbind('Mean' = em.fit$mu, 'Std. Deviation' = em.fit$sigma, 'Weight' = em.fit$lambda)
    colnames(df) <- c("Normal-1", "Normal-2")
    xtable(df)
  })
  
  output$model <- renderUI({
    withMathJax(
    HTML('$$f(x|\\mu_1,\\Sigma_1,\\mu_2,\\Sigma_2)=\\left(1-\\omega\\right)\\mathcal{N}_d(x|\\mu_1,\\Sigma_1) + \\omega\\mathcal{N}_d(x|\\mu_2,\\Sigma_2)$$'))
    
  })
  
  output$nSize <- renderUI({
    withMathJax(
      sliderInput("n", h6('Sample Size'), min = 1, max = 5000, value = 500)
      )
  })
  
  output$m1 <- renderUI({
    withMathJax(
      sliderInput("media1", h6('$$\\mu_1$$'), min = -50, max = 50, value = 10)
    )
  })
  
  output$m2 <- renderUI({
    withMathJax(
      sliderInput("media2", h6('$$\\mu_2$$'), min = -50, max = 50, value = 25)
    )
  })
  
  output$sd1 <- renderUI({
    withMathJax(
      sliderInput("sd1", h6('$$\\sigma_1$$'), min = 0, max = 50, value = 3)
    )
  })
  
  output$sd2 <- renderUI({
    withMathJax(
      sliderInput("sd2", h6('$$\\sigma_2$$'), min = 0, max = 50, value = 6)
    )
  })
  
  output$omega <- renderUI({
    withMathJax(
      sliderInput("w", h6('Weight'), min = 0, max = 1, value = 0.6)
    )
  })
  ##Initial Values
  output$m10 <- renderUI({
    withMathJax(
      sliderInput("m10", h6('$$\\mu_1$$'), min = -50, max = 50, value = 15)
    )
  })
  
  output$m20 <- renderUI({
    withMathJax(
      sliderInput("m20", h6('$$\\mu_2$$'), min = -50, max = 50, value = 5)
    )
  })
  
  output$sd10 <- renderUI({
    withMathJax(
      sliderInput("sd10", h6('$$\\sigma_1$$'), min = 0, max = 50, value = 14)
    )
  })
  
  output$sd20 <- renderUI({
    withMathJax(
      sliderInput("sd20", h6('$$\\sigma_2$$'), min = 0, max = 50, value = 24)
    )
  })
  
  output$omega0 <- renderUI({
    withMathJax(
      sliderInput("w0", h6('Weight'), min = 0, max = 1, value = 0.15)
    )
  })
  
  output$iteracao <- renderUI({
    withMathJax(
      numericInput("itera", h4('Iterations'), min = 1, max = 100, value = 20, step = 1)
    )
  })
  
  iterEM <- reactive({
    amostra <- sliderValues()
    inicial <- c(input$w0,input$m10, input$m20, input$sd10, input$sd20)
    em.fit <- normalEM(amostra$x,k=2,maxit=input$itera,epsilon=0.01, lambda = inicial[1], mu=c(inicial[2],inicial[3]),sigma=c(inicial[4],inicial[5]), verb=T)
    out <- as.data.frame(do.call(rbind, em.fit[[10]]))
    out <- rbind(c(w1=inicial[1], w2=1-inicial[1], mu1=inicial[2],mu2=inicial[3],sigma1=inicial[4],sigma2=inicial[5]), out)
    out$loglik <- em.fit$all.loglik
    out$iter <- 1:nrow(out)
    out.melt <- melt(out, id = "iter")

    out.melt$real <- rep(c(input$w, 1-input$w, input$media1, input$media2, input$sd1, input$sd2, NA), each = input$itera)
    out.melt
  })
  
  output$convPlot <- renderPlot({
    out.melt <- iterEM()        
    out.melt <- subset(out.melt, variable != 'w2')
    out.melt$value<-round(out.melt$value, 2)
    convPlot <- ggplot(aes(x=iter, y=value), data=out.melt) + 
      geom_hline(aes(yintercept = real), colour = 'tomato', size=1.5, data = out.melt, alpha=0.8) +
      geom_line(data = out.melt, size=1.1, aes(group =  factor(variable)), col = 'steelblue') +
      geom_point(size=2.5, colour = 'steelblue') +
    facet_wrap(~variable, scales='free_y') + 
      theme(strip.text.x = element_text(size=12),
            strip.text.y = element_text(size=12, face="bold")
            )
    print(convPlot)
# w1.plot <- qplot(x = iter, y = value, data = subset(out.melt, variable == 'w1')) + 
#   geom_hline(aes(yintercept = real), colour='tomato', size=2.5, alpha = 0.8) +
#   geom_line(size=1.1, colour = 'steelblue')+
#   geom_point(size=2.5, colour = 'steelblue') +
#   labs(x='Iterations', y = 'Estimate', title = expression(omega[1]))
# mu1.plot <- qplot(x = iter, y = value, data = subset(out.melt, variable == 'mu1')) + 
#   geom_hline(aes(yintercept = real), colour='tomato', size=2.5, alpha = 0.8) +
#   geom_line(size=1.1, colour = 'steelblue')+
#   geom_point(size=2.5, colour = 'steelblue') +
#   labs(x='Iterations', y = 'Estimate', title = expression(mu[1]))
# mu2.plot <- qplot(x = iter, y = value, data = subset(out.melt, variable == 'mu2')) + 
#   geom_hline(aes(yintercept = real), colour='tomato', size=2.5, alpha = 0.8) +
#   geom_line(size=1.1, colour = 'steelblue')+
#   geom_point(size=2.5, colour = 'steelblue') +
#   labs(x='Iterations', y = 'Estimate', title = expression(mu[2]))
# sigma1.plot <- qplot(x = iter, y = value, data = subset(out.melt, variable == 'sigma1')) + 
#   geom_hline(aes(yintercept = real), colour='tomato', size=2.5, alpha = 0.8) +
#   geom_line(size=1.1, colour = 'steelblue')+
#   geom_point(size=2.5, colour = 'steelblue') +
#   labs(x='Iterations', y = 'Estimate', title = expression(sigma[1]))
# sigma2.plot <- qplot(x = iter, y = value, data = subset(out.melt, variable == 'sigma2')) + 
#   geom_hline(aes(yintercept = real), colour='tomato', size=2.5, alpha = 0.8) +
#   geom_line(size=1.1, colour = 'steelblue')+
#   geom_point(size=2.5, colour = 'steelblue') +
#   labs(x='Iterations', y = 'Estimate', title = expression(sigma[2]))
# 
# loglik.plot <- qplot(x = iter, y = value, data = subset(out.melt, variable == 'loglik')) + 
#   geom_line(size=1.1, colour = 'steelblue')+
#   geom_point(size=2.5, colour = 'steelblue') +
#   labs(x='Iterations', y = 'Log-Likelihood')
# 
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2, 3)))
# print(w1.plot, vp = vplayout(1, 1))
# print(mu1.plot, vp = vplayout(1, 2))
# print(mu2.plot, vp = vplayout(1, 3))
# print(sigma1.plot, vp = vplayout(2, 1))
# print(sigma2.plot, vp = vplayout(2, 2))
# print(loglik.plot, vp = vplayout(2, 3))
  })
  
})

