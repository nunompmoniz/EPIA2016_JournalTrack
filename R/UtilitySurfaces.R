#UTILITY-SURFACE

#UTIL FOR ONE BUMP
utilOneBump <- function(form,train,test,preds,PHIs,cf,thr,makePlot) {
  
  require(akima)
  
  ph <- phi(resp(form,train),PHIs)
  ph.test <- phi(resp(form,test),PHIs)
  
  phi1 <- unique(resp(form,train)[which(ph==min(ph[ph>=thr]))])
  phi2 <- PHIs$control.pts[4]
  phi3 <- PHIs$control.pts[7]
  max.x <- ceiling(max(preds))
  max.y <- max(resp(form,test))
  
  values <- sort(unique(resp(form,test)))
  if(any(preds>max(values)*3)) {
    preds[preds>max(values)*3] <- max(values)*3
  }
  
  trues <- as.numeric(resp(form,test))
  if(any(trues>max(preds)*3)) {
    trues[trues>max(preds)*3] <- max(preds)*3
  }
  
  values.df <- data.frame(trues=trues,preds=as.numeric(preds))
  
  #############
  #FALSE POSITIVE
  
  x <- 0; y <- 0; u <- 0
  
  if(max.x > max.y) {
    
    x <- c(phi1,
           phi1,
           (phi1+phi1),
           max.x,
           max.x,
           phi1,
           (max.y/2),
           seq(max.y/2,max.x,by=1),
           seq(max.y/2,max.x,by=1))
    y <- c(phi1,
           0,
           phi1,
           phi1,
           0,
           (phi1/2),
           0,
           rep(phi1,length(seq(max.y/2,max.x,by=1))),
           rep(0,length(seq(max.y/2,max.x,by=1))))
    u <- c(thr,
           0, #or 0
           0,
           0,
           -1,
           0,
           -1,
           rep(0,length(seq(max.y/2,max.x,by=1))),
           rep(-1,length(seq(max.y/2,max.x,by=1))))    
    
  } else {
    
    x <- c(phi1,
           phi1,
           (phi1+phi1),
           max.y,
           max.y,
           phi1,
           (max.y/2),
           seq(max.y/2,max.y,by=1),
           seq(max.y/2,max.y,by=1))
    y <- c(phi1,
           0,
           phi1,
           phi1,
           0,
           (phi1/2),
           0,
           rep(phi1,length(seq(max.y/2,max.y,by=1))),
           rep(0,length(seq(max.y/2,max.y,by=1))))
    u <- c(thr,
           0, #or 0
           0,
           0,
           -1,
           0,
           -1,
           rep(0,length(seq(max.y/2,max.y,by=1))),
           rep(-1,length(seq(max.y/2,max.y,by=1))))
    
  }
    
  dat <- data.frame(x,y,u)
  dat <- unique(dat)

  x <- dat$x; y <- dat$y; u <- dat$u
  
  xo.values <- unique(sort(values.df[values.df$preds>=phi1,]$preds))
  yo.values <- unique(sort(values.df[values.df$trues<=phi1,]$trues))
  if(length(xo.values)==0) { if(max.x>max.y) { xo.values <- c(seq(phi1,max.x,by=1)) } else { xo.values <- c(seq(phi1,max.y,by=1)) } }
  if(length(yo.values)==0) { yo.values <- c(seq(0,phi1,by=1)) }
  
  fp <- interp(x,y,u,duplicate="mean",xo=xo.values,yo=yo.values)
  
  #FALSE NEGATIVE

  x <- 0; y <- 0; u <- 0

  if(max.x > max.y) {
    
    x <- c(0,
           0,
           phi1,
           phi1,
           phi1,
           (phi1/2))
    y <- c(phi1,
           max.x,
           phi1,
           max.x,
           (phi1+phi1),
           phi1)
    u <- c(0, #or ZERO
           -1,
           thr,
           0,
           0,
           0)
    
  } else {
    
    x <- c(0,
           0,
           phi1,
           phi1,
           phi1,
           (phi1/2))
    y <- c(phi1,
           max.y,
           phi1,
           max.y,
           (phi1+phi1),
           phi1)
    u <- c(0, #or ZERO
           -1,
           thr,
           0,
           0,
           0)
    
  }
  
  dat <- data.frame(x,y,u)
  dat <- unique(dat)

  x <- dat$x; y <- dat$y; u <- dat$u
  
  xo.values <- unique(sort(values.df[values.df$preds<=phi1,]$preds))
  yo.values <- unique(sort(values.df[values.df$trues>=phi1,]$trues))
  if(length(xo.values)==0) { xo.values <- seq(0,phi1,by=1) }
  if(length(yo.values)==0) { if(max.x>max.y) { yo.values <- seq(phi1,max.x,by=1) } else { yo.values <- seq(phi1,max.y,by=1) } }
  
  fn <- interp(x,y,u,duplicate="mean",xo=xo.values,yo=yo.values)
  
  #TRUE NEGATIVE
  phi_aux <- data.frame(points=resp(form,test)[which(ph.test<=thr)], p=ph.test[ph.test<=thr])
  phi_aux <- phi_aux[with(phi_aux,order(phi_aux$points)),]
  phi_aux <- unique(phi_aux)
  
  x <- c(rep(phi1,length(fp$y)),
         0,
         phi_aux$points)
  
  y <- c(fp$y,
         phi1,
         phi_aux$points)
  
  u <- c(fp$z[1,],
         0,
         phi_aux$p)
  
  dat <- data.frame(x,y,u)
  dat <- unique(dat)

  x <- dat$x; y <- dat$y; u <- dat$u
  
  xo.values <- unique(sort(values.df[values.df$preds<=phi1,]$preds))
  yo.values <- unique(sort(values.df[values.df$trues<=phi1,]$trues))
  if(length(xo.values)==0) { xo.values <- seq(0,phi1,by=1) }
  if(length(yo.values)==0) { yo.values <- seq(0,phi1,by=1) }
  
  tn <- interp(x,y,u,duplicate="mean",xo=xo.values,yo=yo.values)
  
  #TRUE POSITIVE
  phi_aux <- data.frame(points=resp(form,test)[which(ph.test>=thr)], p=ph.test[ph.test>=thr])
  phi_aux <- phi_aux[with(phi_aux,order(phi_aux$points)),]
  phi_aux <- unique(phi_aux)
  
  x <- 0; y <- 0; u <- 0
  
  if(max.x>max.y) {
    
    x <- c(rep(phi1,length(fn$y)),
           phi_aux$points,
           seq(phi3,max.x,by=1),
           max.x,
           phi1,
           (max.x/2),
           seq(max.x/2,max.x,by=1),
           (phi1+phi1))
    
    y <- c(fn$y,
           phi_aux$points,
           seq(phi3,max.x,by=1),
           phi1,
           max.x,
           phi1,
           rep(phi1,length(seq(max.x/2,max.x,by=1))),
           phi1)
    
    u <- c(fn$z[nrow(fn$z),],
           phi_aux$p,
           rep(1,length(seq(phi3,max.x,by=1))),
           0,
           0,
           0,
           rep(0,length(seq(max.x/2,max.x,by=1))),
           0)
    
    
  } else {
    
    x <- c(rep(phi1,length(fn$y)),
           phi_aux$points,
           seq(phi3,max.y,by=1),
           max.y,
           phi1,
           (max.y/2),
           seq(max.y/2,max.y,by=1),
           (phi1+phi1))
    
    y <- c(fn$y,
           phi_aux$points,
           seq(phi3,max.y,by=1),
           phi1,
           max.y,
           phi1,
           rep(phi1,length(seq(max.y/2,max.y,by=1))),
           phi1)
    
    u <- c(fn$z[nrow(fn$z),],
           phi_aux$p,
           rep(1,length(seq(phi3,max.y,by=1))),
           0,
           0,
           0,
           rep(0,length(seq(max.y/2,max.y,by=1))),
           0)
    
    
  }
  
  dat <- data.frame(x,y,u)
  
  xo.values <- unique(sort(values.df[values.df$preds>=phi1,]$preds))
  yo.values <- unique(sort(values.df[values.df$trues>=phi1,]$trues))
  if(length(xo.values)==0) { if(max.x>max.y) { xo.values <- seq(phi1,max.x,by=1) } else { xo.values <- seq(phi1,max.y,by=1) } }
  if(length(yo.values)==0) { if(max.x>max.y) { yo.values <- seq(phi1,max.x,by=1) } else { yo.values <- seq(phi1,max.y,by=1) } }
  
  dat <- unique(dat)
  x <- dat$x; y <- dat$y; u <- dat$u
  
  tp <- interp(x,y,u,duplicate="mean",xo=xo.values,yo=yo.values)
  
  if(any(is.na(tp$z[,1]))) { tp$z[,1] <- fp$z[,ncol(fp$z)] }
  
  ####
  
  tests <- fp
  tests$x <- c(tn$x,tp$x)
  tests$y <- c(tn$y,tp$y)
  tst.m <- cbind(tn$z,fn$z)
  tst.m2 <- cbind(fp$z,tp$z)
  tst <- rbind(tst.m,tst.m2)
  tests$z <- tst
  
  if(any(table(tests$x)>1)) { 
    tests$z <- tests$z[!duplicated(tests$x),]
    tests$x <- unique(tests$x)
  }
  
  if(any(table(tests$y)>1)) { 
    tests$z <- tests$z[,!duplicated(tests$y)]
    tests$y <- unique(tests$y)
  }
  
  tests$z[which(tests$z==min(tests$z))] <- -1

  if(makePlot) {
    xl = expression(hat(Y))
    yl = expression(Y)
    image.plot(tests, col=surface.colors,xlab=xl,ylab=yl,legend.shrink = 1,legend.width = 0.8,cex.axis = 1,cex.main = 1.2)
    points(preds,resp(form,test),pch=10,cex=.5)
    contour(tests,lwd=0.6,drawlabels=T,labcex=0.7,add=T,cex=0.3,levels = seq(-1,1,by=0.2), nlevels = 1)
    abline(h=PHIs$control.pts[7],lty=2)
    abline(v=PHIs$control.pts[7],lty=2)
    box()
  }
  
  #####
  values.df$Utility <- 0
  for(i in 1:nrow(values.df)) {
    values.df[i,]$Utility <- tests$z[which(tests$x==values.df[i,]$preds),which(tests$y==values.df[i,]$trues)]
  }
  
  if(any(is.na(values.df$Utility))) {
    print(values.df[is.na(values.df$Utility),])
    values.df[is.na(values.df$Utility),]$Utility <- 0
  }
  
  detach("package:akima", unload=TRUE)
  
  values.df
  
}

