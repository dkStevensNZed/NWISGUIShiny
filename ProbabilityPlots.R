summry <- function(x) {

  ind = which(!is.na(x))
  x.s = summary(x[ind])
  x.sd = sd(x[ind])
  x.n = length(x[ind])
  x.cv = x.sd/mean(x[ind])
  c(nObs = x.n,x.s,sd=x.sd,cv=x.cv)
  
}

getSeason <- function(d) {
  s = rep(0,length(d))
  for (i in 1:length(d)) {
    yd = as.POSIXlt(d[i])$yday
    if(between2(yd,  0, 89,TRUE)) s[i] = 1
    if(between2(yd, 90,180,TRUE)) s[i] = 2
    if(between2(yd,181,272,TRUE)) s[i] = 3
    if(between2(yd,273,365,TRUE)) s[i] = 4
  }
  s
}

getSeason.2 <- function(d,flgAsLabel=F) {
  iSn <- ceiling((as.POSIXlt(d)$mon + 1)/3)
  sLab <- c('Winter','Spring','Summer','Fall')
  if(flgAsLabel) 
    return(sLab[iSn])
  else
    return(iSn)
}

between2a <- function(x,x1,x2,flgIncl=T) {
  if(flgIncl) {
    b = x <= x2 & x >= x1
  }
  else {
    b = x < x2 & x > x1
  }
  b
}

between2 <- function(x,x1,x2,flgInclL=T,flgInclU=F) {
  
  if(flgInclL & flgInclU)
    b = x <= x2 & x >= x1
  else if (flgInclL)
    b = x <= x2 & x > x1
  else if (flgInclU)
    b = x < x2 & x >= x1
  else
    b = x < x2 & x > x1
  
  return(b)
}


ties <- function(x,rm.na=T) {
  x.s <- sort(x)
    #[!is.na(x)]
  x.r <- rank(x.s,ties.method='average')
  x.ties <- which(diff(sort(x.r))==0)
  x.t <- lapply(x.ties,function(t.x) {
    cat(t.x,': ',x.s[t.x],'\n')
    which(x == x.s[t.x])
  })
  return(list(rank=x.r,ties=unique(x.t)))
}

pPlotxy <- function (x.o, qdist = qnorm, probs = NULL, line = FALSE, xlab = NULL,
    ylab = "Frequency of observations less than, %", xTicks = NULL, xTickLabels = NULL,
    main = NULL, grid = FALSE, fgrid = FALSE, xfgrid = FALSE, xLim = NULL, yLim=NULL,
    Log = '', col='black', cInd = NULL, cex = 1, lwd = 1, addData=F,addMedian=F, 
    cex.axis=1,cex.lab=1,ax.fac=1,pSize='large',...)
{
    DOTARGS <- as.list(substitute(list(...)))[-1]
    DOTARGS <- paste(names(DOTARGS), DOTARGS, sep = "=", collapse = ", ")
 #   xlab = deparse(substitute(x))
     
    x <- sort(x.o)
    QNAME <- deparse(substitute(qdist))
    DOTS <- list(...)
    qdist <- match.fun(qdist)
#    print(DOTS)
    QFUN <- function(p) {
        args = DOTS
        args$p = p
        do.call("qdist", args)
    }
    y <- QFUN(ppoints(length(x))); ny <- length(y)
#    if(is.null(yLim)) yLim <- c(qnorm(.01),qnorm(.99))
    if(is.null(yLim)) yLim <- c(qnorm(1/ny),qnorm((ny-1)/ny))
    if (is.null(probs)) {
      probs <- c(0.01,0.02, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.98, 0.99)
      # if (length(x) >= 1000)
      #   probs <- c(0.001, probs, 0.999)
      if(max(yLim)> 3.9)
        probs <- c(.00001,.0001,.0005,0.001,.005, probs,0.995, 0.999,0.9995,0.9999,0.99999)
      else if(max(yLim) > 3.7)
        probs <- c(.0001,.0005,0.001,.005, probs,0.995, 0.999,0.9995,0.9999)
      else if(max(yLim) > 3.1)
        probs <- c(.0005,0.001,.005, probs,0.995, 0.999,0.9995)
      else if(max(yLim) > 2.3)
        probs <- c(0.001,.005, probs,0.995, 0.999)
    }

    qprobs <- QFUN(probs)
    if(is.null(xLim)) xLim <- range(x)
#    if(Log=='x') xLim <- range(x)
    if(!addData) {
    xaxt <- 'n'
    if(is.null(xTicks)) {
      if(Log == 'x') {
        x.exp <- floor(min(range(log10(xLim)))):ceiling(max(range(log10(xLim))))
        x.tck <- sort(as.vector(outer(c(1),10^x.exp)))
        x.lab <- parse(text=paste('10^',x.exp,sep=''))                   
        xaxt <- 'n'
      } else {
        if(is.null(xLim)) {
          xLim = pretty(x)
        }
        xTicks = pretty(xLim)        
      }
    }
#    cat('pSize: ',pSize,'\n')
    yLim <- yLim; if(is.null(yLim)) yLim = range(c(y, qprobs))
    cex.main <- cex.axis*0.8
    plot(x, y, axes = FALSE, type = "n",xaxt=xaxt,
        xlim = xLim, ylim=yLim,xlab = xlab, ylab = ylab, main=main, log=Log,cex.axis=cex.axis,
        cex.lab=cex.lab,cex.main=cex.main)

   if(Log=='x') {
      x.tck <- 10^x.exp
      x.lab <- parse(text=paste('10^',x.exp,sep=''))
      if(grid) abline(h = qprobs, v = x.tck, col = "grey90", lty=1,lwd=2)
      if(fgrid) abline(v = as.vector((outer(c(2,5),x.tck))), col = "grey90", lty=3,lwd=1)
      if(xfgrid) abline(v = as.vector((outer(c(2:9),x.tck))), col = "grey90", lty=3,lwd=1)
      padj <- -2; if(pSize=='small') padj <- -1.5
      axis(1, at=(x.tck), labels = x.lab, tcl=.35,cex.axis=cex.axis*ax.fac)
      axis(1,at=as.vector((outer(c(2,5),x.tck))),labels=rep(c(2,5),times=length(x.exp)),
        tcl=.25,cex.axis=cex.axis,padj=padj,cex.axis=cex.axis*ax.fac*.8)     

    } else {
      if(grid) abline(h = qprobs, v = xTicks, col = "grey90", lty=2)
      axis(1, at = xTicks, labels = xTickLabels, tcl=.35,cex.axis=cex.axis*ax.fac) 
    }
    axis(2, at = qprobs, labels = 100 * probs, las = 1,
      tcl = .35, cex.axis = cex.axis*ax.fac)

    QTEXT <- paste("Quantile: ", QNAME, sep = "")
    if (nchar(DOTARGS))
        QTEXT <- paste(QTEXT, DOTARGS, sep = ", ")
#    mtext(QTEXT, side = 1, line = 3, adj = 1)
    }
#    if(length(cInd) == 0) 
    cInd = 1:length(x)
#    browser()
    x.lm <- sort(x.o[cInd])
    y.lm <- QFUN(ppoints(length(x.lm)))
    pp.lm = lm(y.lm[cInd]~x.lm[cInd])
    slope <- pp.lm$coefs[1]   #diff(yl)/diff(xl)
    int <- 0  #pp.lm$coefs[2]     #yl[1] - slope * xl[1]
    if (line) {
      abline(pp.lm, col = "red", lwd = lwd)
    }

    points(x,y,cex=cex,pch=19,col=col)
    if(addMedian) ablineclip(v=median(x),col=col,y2=0)
    box()    

#    z <- list(qdist = QFUN, int = int, slope = slope)
    z <- list(qdist = QFUN, int = int, slope = slope, x=x, y=y)
    class(z) <- "probplot"
    invisible(z)
}

pPlotxyInset <- function (x.o, qdist = qnorm, probs = NULL, line = FALSE, xlab = NULL,
    ylab = "Frequency of observations less than, %", xTicks = NULL, xTickLabels = NULL,
    main = NULL, grid = FALSE, xLim = NULL, Log = '', col=NULL,
    cInd = NA, cex = 1, lwd = 1, addData=F, ...)
{
    DOTARGS <- as.list(substitute(list(...)))[-1]
    DOTARGS <- paste(names(DOTARGS), DOTARGS, sep = "=", collapse = ", ")
 #   xlab = deparse(substitute(x))

    x <- sort(x.o)
    QNAME <- deparse(substitute(qdist))
    DOTS <- list(...)
    qdist <- match.fun(qdist)
    cat(DOTS,'\n')
    QFUN <- function(p) {
        args = DOTS
        args$p = p
        do.call("qdist", args)
    }
    y <- QFUN(ppoints(length(x)))
    
    if (is.null(probs)) {
        probs <- c(0.01, 0.05, seq(0.1, 0.9, by = 0.1), 0.95,
            0.99)
        if (length(x) >= 1000)
            probs <- c(0.001, probs, 0.999)
    }
    qprobs <- QFUN(probs)
    if(! addData) {
     plot(x, y, axes = FALSE, type = "n", ylim = range(c(y, qprobs)),
        xlim = xLim, xlab = xlab, ylab = ylab, main=main, log=Log)
    box()
    if(is.null(xTicks)) {
      if(is.null(xLim)) {
        xLim = pretty(x)
      }
      xTicks = pretty(xLim)
    }
    if(grid) abline(h = qprobs, v = xTicks, col = "lightgrey", lty=2)
    axis(1, at = xTicks, labels = xTickLabels, tcl=.35)
    axis(2, at = qprobs, labels = 100 * probs, las = 1,
      tcl = .35, cex.axis = .85)

    QTEXT <- paste("Quantile: ", QNAME, sep = "")
    if (nchar(DOTARGS))
        QTEXT <- paste(QTEXT, DOTARGS, sep = ", ")
#    mtext(QTEXT, side = 1, line = 3, adj = 1)
    if(length(cInd) == 0) cInd = 1:length(x)
    x.lm <- sort(x.o[cInd])
    y.lm <- QFUN(ppoints(length(x.lm)))
    pp.lm = lm(y.lm[cInd]~x.lm[cInd])
    slope <- pp.lm$coefs[1]   #diff(yl)/diff(xl)
    int <- pp.lm$coefs[2]     #yl[1] - slope * xl[1]
    if (line) {
      abline(pp.lm, col = "red", lwd = lwd)
    }
    }
    
    points(x,y,cex=cex,pch=19,col=col)
    z <- list(qdist = QFUN, int = int, slope = slope)
    class(z) <- "probplot"
    invisible(z)
}

getLogGridTicks <- function(lims,Grid) {

  l.n <- min(lims); l.x <- max(lims)
  x.n <- floor(log10(l.n))
  x.x <- ceiling(log10(l.x))
  tcks <- NULL
  
  if(Grid == 'f') {     # fine grid - lines at each .1,.2,.3,.4, etc.
    f.g <- c(1.5,2,3,4,6,7,8,9)
    for(i in x.n:(x.x-1)) {
      tcks <- c(tcks,f.g*10^i)
    }  
  } else if(Grid == 'm') {
    f.g <- c(1.5,3.5,7.5)
    for(i in x.n:(x.x-1)) {
      tcks <- c(tcks,f.g*10^i)
    }  
  }

  tcks <- tcks[which(tcks <= 10^x.x*1.1)]
  tcks 
}


getLogTicks <- function(lims) {
  l.n <- min(lims); l.x <- max(lims)
  x.n <- floor(log10(l.n))
  x.x <- ceiling(log10(l.x))
  if(x.x - x.n >= 2) {
    tck <- c(1,2,5)
    tcks <- NULL
    for(i in x.n:(x.x-1)) {
      tcks <- c(tcks,tck*10^i)
    }
  } else {
    tcks <- seq(10^x.n,10^x.x,by=10^x.n)
  }
#  cat(10^x.x*1.1,'\n')
  tcks <- tcks[which(tcks <= 10^x.x*1.1)]
  tcks 
}

addBentLine <- function(x,y,hLine=NA,vLine=NA,bend=F,pLabel) {

  u <- par('usr')
  if(!is.na(vLine)) {
    x.app = approxfun(x,y)      
    for(vL in vLine) {
      ly = x.app(vL)      
      if(bend) {
        dy = u[4] - u[3]
        x0=c(u[1],vL); y0=c(ly,ly); x1=c(vL,vL);
        y1=c(ly,u[3]); if(Log=='y') y1[2] <- 10^y1[2]
        segments(x0=x0,y0=y0,x1=x1,y1=y1,lty=1,lwd=2,col='red')
        lb = paste("italic(p) == '",format(pnorm(vL)*100,digits=3),"' *'%'",sep='')
        if(pLabel) {
          require(plotrix)
          ly <- u[3] + dy/10
          if(Log=='y') ly <- 10^ly
          #boxedtext(lx,ly,pos=4,l=parse(text=lb),cex=.75,col='red',offset=.5)
          par.xpd <- par('xpd'); par(xpd=T)
          boxed.labels(vL+.3,ly,labels=parse(text=lb),cex=.75,adj=.5,ylog=T,
            border=F,bg=rgb(.9,.9,.9,.5),xpad=1.3,ypad=2.3)
          par(xpd=par.xpd)
        }
      }
    }
  }
  
  if(!is.na(hLine[1])) {
    y.app = approxfun(y,x)      
    for(hL in hLine) {
      lx = y.app(hL$y)
      abline(h=hL$y,col=hL$col,lwd=hL$lwd,lty=hL$lty)
      if(bend) {
        dy = u[4] - u[3]
        x0=c(u[1],lx); y0=c(hL$y,hL$y); x1=c(lx,lx);
        y1=c(hL$y,u[3]); if(Log=='y') y1[2] <- 10^y1[2]
        segments(x0=x0,y0=y0,x1=x1,y1=y1,lty=1,lwd=2,col='red')
        lb = paste("italic(p) == '",format(pnorm(lx)*100,digits=3),"' *'%'",sep='')
        if(pLabel) {
          require(plotrix)
          ly <- u[3] + dy/10
          if(Log=='y') ly <- 10^ly
          par.xpd <- par('xpd'); par(xpd=T)
          boxedtext(lx,ly,pos=4,l=parse(text=lb),cex=.75,offset=.5)
          par(xpd=par.xpd)
        }
      }
    }
  }   
}

pPlotyx = function (x, qdist = qnorm, probs = NULL, line = TRUE, ylab = NA,
    xlab = "Probability in %", xTicks = NULL, xTickLabels = NULL, bg ='white',
    main = NULL, grids = FALSE, ygrid=FALSE, xLim = NULL, Log = "", col='black',
    cInd = NULL, extend=FALSE, vLine = NA, hLine = NA, bend = FALSE, cex = 1,
    pbgColor = NA,cex.axis=.8, cex.main=0.9, cex.lab=0.9, lwd=1, pLabel = TRUE, medGrid = FALSE, 
    fineGrid = FALSE, showCI = F, showIQR=F, showYasPower = F, 
    showReturnPeriod = F ,rightAxis=F, flgPar=F, gridCol = 'lightgrey',
    cens = NA, showCens = T, addData=F,...)
    
{
    QNAME <- deparse(substitute(qdist))
    DOTS <- list(...)
    qdist <- match.fun(qdist)
    QFUN <- function(p) {
      args = DOTS
      args$p = p
      do.call("qdist", args)
    }
    
    if(flgPar) par(mar=c(5.1,5.1,2.1,1.1))
    DOTARGS <- as.list(substitute(list(...)))[-1]
    DOTARGS <- paste(names(DOTARGS), DOTARGS, sep = "=", collapse = ", ")
#    xlab = deparse(substitute(x))
    if(!is.na(cens[1])) {
      x.df <- data.frame(x=x,cens=cens)
      x.df <- x.df[!is.na(x.df$x),]
      y.df <- x.df[order(x.df$x),]
      y <- y.df$x
      cens <- x.df$cens
      x <- QFUN(ppoints(length(y.df$x)))
    } else {
      x <- x[!is.na(x)]
      x.df <- data.frame(x=x,cens=cens)
      x.df <- x.df[!is.na(x.df$x),]
      y.df <- x.df[order(x.df$x),]
      y <- y.df$x
      y <- sort(y)
      x <- QFUN(ppoints(y))
    }
#    x <- QFUN(ppoints(length(y.df$x)))
    if (is.null(probs)) {
      probs <- c(0.01, 0.02, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.98, 0.99)
      if (length(x) >= 1000 | extend)
        probs <- c(0.001,.005, probs, 0.995, 0.999)
    }
    qprobs <<- QFUN(probs)
    ylim <- range(y)
    if(!is.null(xLim)) ylim <- xLim
    if(flgPar & showReturnPeriod) {mars <- par('mar');mars[1] <- 6.1;par(mar=mars)}
    if(flgPar & rightAxis) {mars <- par('mar'); mars[4] <- 5.1; par(mar=mars)}
    cex.points = 1  #max(.25,cex/length(x)^(1/6))
    if(length(x) < 60) cex.points <- 1.25

    if(addData) {
      cenInd <- 1:nrow(y.df)
      if(!showCens) cenInd <- which(!y.df$cens)
      points(y[cenInd]~x[cenInd],pch=19,col=col,cex=cex.points)
        if(!is.na(cens[1])) cInd <- cenInd
        if(length(cInd) == 0) cInd = 1:length(x)
        xr = x[cInd]
        pp.lm = lm(y[cInd]~xr)
        if(Log=='y') pp.lm <- lm(log10(y[cInd])~xr)
        addBentLine(x,y,hLine=hLine,vLine=vLine,bend=bend,pLabel=pLabel)
        yl <- quantile(y, c(0.25, 0.75))
        xl <- qdist(c(0.25, 0.75), ...)
        slope <- diff(yl)/diff(xl)
        if(Log=='y') slope <- diff(log10(yl))/diff(xl)
    
        int <- yl[1] - slope * xl[1]
        if(Log=='y') int <- log10(yl[1]) - slope*xl[1]
        if (line) {         
          abline(pp.lm,lwd=lwd,col=col)
          new <- data.frame(x=xr)
          y.p <- predict(pp.lm,new=new,interval='confidence')
          if(showCI) {
            if(Log=='y') {
              lines(10^(y.p[,2])~new$x,col='lightblue',lwd=2)
              lines(10^(y.p[,3])~new$x,col='lightblue',lwd=2)        
            } else {
              lines(y.p[,2]~new$x,col='lightblue',lwd=2)
              lines(y.p[,3]~new$x,col='lightblue',lwd=2)
            }
          }
          if(showIQR) abline(int, slope, col = col, lwd = lwd)      
        }          
      return()
    }
    
    pp = plot(y~x,axes=F,xlab='',type='n',main=main,ylab='',pch=19,
      xlim = range(c(x, qprobs)),ylim = ylim, log=Log, cex.axis=cex.axis,cex.lab=cex.lab,
      cex.main=cex.main,bg=bg)
    if(!is.na(pbgColor)) {
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
        col = pbgColor)
    }
    box()
    
    pInd = 1:length(probs)
    dfx = par()$fig[2] - par()$fig[1]
    if(dfx <= 0.75) {
#    if(any(par()$fig %in% c(-1,1.1) == FALSE)) {
      xTickLabels=FALSE
 #     cex.points=.5

      if(length(x) < 60) cex.points <- 1.25
      if(length(x) < 1000) {
        pInd <- 1:15 
#        pInd = c(1,4,8,12,15)
      }
      else{
        pInd = pInd
  #      cex.points = 0.25
      }
    }
    xTicks = pretty(ylim)
    if(Log=="y") xTicks <- getLogTicks(ylim) 
    prbs = rep(NA,length(probs))
    prbs[pInd] = probs[pInd]
 
    u = par('usr')
    u.g = pretty(u[3:4])
    dy = diff(u.g)[1]

    if(medGrid) {
       fprobs<- c(.5,5,15,25,35,45)/100
       fprobs <- c(fprobs,1-fprobs)       
       abline(v=qnorm(fprobs), col=gridCol,lty=1, lwd=0.8)
       y.g = seq(u.g[1],u.g[length(u.g)],dy/4)
       if(Log=='y') {
         y.g <- getLogGridTicks(ylim,Grid='m')
       }
       abline(h=y.g,col=gridCol,lty=1,lwd=0.8)
       if(Log == 'y') {
         y.g = seq(u.g[1],u.g[length(u.g)],dy/2)      
       } else {
         y.g = seq(u.g[1],u.g[length(u.g)],dy/2)             
         abline(h=y.g,col=gridCol,lty=1,lwd=2)           
       }           
    }
    if(fineGrid) {
       fprobs<- c(seq(.01,.09,.01),seq(0.2,0.9,0.1),seq(1.2,1.8,0.2),
       seq(2,4),seq(6,9),seq(12,18,2),seq(22,28,2),seq(32,38,2),seq(42,48,2))/100
       fprobs <- c(fprobs,1-fprobs)       
       abline(v=qnorm(fprobs), col=gridCol,lty=1, lwd=0.8)
       if(Log=='y') {
         y.g <- getLogGridTicks(ylim,Grid='f')
       }
       abline(h=y.g,col=gridCol,lty=1,lwd=0.8)           
       if(Log == 'y') {
         y.g = seq(u.g[1],u.g[length(u.g)],dy/2)      
       } else {
         y.g = seq(u.g[1],u.g[length(u.g)],dy/2)             
         abline(h=y.g,col=gridCol,lty=1,lwd=2)           
       }              
    }
    if(grids) abline(v = qprobs, h=xTicks, col = gridCol, lty=1, lwd=2) 

    if(Log=='y') {
      y.l = as.character(xTicks)
      if(showYasPower) {
        x.l <- 10^unique(floor(log10(xTicks)))
        y.l[xTicks %in% x.l] = paste('10^',unique(floor(log10(xTicks))),sep='')
        y.l[!(xTicks %in% x.l)] = c('2','5')
      }
      if(rightAxis) axis(4, at = xTicks, labels = parse(text=y.l), tcl=.35,las=1, 
        cex.axis=cex.axis)
      axis(2, at = xTicks, labels = parse(text=y.l), tcl=.35,las=1, cex.axis=cex.axis) 
    } else {
      if(rightAxis) axis(4, at = xTicks, labels = TRUE, tcl=.35,las=1, 
        cex.axis=cex.axis)
      axis(2, at = xTicks, labels = TRUE, tcl=.35,las=1, cex.axis=cex.axis)
    }
    axis(1, at = qprobs, labels = 100 * prbs,
      las=1,tcl = .35, cex.axis = cex.axis, padj = -.5, srt=30)
    mtext(text=xlab,side=1,line=2.5); mtext(text=ylab,side=2,line=4)
    u <- par('usr')
    if(showReturnPeriod) {
      rp <- c(1.01,2,5,10,25,50,100,250,500,1000)
      qp = QFUN(1-1/rp)
      axis(1, at = u[c(1,2)], labels=F, tcl=.25, line=3.3, lwd=2)
       
      axis(1, at = qp, labels = rp, line=3.3,
        las=1,tcl = .25, cex.axis = cex.axis*.6, padj = -.5, srt=30)
      mtext(text='Return period (d)',side=1,line=4.4)    
    }      
   # text(qprobs[pInd],par('usr')[3], labels = 100*prbs[pInd], srt = 0, adj = c(0,-3), 
   #     xpd = TRUE, cex=.65, pos=1)
    cenInd <- 1:nrow(y.df)
    if(!showCens)cenInd <- which(!y.df$cens)
    points(y[cenInd]~x[cenInd],pch=19,col=col,cex=cex.points)
    par.usr = par('usr')
    addBentLine(x,y,hLine=hLine,vLine=vLine,bend=bend,pLabel=pLabel)
        
    QTEXT <- paste("Quantile: ", QNAME, sep = "")
    if (nchar(DOTARGS))
        QTEXT <- paste(QTEXT, DOTARGS, sep = ", ")
    #mtext(QTEXT, side = 1, line = 3, adj = 1)
    if(!is.na(cens[1])) cInd <- cenInd
    if(length(cInd) == 0) cInd = 1:length(x)
    xr = x[cInd]
    pp.lm = lm(y[cInd]~xr)
    if(Log=='y') pp.lm <- lm(log10(y[cInd])~xr)

    yl <- quantile(y, c(0.25, 0.75))
    xl <- qdist(c(0.25, 0.75), ...)
    slope <- diff(yl)/diff(xl)
    if(Log=='y') slope <- diff(log10(yl))/diff(xl)

    int <- yl[1] - slope * xl[1]
    if(Log=='y') int <- log10(yl[1]) - slope*xl[1]
    if (line) {         
      abline(pp.lm,lwd=lwd,col=col)
#      new <- data.frame(x=seq(-3,3,length.out=length(xr)))
      new <- data.frame(x=xr)
      y.p <- predict(pp.lm,new=new,interval='confidence')
      if(showCI) {
        if(Log=='y') {
          lines(10^(y.p[,2])~new$x,col='lightblue',lwd=2)
          lines(10^(y.p[,3])~new$x,col='lightblue',lwd=2)        
        } else {
          lines(y.p[,2]~new$x,col='lightblue',lwd=2)
          lines(y.p[,3]~new$x,col='lightblue',lwd=2)
        }
      }
      if(showIQR) abline(int, slope, col = "red", lwd = lwd)      
    }
    points(x[cenInd],y[cenInd],cex=cex.points,pch=19,col=col)
    box(lwd=lwd)
    
    z <- list(qdist = QFUN, int = int, slope = slope, x=x, y=y)
    class(z) <- "probplot"
    invisible(z)
}

pPlotyxMTB = function (x, qdist = qnorm, probs = NULL, line = TRUE, ylab = NA,
    xlab = "Probability in %", xTicks = NULL, xTickLabels = NULL, bg ='white',
    main = NULL, grids = FALSE, ygrid=FALSE, xLim = NULL, Log = "", col='black',
    cInd = NULL, extend=FALSE, vLine = NA, hLine = NA, bend = FALSE, cex = 1,
    pbgColor = NA,cex.axis=.8, lwd=1, pLabel = TRUE, medGrid = FALSE, 
    fineGrid = FALSE, showCI = F, showPI = F, showIQR=F, showYasPower = F, 
    showReturnPeriod = F ,rightAxis=F, flgMTB = F, gridCol = 'lightgrey',...)
{
    gLty <- 1; gLwd <- 0.8; gLwdMain <- 2
    if(flgMTB) {
      par(mar=c(3.6,4.1,2.1,8.1),bg='seashell1')   #bg='palegoldenrod')        
      gLty = 2; gLwd = .8; gLwdMain = gLwd
      if(is.na(pbgColor)) pbgColor <- 'white'
    } else {
      par(mar=c(3.6,4.1,2.1,1.1),bg='white')    
    }
    DOTARGS <- as.list(substitute(list(...)))[-1]
    DOTARGS <- paste(names(DOTARGS), DOTARGS, sep = "=", collapse = ", ")
#    xlab = deparse(substitute(x))
    x <- x[!is.na(x)]                                        
    y <- sort(x)
    QNAME <- deparse(substitute(qdist))
    DOTS <- list(...)
    qdist <- match.fun(qdist)
    QFUN <- function(p) {
      args = DOTS
      args$p = p
      do.call("qdist", args)
    }
    x <- QFUN(ppoints(length(x)))
    if (is.null(probs)) {
      probs <- c(0.01, 0.02, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.98, 0.99)
      if (length(x) >= 1000 | extend)
        probs <- c(0.001, probs, 0.999)
    }
    qprobs <<- QFUN(probs)
    if(!is.null(xLim)) ylim <- xLim
    if(showReturnPeriod) {mars <- par('mar');mars[1] <- 6.1;par(mar=mars)}
    if(rightAxis) {mars <- par('mar'); mars[4] <- 5.1; par(mar=mars)}
    pp = plot(y~x,axes=F,xlab='', type='n',main=main,ylab='',pch=19,
      xlim = range(c(x, qprobs)),ylim = ylim, log=Log, cex.axis=cex.axis,bg=bg)
    if(!is.na(pbgColor)) {
      if(Log=='y') {
        rect(par("usr")[1], 10^par("usr")[3], par("usr")[2],10^ par("usr")[4], 
          col = pbgColor)     
      } else {
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
          col = pbgColor)
      }
    }
    box()
    cex.points = max(.25,cex/length(x)^(1/6))
    pInd = 1:length(probs)
    dfx = par()$fig[2] - par()$fig[1]
    if(dfx <= 0.75) {
#    if(any(par()$fig %in% c(-1,1.1) == FALSE)) {
      xTickLabels=FALSE
 #     cex.points=.5

      if(length(x) < 60) cex.points <- 1.25
      if(length(x) < 1000) {
        pInd <- 1:15 
#        pInd = c(1,4,8,12,15)
      }
      else{
        pInd = pInd
  #      cex.points = 0.25
      }
    }
    xTicks = pretty(ylim)
    if(Log=="y") xTicks <- getLogTicks(ylim) 
    prbs = rep(NA,length(probs))
    prbs[pInd] = probs[pInd]
 
    u = par('usr')
    u.g = pretty(u[3:4])
    dy = diff(u.g)[1]

    if(medGrid) {
       fprobs<- c(.5,5,15,25,35,45)/100
       fprobs <- c(fprobs,1-fprobs)       
       abline(v=qnorm(fprobs), col=gridCol,lty=gLty, lwd=gLwd)
       y.g = seq(u.g[1],u.g[length(u.g)],dy/4)
       if(Log=='y') {
         y.g <- getLogGridTicks(ylim,Grid='m')
       }
       abline(h=y.g,col=gridCol,lty=gLty,lwd=gLwd)
       if(Log == 'y') {
         y.g = seq(u.g[1],u.g[length(u.g)],dy/2)      
       } else {
         y.g = seq(u.g[1],u.g[length(u.g)],dy/2)             
         abline(h=y.g,col=gridCol,lty=gLty,lwd=gLwdMain)           
       }           
    }
    if(fineGrid) {
       fprobs<- c(seq(.01,.09,.01),seq(0.2,0.9,0.1),seq(1.2,1.8,0.2),
       seq(2,4),seq(6,9),seq(12,18,2),seq(22,28,2),seq(32,38,2),seq(42,48,2))/100
       fprobs <- c(fprobs,1-fprobs)       
       abline(v=qnorm(fprobs), col=gridCol,lty=gLty, lwd=gLwd)
       if(Log=='y') {
         y.g <- getLogGridTicks(ylim,Grid='f')
       }
       abline(h=y.g,col=gridCol,lty=gLty,lwd=gLwd)           
       if(Log == 'y') {
         y.g = seq(u.g[1],u.g[length(u.g)],dy/2)      
       } else {
         y.g = seq(u.g[1],u.g[length(u.g)],dy/2)             
         abline(h=y.g,col=gridCol,lty=gLty,lwd=gLwdMain)           
       }              
    }
    if(grids) abline(v = qprobs, h=xTicks, col = gridCol, lty=gLty, lwd=gLwdMain) 

    if(Log=='y') {
      y.l = as.character(xTicks)
      if(showYasPower) {
        x.l <- 10^unique(floor(log10(xTicks)))
        y.l[xTicks %in% x.l] = paste('10^',unique(floor(log10(xTicks))),sep='')
        y.l[!(xTicks %in% x.l)] = c('2','5')
      }
      if(rightAxis) axis(4, at = xTicks, labels = parse(text=y.l), tcl=.35,las=1, 
        cex.axis=cex.axis)
      axis(2, at = xTicks, labels = parse(text=y.l), tcl=.35,las=1, cex.axis=cex.axis) 
    } else {
      if(rightAxis) axis(4, at = xTicks, labels = TRUE, tcl=.35,las=1, 
        cex.axis=cex.axis)
      axis(2, at = xTicks, labels = TRUE, tcl=.35,las=1, cex.axis=cex.axis)
    }
    axis(1, at = qprobs, labels = 100 * prbs,
      las=1,tcl = .35, cex.axis = cex.axis, padj = -.5, srt=30)
    mtext(text=xlab,side=1,line=2.5); mtext(text=ylab,side=2,line=3)
    u <- par('usr')
    if(showReturnPeriod) {
      rp <- c(1.01,2,5,10,25,50,100,250,500,1000)
      qp = QFUN(1-1/rp)
      axis(1, at = u[c(1,2)], labels=F, tcl=.25, line=3.3, lwd=2)
       
      axis(1, at = qp, labels = rp, line=3.3,
        las=1,tcl = .25, cex.axis = cex.axis*.6, padj = -.5, srt=30)
      mtext(text='Return period (d)',side=1,line=4.4)    
    }      
   # text(qprobs[pInd],par('usr')[3], labels = 100*prbs[pInd], srt = 0, adj = c(0,-3), 
   #     xpd = TRUE, cex=.65, pos=1)
    
    points(y~x,pch=19,col=col,cex=cex.points)
    par.usr = par('usr')
    if(!is.na(vLine)) {
      x.app = approxfun(y,x)      
      for(vL in vLine) {
        lx = x.app(vLine)      
        if(bend) {
          dy = par.usr[4] - par.usr[3]
          x0=c(par.usr[1],lx); y0=c(vL,vL); x1=c(lx,lx);
          y1=c(vL,par.usr[3]); if(Log=='y') y1[2] <- 10^y1[2]
          segments(x0=x0,y0=y0,x1=x1,y1=y1,lty=1,lwd=2,col='red')
          lb = paste("italic(p) == '",format(pnorm(lx)*100,digits=3),"' *'%'",sep='')
          if(pLabel) {
            require(plotrix)
            ly <- par.usr[3] + dy/10
            if(Log=='y') ly <- 10^ly
            #boxedtext(lx,ly,pos=4,l=parse(text=lb),cex=.75,col='red',offset=.5)
            boxed.labels(lx+.4,ly,labels=parse(text=lb),cex=.75,adj=.5,ylog=T,
              border=F,bg=rgb(.9,.9,.9,.5),xpad=1.3,ypad=2.3)
          }
        }
      }
    }
    
    if(!is.na(hLine[1])) {
      y.app = approxfun(x,y)      
      for(hL in hLine) {
        lx = y.app(hLine)
        abline(h=hL,col='blue',lwd=2,lty=1)
#        if(bend) {
#          dy = par.usr[4] - par.usr[3]
#          x0=c(par.usr[1],lx); y0=c(vL,vL); x1=c(lx,lx);
#          y1=c(vL,par.usr[3]); if(Log=='y') y1[2] <- 10^y1[2]
#          segments(x0=x0,y0=y0,x1=x1,y1=y1,lty=1,lwd=2,col='red')
#          lb = paste("italic(p) == '",format(pnorm(lx)*100,digits=3),"' *'%'",sep='')
#          if(pLabel) {
#            require(plotrix)
#            ly <- par.usr[3] + dy/10
#            if(Log=='y') ly <- 10^ly
#            boxedtext(lx,ly,pos=4,l=parse(text=lb),cex=.75,offset=.5)
#          }
#        }
      }
    }
        
    QTEXT <- paste("Quantile: ", QNAME, sep = "")
    if (nchar(DOTARGS))
        QTEXT <- paste(QTEXT, DOTARGS, sep = ", ")
    #mtext(QTEXT, side = 1, line = 3, adj = 1)
    if(length(cInd) == 0) cInd = 1:length(x)
    xr = x[cInd]
    pp.lm = lm(y[cInd]~xr)
    if(Log=='y') pp.lm <- lm(log10(y[cInd])~xr)
    yl <- quantile(y, c(0.25, 0.75))
    xl <- qdist(c(0.25, 0.75), ...)
    slope <- diff(yl)/diff(xl)
    if(Log=='y') slope <- diff(log10(yl))/diff(xl)

    int <- yl[1] - slope * xl[1]
    if(Log=='y') int <- log10(yl[1]) - slope*xl[1]
    if (line) {         
      abline(pp.lm,lwd=lwd)
      y.p <- predict(pp.lm,interval=c('prediction'))
      y.c <- predict(pp.lm,interval=c('confidence'))
      if(showCI) {
        if(Log=='y') {
          lines(10^y.c[,2]~x,col='lightblue',lwd=2)
          lines(10^y.c[,3]~x,col='lightblue',lwd=2)        
        } else {
          lines(y.c[,2]~x,col='lightblue',lwd=2)
          lines(y.c[,3]~x,col='lightblue',lwd=2)
        }
      }
      if(showPI) {
        if(Log=='y') {
          lines(10^y.p[,2]~x,col='lightblue',lwd=2)
          lines(10^y.p[,3]~x,col='lightblue',lwd=2)        
        } else {
          lines(y.p[,2]~x,col='lightblue',lwd=2)
          lines(y.p[,3]~x,col='lightblue',lwd=2)
        }
      }

      if(showIQR) abline(int, slope, col = "red", lwd = lwd)      
    }
    points(x,y,cex=cex.points,pch=19,col=col)
    box(lwd=lwd)
    if(flgMTB) {
      require(nortest)
      y.mean <- mean(y)
      y.med <- median(y)
      y.sd <- sd(y)
      y.n <- length(y)
      y.adc <- NA
      if(y.n > 7) {
        ad <- ad.test(y)
        if(Log=='y') ad <- ad.test(log10(y))
        y.ad <- ad$statistic
        st <- ''
        if(ad$p.value <= 0.05) st <- '*'
        y.adc <- paste(format(y.ad,digits=2),st,sep='')
      }
      par(xpd=T)
      legend('topright',legend=paste(c('Mean','Median','Std. Dev','A-D test','n',
        format(c(y.mean,y.med,y.sd),digits=2),y.adc,ceiling(y.n))),ncol=2,
        bg='white',inset=c(-.36,0),cex=.7)
      par(xpd=F)
    }  
    par(bg='white')  
    z <- list(qdist = QFUN, int = int, slope = slope)
    class(z) <- "probplot"
    par(oPar)
    invisible(z)
}

pPlotyxOld <- function (x, qdist = qnorm, probs = NULL, line = TRUE, ylab = NULL,
    xlab = "Frequency of observations less than, %", xTicks = NULL, xTickLabels = NULL,
    main = NULL, grid = FALSE, xLim = NULL, Log = '', col='black',
    cInd = NULL, xaxt = 'y', addData=F, cex.pts = 1, ...)
{
    DOTARGS <- as.list(substitute(list(...)))[-1]
    DOTARGS <- paste(names(DOTARGS), DOTARGS, sep = "=", collapse = ", ")
 #   xlab = deparse(substitute(x))
    x <- sort(x)
    QNAME <- deparse(substitute(qdist))
    DOTS <- list(...)
    qdist <- match.fun(qdist)
    QFUN <- function(p) {
        args = DOTS
        args$p = p
        do.call("qdist", args)
    }
    y <- QFUN(ppoints(length(x)))
    if (is.null(probs)) {
        probs <- c(0.01, 0.02, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.98,
            0.99)
        if (length(x) >= 1000)
            probs <- c(0.001, probs, 0.999)
    }
    qprobs <- QFUN(probs)
    if(!addData) {
    plot(y, x, axes = FALSE, type = "n", xlim = range(c(y, qprobs)),
        ylim = xLim, xlab = xlab, ylab = ylab, main=main, log=Log)
    box()
    if(grid) abline(v = qprobs,h=xTicks, col = "lightgrey", lty=2)

    axis(2, at = xTicks, labels = xTickLabels, tcl=.35)
    if(xaxt=='y') axis(1, at = qprobs, labels = 100 * probs, las = 1,
      tcl = .35, cex.axis = .85)
    }
    points(x~y, pch=19,col=col,cex=cex.pts)
    QTEXT <- paste("Quantile: ", QNAME, sep = "")
    if (nchar(DOTARGS))
        QTEXT <- paste(QTEXT, DOTARGS, sep = ", ")
    #mtext(QTEXT, side = 1, line = 3, adj = 1)
    if(length(cInd) == 0) cInd = 1:length(x)
    xr = x[cInd]
    if(Log=='y') xr = log10(xr)
    pp.lm = lm(xr~y[cInd])
    xp = predict(lm(xr~y[cInd]))
    yp = y[cInd]
    yl = c(yp[1],yp[length(cInd)])
    xl = c(xp[1],xp[length(cInd)])
    slope <- diff(xl)/diff(yl)
    int <- xl[1] - slope * yl[1]
    if (line) {
      abline(int,slope,col=col)
    }
    z <- list(qdist = QFUN, int = int, slope = slope)
    class(z) <- "probplot"
    invisible(z)
}

SeasonalSubSeries <- function(x.df,pCol,pLab,pType='Season',breakOnYear=F,sf=1,Log='n') {

  par(mar=c(4,3.5,1,1),mgp=c(2.5,.5,0),pch=19,las=1,tcl=0)
  
#  [ To create these plots, we need to create variables with which to extract ]
#  [ seasons and months. Months are easy, as they are part of a R base ]
#  [ procedure as.POSIXlt(...) that creates a list date values that includes ]
#  [ the month as ‘mon’, using d$mon where d is the date object. Unfortunately ]
#  [ no ‘season’ exists, so we’ll have to write a bit of code to do it ourselves ]
#  [ We’ll write a procedure called season(...) (bottom of this box) that accepts a ]
#  [ vector of dates, figures out the seasons (Jan-Mar == Jul-Sep = summer, and ]
#  [ returns a vector of indices that season. Those indices are then used to find ]
#  [ the subset of the data frame that corresponds to a season.]
  # add columns to the LBR.TotalP data frame to hold Month and Season
  
  x.df[,'Year'] = as.POSIXlt(x.df$Date)$year + 1900
  x.df[,'Month'] = as.POSIXlt(x.df$Date)$mon + 1
  x.df[,'Season'] = rep(0,times=length(x.df$Date)) # create space in df
  # loop through the seasons to get the indices, then populate the $Season element
  # of the data frame with the season ID
  for(i in 1:4){
    sind = season(x.df$Date,i)
    x.df$Season[sind]=i
  }
  
  nYr = length(unique(x.df$Year))+2
  iYr = 1:length(x.df[,pCol])
  y = x.df[,pCol]
  d = x.df$Date
  
  # choose one then the other for the ’seasonal subseries plot’

  if(!pType == 'Season') {
    nSn <- 12; xLab <- 'Month'; xLabels <- c(month.abb,'') # R has month names - get them
  }
  else {
    nSn <- 4; xLab <- 'Season'; xLabels <- c('Winter','Spring','Summer','Fall','')
  }
  
  # set up axis label bounds and label tick marks

  xLb <- nYr*365.25/3; xUb <- nSn*nYr*365.25 + xLb
  at <- seq(xLb,xUb,length.out=nSn+1)

  # create the plot with no axes or data - we’ll add them later

  plot(y/sf~iYr,data=x.df,type='n',xlim = c(xLb,xUb),xlab=xLab,ylab=pLab,xaxt='n',log=Log)

  # add axes and lines to separate seasons/months

  axis(1,at=at+xLb,labels=xLabels)
  abline(v=at,col='grey')
  
  # add a line for each season/month - note that we assume the average year length
  # is 365.25 days to account for leap year - good enough for plotting purposes
  
  for(i in 1:(nSn)) {
    if(nSn == 12) {
      pind <- which(x.df$Month == i)
    }
    else if(nSn == 4) {
      pind <- which(x.df$Season == i)
    } 
    tm <- (i-1)*nYr*365.25 + xLb +
      (as.POSIXlt(d[pind])$year - as.POSIXlt(d[1])$year)*365.25 +
       as.POSIXlt(d[pind])$yday
    if(!breakOnYear) {      
      lines(y[pind]/sf~tm)
    }
    else{
      sapply(unique(x.df$Year[pind]),function(yr) {
        yInd <- which(as.POSIXlt(d[pind])$year+1900 == yr)
        lines(y[pind][yInd]/sf~tm[yInd])
      })
    }
  }
}
# user-defined procedure to return indices for dates matching the Season criterion
season <- function(dfDate,Season) {
  mnths <- matrix(1:12,nrow=4,ncol=3,byrow=TRUE)
  ind <- (as.POSIXlt(dfDate)$mon+1) %in% mnths[Season,1:3]
  season <- which(ind==TRUE)
  season
}


boxedtext <- function(x,y,l,pos=1,offset=0,cex=1,col='grey') {
  o = strwidth("UUU",cex=cex)*offset 
  w = strwidth(l,cex=cex) 
  hgt = strheight(l,cex=cex)
  if(pos==2) {         # left
    lft <- x - o - w; rgt <- lft + w*1.1
    bot <- y - hgt/2*2; top <- bot + hgt*2  
  }      
  else if(pos==4) {    # right
    lft <- x + o - w/10; rgt <- lft + w*1.1
    bot <- y- hgt/2*2; top <- bot + hgt*2    
  }
  else if(pos==1) {    # below
    lft <- x - w/2*1.05; rgt <- lft + w*1.1
    bot <- y - o - hgt/2*2; top <- bot + hgt*2      
  }
  else if(pos==3) {    # above
    lft <- x - w/2*1.05; rgt <- lft + w*1.1
    bot <- y + o - hgt/2*2; top <- bot + hgt*2 
  } 
  col = col2rgb(col) 
#  cat(lft,rgt,bot,top,o,w,hgt,'\n')
  rect(lft,bot,rgt,top,col=rgb(t(col/255),alpha=.75),border=NA)  
  text(x,y,labels=l,pos=pos,offset=offset,cex=cex)     
}

boxedltext <- function(x,y,l,o,w,hgt,pos=1,offset=0,cex=1,col='grey') {

  if(pos==2) {         # left
    lft <- x - o - w; rgt <- lft + w*1.1
    bot <- y - hgt/2*2; top <- bot + hgt*2  
  }
  else if(pos==4) {    # right
    lft <- x + o - w/10; rgt <- lft + w*1.1
    bot <- y- hgt/2*2; top <- bot + hgt*2    
  }
  else if(pos==1) {    # below
    lft <- x - w/2*1.05; rgt <- lft + w*1.1
    bot <- y - o - hgt/2*2; top <- bot + hgt*2      
  }
  else if(pos==3) {    # above
    lft <- x - w/2*1.05; rgt <- lft + w*1.1
    bot <- y + o - hgt/2*2; top <- bot + hgt*2        
  } 
  col = col2rgb(col) 
  print(c(lft,bot,rgt,top))
#  panel.rect(lft,bot,rgt,top,col=rgb(t(col/255),alpha=.75),border=NA)  
#  panel.text(x,y,labels=l,pos=pos,offset=offset,cex=cex)    
  list(rect=c(lft,bot,rgt,top),x=x,y=y,text=l)   
}

insertToVec <- function(cc,c.ins,iPos) {
  if(length(cc) < iPos) break('vector too short')
  cc.tmp <- cc[1:(iPos-1)]
  cc.tmp <- c(cc.tmp,c.ins)
  cc.tmp <- c(cc.tmp,cc[iPos:length(cc)])
  cc.tmp
}
