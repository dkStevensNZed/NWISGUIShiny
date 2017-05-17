getCols <- function(n,c.=50,l=50,power=1) {
  # cols <- sapply(seq(80,310,length.out=n),function(h) {
  #   sequential_hcl(n=1,h=h,c.=c.,l=l,power=power)
  # })
  Cols <- list('1'=c("#b96f74"),
               '2'=c("#98984d","#b3669e"),
               '3'=c("#9e6ebd","#7aa457","#cb6751"),
               '4'=c("#cb5582","#71a659","#8975ca","#c5783e"),
               '5'=c("#b4943e","#777acd","#60a862","#c45ca2","#cb5a4c"),
               '6'=c("#cc5559","#72ad42","#ba5fb2","#5da378","#6a7fce",
                     "#bc8b3d"),
               '7'=c("#c45ca2","#76b74b","#7876cc","#c59040","#4fb7a2",
                     "#cb574e","#6b823d"),
               '8'=c("#7375cb","#989b3d","#bb5ebc","#55aa61","#c85878",
                     "#4dacca","#cd5136","#c88644"),
               '9'=c("#618bcc","#7ea342","#8460cb","#c18c41","#cb48a0",
                     "#4aab83","#cb5537","#be7cb9","#c7596d"),
               '10'=c("#ce436a","#6ab54c","#b459c1","#cba63c","#6f7dcb",
                      "#cf5433","#4bb197","#c06c94","#74843b","#bd794d"),
               '35'=c("#676d24","#b05dd4","#53c95b","#d54eab","#6fba38",
                      "#5e67d7","#a8ba2f","#884ba4","#1cd285","#dc3f6e",
                      "#51bf80","#db3b38","#3abec8","#df752a","#5c7dc8",
                      "#d5a133","#be8fdc","#62a03a","#ad3f77","#3d8035",
                      "#dd83b1","#b7b04a","#84588f","#9ab66e","#b03d2b",
                      "#60bf9f","#e96f54","#5da5d8","#a16623","#36815b",
                      "#dd7c7e","#a1924b","#9d4658","#da9866","#9b5837")
  )
  nm <- names(Cols)
  if(!n %in% nm) n <- '35'
  return(Cols[[as.character(n)]])
}

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}

getGapTimes <- function(d.tmp) {
  
  gap <- difftime(d.tmp$dateTime[-1],d.tmp$dateTime[-nrow(d.tmp)],units='min')
  gap.start <- which(gap > 15 & gap <= 1e10)
  gap.end <- gap.start + 1
  gap.times <- matrix(d.tmp[sort(c(gap.start,gap.end)),'dateTime'],ncol=2,byrow=T)
  gap.times <- data.frame(Start=as.POSIXct(gap.times[,1],origin='1970-01-01'),
                          End=as.POSIXct(gap.times[,2],origin='1970-01-01'))
  return(gap.times)
  
}

extractData <- function(flgUpDate=T,Result.all.lst,wy=c('WY2016'),st=c('RICH'),vars=c('Tempw'),resetDates=T) {
  
  flgRecalcStatSummary <<- T
  
#  enabled(gbPlot) <- enabled(gbPlotGodin) <- enabled(gbPlotButterworth) <- enabled(gbPlotHarmonic) <- enabled(gbGapStats) <- enabled(gbGapFill) <- F
#  svalue(sb) <- 'Extracting data ...'
  wYear <- wy
#  if(is.na(vars)) vars <- svalue(gcbgParameter)
#  if(length(vars) > 1) galert(msg=paste('For now, only one parameter can be selected.\nYou selected ',
#                                        paste(vars,collapse=', '),sep=''))
  sites <- st
  if(length(wYear) == 1 & length(vars) == 1 & length(sites) == 1)  {
    ds <- chooseDataSubsetUSGS(Result.all.lst,wYear,vars,sites)
    
    if(is.na(ds[1])) {   #} nrow(d.tmp) == 0) {
      #      cat('2\n'); browser()
      
      # svalue(glNoData) <- paste('No data found for site(s)\n',
      #                           paste(sites,collapse=''),' for water years ',
      #                           paste(wYear,collapse=''),'\n',
      #                           paste(vars,collapse=','),'\n\nMake another selection.',sep='')
      return(F)
    }
    
    
    d.tmp <- Result.all.lst[[wYear]][[vars]]$Data[[sites]][ds$d.ind,]
    #    browser()
    d.tmp <- d.tmp[which(d.tmp[,4] > 0),]
    
#    svalue(glNoData) <- 'Processing extracted data'
    
    # this part is really slow for large data sets
    d.tmp$dateTime <- as.POSIXct(as.POSIXlt(d.tmp$dateTime,tz='Etc/GMT+8'),tz='Etc/GMT+8')
    tStart <- min(d.tmp$dateTime)  #as.POSIXct(svalue(geStart),tz='Etc/GMT+8')
    tEnd <- max(d.tmp$dateTime)  #as.POSIXct(svalue(geEnd),tz='Etc/GMT+8')
    if(!resetDates) d.tmp <- d.tmp[which(d.tmp$dateTime >= tStart & d.tmp$dateTime <= tEnd),]
    
    # if(flgUpDate) {
    #   sapply(names(graphicsList),function(ngl) {gl <- graphicsList[[ngl]]; blockHandlers(gl$obj)})
    #   if(resetDates) svalue(geStart) <- min(d.tmp$dateTime); svalue(geEnd) <- max(d.tmp$dateTime)
    #   sapply(names(graphicsList),function(ngl) {gl <- graphicsList[[ngl]]; unblockHandlers(gl$obj)})
    # }
    
    if(ncol(d.tmp) == 6) {
      d.tmp[,7] <- NA
      d.tmp[,8] <- d.tmp[,6]
      d.tmp[,6] <- NA
    }
    
    USGSNames <- names(d.tmp)
    pNames <<- parseNames(USGSNames)
    
    names(d.tmp)[4:8] <- c('ResultU','StatusU','Resultl','StatusL','TimeZone')
    d.tmp$WaterYear <- getWaterYr(d.tmp$dateTime)  #wYear
    tStart <- min(d.tmp$dateTime)  #as.POSIXct(svalue(geStart),tz='Etc/GMT+8')
    tEnd <- max(d.tmp$dateTime)  #as.POSIXct(svalue(geEnd),tz='Etc/GMT+8')
    
#    tStart <- as.POSIXct(svalue(geStart),tz='Etc/GMT+8');tEnd <- as.POSIXct(svalue(geEnd),tz='Etc/GMT+8')
    #    d.tmp <- d.tmp[which(d.tmp$dateTime >= tStart & d.tmp$dateTime <= tEnd),]
    d.tmp$DepthU <- pNames[1]
    if(ncol(d.tmp) > 5) d.tmp$DepthL <- pNames[2]
    d.tmp$WaterYear <- getWaterYr(as.POSIXct(as.POSIXlt(d.tmp$dateTime,tz='Etc/GMT+8'),tz='Etc/GMT+8'))
    ds.cur <<- d.tmp
    
  } else {
    
    d.tmp <- buildDataFrame(Result.all.lst,wYear,vars,sites)
    
    if(is.null(d.tmp) | ncol(d.tmp) == 0 | nrow(d.tmp) == 0) {
      # svalue(glNoData) <- paste('No data found for site(s)\n',
      #                           paste(sites,collapse=''),'\nfor water years ',
      #                           paste(wYear,collapse=','),'\n',
      #                           paste(vars,collapse=','),'\n\nMake another selection.',sep='')
      # 
      return(F)
    }
    
    d.tmp <- d.tmp[which(d.tmp[,4] > 0),]
    d.tmp$dateTime <- as.POSIXct(as.POSIXlt(d.tmp$dateTime,tz='Etc/GMT+8'),tz='Etc/GMT+8')
    tStart <- min(d.tmp$dateTime)  #as.POSIXct(svalue(geStart),tz='Etc/GMT+8')
    tEnd <- max(d.tmp$dateTime)  #as.POSIXct(svalue(geEnd),tz='Etc/GMT+8')
#    tStart <- as.POSIXct(svalue(geStart),tz='Etc/GMT+8')
#    tEnd <- as.POSIXct(svalue(geEnd),tz='Etc/GMT+8')
    if(!resetDates) d.tmp <- d.tmp[which(d.tmp$dateTime >= tStart & d.tmp$dateTime <= tEnd),]
    
    if(flgUpDate) {
#      sapply(names(graphicsList),function(ngl) {gl <- graphicsList[[ngl]]; blockHandlers(gl$obj)})
#      if(resetDates) svalue(geStart) <- min(d.tmp$dateTime); svalue(geEnd) <- max(d.tmp$dateTime)
#      sapply(names(graphicsList),function(ngl) {gl <- graphicsList[[ngl]]; unblockHandlers(gl$obj)})
    }
    
    # tStart <- as.POSIXct(svalue(geStart),tz='Etc/GMT+8')
    # tEnd <- as.POSIXct(svalue(geEnd),tz='Etc/GMT+8')
    d.tmp <- d.tmp[which(d.tmp$dateTime >= tStart & d.tmp$dateTime <= tEnd),]
    ds.cur <<- d.tmp
  }
  tDSStart <<- min(ds.cur[,'dateTime'])
  tDSEnd <<- max(ds.cur[,'dateTime'])
  
  v <- paste(vars,collapse=', ')
  #  cat('3\n'); browser()
  
  s <- paste(sites,collapse=', ')
  wy <- paste(wYear,collapse=', ')
  if(length(wYear) > 6) wy <- paste(paste(wYear[1:5],collapse=', '),' ... ',wYear[length(wYear)],sep=', ')
#  svalue(glNoData) <- 'Ok'
#  svalue(sb) <- paste('  ',nrow(ds.cur),' observations found for ',v,' in ',
#                      wy,' at ',s,sep='')
  i <<- 0
  if(exists('df.ExpOK')) {
    rm(df.ExpOK,pos=1)
    gc()
  }
#  wait.a.bit <- gtimer(1000*10,fun.time,one.shot=T)
#  sapply(names(graphicsList),function(ngl) {gl <- graphicsList[[ngl]]; blockHandlers(gl$obj)})
#  svalue(geyMax) <- pList[[v]]$limits[2]; svalue(geyMin) <- pList[[v]]$limits[1]
#  sapply(names(graphicsList),function(ngl) {gl <- graphicsList[[ngl]]; unblockHandlers(gl$obj)})
#  enabled(gbPlot) <- T
  return(d.tmp)
  
}

chooseDataSubsetUSGS <- function(d.lst,wyear,var,site) {
  
  d.df <- d.lst[[wyear]][[var]]$Data[[site]]
  
  if(nrow(d.df) > 0) {
    site.no <- site.df$All[which(site.df$All$Site_name==site),'Site_no']
    d.ind <- which(d.df$site_no %in% site.no & d.df[,4] > 0)
    return(list(d.ind=d.ind, site=site, vname=names(d.lst)))
    
  } else
    
    return(NA)
  
}

getWaterYr <- function(dt) {
  yrRange <- unique(range(as.POSIXlt(dt,tz='Etc/GMT+8')$year+1901))
  dt.wy <- rep(NA,times=length(dt))
  sapply(yrRange, function(yr) {
    dStart <- as.POSIXct(paste(yr-1,'-10-01',sep=''),tz='Etc/GMT+8')
    dEnd <- as.POSIXct(paste(yr,'-09-30 23:59:59',sep=''),tz='Etc/GMT+8')
    wyName <- paste('WY',yr,sep='')
    d.ind <- which(between2(dt,dStart,dEnd,T,T))
    dt.wy[d.ind] <<- wyName
  })
  return(dt.wy)
}

getParameters <- function() {
  Tempw <- list(name='Tempw',fullname='Water Temperature',units='Deg C',pCode='00010',limits=c(5,30))
  SpecCond <- list(name='SpecCond',fullname='Specific conductance',units='uSiemens',pCode='00095',limits=c(1,60000))
  Turb <- list(name='Turb',fullname='Turbidity',units='FNU',pCode='63680',limits=c(.1,600))
  Disch <- list(name='Disch',fullname='Discharge',units='cms',pCode='00060',limits=c(.1,1000))
  Vel <- list(name='Vel',fullname='Velocity',units='m/s',pCode='00055',limits=c(.1,10))
  #  TWL <- list(name='TWL',fullname='Tide stage',units='m',pCode='62624',limits=c(.1,10))
  #  RWL <- list(name='RWL',fullname='River stage',units='m',pCode='00072',limits=c(.1,20))
  Gage <- list(name='Gage',fullname='Gage height',units='ft',pCode='00065',limits=c(0.1,50))
  WL <- list(name='WL',fullname='Water level',units='ft',pCode='72137',limits=c(.1,20))
  SSC <- list(name='SSC',fullname='Suspended sediment',units='mg/L',pCode='99409',limits=c(.1,800))
  
  return(list(Tempw=Tempw,SpecCond=SpecCond,Turb=Turb,Disch=Disch,Vel=Vel,Gage=Gage,WL=WL,SSC=SSC))
  
}

parseNames <- function(nm) {
  
  nCol <- NULL
  if(length(nm) == 8) {
    nCol <- c(4,6)
  }
  
  if(is.null(nCol)) return(0)
  nms <- nm[nCol]
  dpths <- sapply(nms,function(NMS) {
    NMS <- gsub('[:] ','.',NMS)
    nm.s <- strsplit(NMS,'[.]')
    dpth <- as.numeric(nm.s[[1]][3])
    dpth
  })
  
  return(dpths)
  
}

buildDataFrame <- function(d.lst,wYear,vars,sites) {
  d.fin <<- data.frame()
  lapply(wYear,function(wyr) {
    sapply(vars, function(v) {
      sapply(sites,function(s) {
        d.tmp <- d.lst[[wyr]][[v]]$Data[[s]]
        
        USGSNames <- names(d.tmp)
        pNames <- parseNames(USGSNames)
        
        if(!is.null(d.tmp)) {
          if(nrow(d.tmp) > 0) {
            if(ncol(d.tmp) == 6) {
              d.tmp[,7] <- NA
              d.tmp[,8] <- d.tmp[,6]
              d.tmp[,6] <- NA
            }
            
            names(d.tmp)[4:8] <- c('ResultU','StatusU','Resultl','StatusL','TimeZone')
            d.tmp$dateTime <- as.POSIXct(as.POSIXlt(d.tmp$dateTime,tz='Etc/GMT+8'),tz='Etc/GMT+8')
            d.tmp$WaterYear <- getWaterYr(d.tmp$dateTime)  #wyr
            d.tmp$DepthU <- pNames[1]
            if(ncol(d.tmp) > 5) d.tmp$DepthL <- pNames[2]
            d.tmp$p_name <- v
            names(d.tmp)[4:8] <- c('ResultU','StatusU','Resultl','StatusL','TimeZone')
            d.fin <<- rbind(d.fin,d.tmp)
            #            svalue(glNoData) <- 'Ok'
          }
          
        } else {
          
          # svalue(glNoData) <- paste('No data found for site(s)\n',
          #                           paste(sites,collapse=''),' for water years ',
          #                           paste(wYear,collapse=''),'\n',
          #                           paste(vars,collapse=','),'\n\nMake another selection.',sep='')
          return(NULL)
          
        }
      })
    })
  })
  return(d.fin)
}

buildSummary <- function(d.sum,rCol) {
  #  cat('in buildSummary: ',nrow(d.sum),'\n');print(d.sum[1,c('WaterYear','p_name')])
  if(nrow(d.sum) > 0) {
    d.Sum <- summary(d.sum[which(d.sum[,rCol] > 0),rCol],na.rm=T)
    d.sd <- sd(d.sum[which(d.sum[,rCol] > 0),rCol],na.rm=T)
    d.n <- nrow(d.sum[which(!is.na(d.sum[,rCol])),])
    d.m <- length(seq.POSIXt(min(d.sum$dateTime),max(d.sum$dateTime),
                             by='15 min'))*length(unique(d.sum$site_no)) - nrow(d.sum)
    d.sumry <- c('# Obs'=floor(d.n),'# Missing'=floor(d.m),d.Sum['Min.'],d.Sum['Max.'],
             round(d.Sum['Mean'],2),'Std. dev.'=round(d.sd,2),d.Sum['Median'])
    return(d.sumry)
  } else {
    d.Sum <- c('# Obser'=0,'# Missing'=0,'Min'=NA,'Max.'=NA,
               'Mean'=NA,'Std. dev.'=NA,'Median'=NA) 
    return(d.Sum)
  }
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

combineSiteNames <- function(site_no,nameOnly=F) {
  ssn <- sapply(site_no,function(sn){
    s <- unique(site.df$All[which(site.df$All$Site_no == sn),])
    if(nameOnly) {
      sSn <- as.character(s$Site_name)
    } else {
      sSN <- paste(sn, ' (',s$Site_name,')',sep='')
    }
  })
  return(ssn)
}

mergeSites <- function(ds.cur,sites) {
  
  d.tmp <- ds.cur[which(!is.na(ds.cur[,c(3,4)])),c(2,3,4)]
  sites <- site.df$All[which(site.df$All$Site_name %in% sites),'Site_no']
  d.mrg <<- d.tmp[which(d.tmp$site_no == sites[1]),]
  names(d.mrg)[3] <- combineSiteNames(unique(d.mrg[,1]))
  sapply(sites[-1],function(st) {
    d.mt <- d.tmp[which(d.tmp$site_no == st),]
    names(d.mt)[3] <- combineSiteNames(unique(d.mt[,1]))
    
    if(nrow(d.mt) > 0)
      d.mrg <<- merge(d.mrg,d.mt,by='dateTime',all=F)
  })
  #  cat('ms\n'); browser()
  return(d.mrg[,c(1,seq(3,ncol(d.mrg),by=2))])
}

mergeWaterYears <- function(ds.cur,wy) {
  
  d.tmp <- ds.cur[,c(2,3,4,9)]
  d.mrg <<- d.tmp[which(d.tmp$WaterYear == wy[1]),]
  names(d.mrg)[3] <<- wy[1]
  d.mrg$yday <<- with(as.POSIXlt(d.mrg$dateTime,tz='Etc/GMT+8'),
                      yday + hour/24 + min/1440 +
                        sec/86400
  )
  d.mrg <<- d.mrg[,c(3,5)]
  
  sapply(wy[-1],function(wys) {
    d.mt <- d.tmp[which(d.tmp$WaterYear == wys),]
    names(d.mt)[3] <- wys
    d.mt$yday <- with(as.POSIXlt(d.mt$dateTime,tz='Etc/GMT+8'),
                      yday + hour/24 + min/1440 +
                        sec/86400
    )
    if(nrow(d.mt) > 0)
      d.mrg <<- merge(d.mrg,d.mt[,c(3,5)],by='yday',all=T)
  })
  
  iCol <- vector()
  iCol <- 1
  iPlt <- 1
  for(i in 2:ncol(d.mrg)) {
    isna <- !all(is.na(d.mrg[,i]))
    if(isna) {
      iCol[iPlt+1] <- i
      iPlt <- iPlt + 1
    }
  }

  return(d.mrg[,iCol])
}
