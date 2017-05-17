#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(shinyjs)
require(DT)

setwd('c:/Users/David Stevens/Desktop/Sabbatical 2016_17/USGS/NWISGUIShiny')

source('SiteSelection.r')
source('ProbabilityPlots.r')
source('extractDataShiny.r')

site.df <<- getAllStations()
pList <<- getParameters()
pCBG <<- sapply(pList,function(pL) pL$fullname)
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# # load auxiliary R files
# #save(Result.all.lst,file='Result.SFBay.rData')
# 
if(!exists('r.x')) {
  cat('Loading database ... ');  flush.console()
  load('r.x.rData')
  cat('Done\n'); flush.console()
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  actionButton("close", "Close window"),
                   
  shinyUI(fluidPage(theme = 'bootstrap.css',
                     
    ## custom CSS for 3 column layout (used below for mechanics filter options)
    tags$head(
      tags$style(HTML("
        .multicol {
        -webkit-column-count: 3; /* Chrome, Safari, Opera */
        -moz-column-count: 3; /* Firefox */
        column-count: 3;
        }"
      )),
      
      tags$style(HTML(type='text/css', "
       .label {
         font-size: 65%;
         line-height: 20px;
       }
       .radio {
         font-size: 65%; 
         line-height: 20px;
         padding-top: 0px;
         padding-bottom: 0px;
         margin-top: 0px;
         margin-bottom: 0px;
       }
       .checkbox {
         font-size: 65%; 
         line-height: 20px;
         padding-top: 0px;
         padding-bottom: 0px;
         margin-top: 2px;
         margin-bottom: 0px;
       }"
      )),
      tags$style(type='text/css', "
        .selectize-input { 
          font-size: 10px; 
          line-height: 10px;
          padding: 0px;
          margin-top: 0px;
          margin-bottom: 0px;
          } 
       .selectize-dropdown {
          font-size: 12px; 
          line-height: 12px; 
          }"
      )
  ),   # end of tags-head block
  
   # Application titles
 
   headerPanel(h2("San Francisco Bay/Sacramento Delta NWIS Visualization")),
   headerPanel(h4("Select from data identifiers")),
 
   fluidRow(wellPanel(fluidRow(
     column(12,fluidRow(
       column(3,
         tags$div(class = "multicol", 
            checkboxGroupInput(inputId="wateryear", 
                               choices = rev(sort(names(r.x))), 
                               label = "Water Year", 
                               selected = 'WY2016', width='75%'))),
       column(1,offset=.5, 
         checkboxGroupInput(inputId="site",label="Site",
                            choices=names(r.x[['WY2016']][[1]]$Data),
                            selected='RICH')),
       column(1,offset=0.5,
         radioButtons(inputId="parameter",label="Parameters",
                            choices=names(pCBG),
                            selected='Tempw')),
       column(3,offset=1,
         radioButtons(inputId="analysis",label="Analysis",
                      choices=c('No filter','Godin filter',
                                'Butterworth','Harmonics','Loess','Moving average',
                                'Period average','Breakpoint trend analysis',
                                'Gap analysis','...'),
                      selected='No filter'))
                
      ))))
      
       # column(4,wellPanel(sliderInput(inputId="month",
       #              label="Month of the year:",
       #              min = 1,
       #              max = 12,
       #              value = 1)
       # ))
      
      ),
   
      mainPanel(
        
        headerPanel(h4("Visualize data")),
        
        tabsetPanel(
          tabPanel("Time series",type='pills', plotOutput("tsPlot",height=366,
                   click = 'tsPlot_click',
                   brush = brushOpts(
                     id='tsPlot_brush',
                     resetOnNew=TRUE
                   )
                   )), 
          tabPanel("Distribution", plotOutput("distPlot")), 
          tabPanel("Probability", plotOutput("probPlot")), 
          tabPanel("Box/Whisker", plotOutput("boxPlot")), 
          tabPanel("Heat map", plotOutput("heatMapPlot")), 
          tabPanel("Correlation", plotOutput("correlationPlot")), 
          tabPanel("Season subseries", plotOutput("seasonalSubPlot")), 
          tabPanel("Summary", verbatimTextOutput("sumTable")),
          tabPanel("Table", dataTableOutput("dataTable")),
          tabPanel("Plot control",
            fluidRow("plotControl",
              tabsetPanel(
                tabPanel("Axis Limits",wellPanel(column(5,
                  dateRangeInput("daterange1", "Date range:",
                                 start = "2016-10-01",
                                 end   = "2017-09-30")),
                  fluidRow(column(2,numericInput("yMin","y Min",5)),
                           column(2,numericInput("yMax","y Max",25))
                  
                ))),
                tabPanel("Plot option",wellPanel()),
                tabPanel("Legend",wellPanel(fluidRow(column(3,
                  checkboxInput("legpos","Legend position")),
                  column(8,
                  selectInput("selleg","Position",
                    choices=c('topleft','top','topright','right','bottomright',
                              'bottom','bottomleft','left','center'),selected='topleft',width= "25%"))))),
                tabPanel("Misc",wellPanel())
              )   # end of nested tabsetPanel
            )
          )
        )   # end of outer tabsetPanel
      )   # end of main panel
    )
  )   # end of shinyUI block
)   # end of ui block

# Define server logic required to draw a histogram

server <- function(input, output) {

  ranges <- reactiveValues(x = NULL, y = NULL)

  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })
  
  output$tsPlot <- renderPlot({
     WY <- sort(input$wateryear)
     nWY <- length(WY)
     ST <- sort(input$site)
     nST <- length(ST)
     VR <- sort(input$parameter)
     nVR <- length(VR)
     cat(WY,VR,ST,'\n')
     ds.cur <- extractData(Result.all.lst=r.x,wy=WY,st=ST,vars=VR)
   
      w.ind <- 1:nrow(ds.cur)  #which(as.POSIXlt(dt)$mon + 1 == input$month)
      x  <- ds.cur[, 4]
      dt <- ds.cur[, 'dateTime']
      x <- x[w.ind]
      dt <- dt[w.ind]
      st.c <- paste(ST,collapse=', ')
      cols <<- add.alpha(getCols(length(ST)),0.5)
        
      xLim <- ranges$x; yLim <- ranges$y
#      main <- paste('Time series for ',st.c,) 
      yLab <- paste(pList[[input$parameter]]$fullname,', ',pList[[input$parameter]]$units,sep='')
      plot(x~dt, type='n', main=paste('Time series for ',st.c,'\n',
                                      'Water years ',paste(WY,collapse=', '),sep=''),
           xlim=xLim,ylim=yLim, 
           col = 'darkgray', xlab='Date/Time',ylab=yLab, tcl=.5, las=1)
      u <- par('usr')
      gap.height <- (u[4] - u[3])/2/30
      iPlt <<- 1
      
      sapply(ST,function(st) {
        sn <- as.character(site.df$All[which(site.df$All$Site_name==st),'Site_no'])
        d.tmp <- ds.cur[which(ds.cur$site_no==sn),]
        gap.times <- getGapTimes(d.tmp)
        ds.tmp <- data.frame(dateTime=seq.POSIXt(min(d.tmp$dateTime),
                                                 max(d.tmp$dateTime),by='15 min'))
        d.tmp <- merge(ds.tmp,d.tmp,by='dateTime',all.x=T)
        
        lines(d.tmp[,4]~d.tmp[,'dateTime'],col=cols[iPlt])
        rect(gap.times[,1],u[3]+gap.height*(iPlt-1),
             gap.times[,2],u[3] + gap.height*(iPlt),border=NA,col=cols[iPlt])
        iPlt <<- iPlt + 1
      })
      if(length(ST) > 1) legend('topleft',legend=ST,cex=0.8,bty='n',fill=cols,border=NA)
      box()
      
   })
   
   observeEvent(input$tsPlot_click, {
     brush <- input$tsPlot_brush
     if (!is.null(brush)) {
       ranges$x <- c(brush$xmin, brush$xmax)
       ranges$y <- c(brush$ymin, brush$ymax)
       
     } else {
       ranges$x <- NULL
       ranges$y <- NULL
     }
   })
   
   output$distPlot <- renderPlot({
     WY <- sort(input$wateryear)
     nWY <- length(WY)
     ST <- sort(input$site)
     nST <- length(ST)
     VR <- sort(input$parameter)
     nVR <- length(VR)
     ds.cur <- extractData(Result.all.lst=r.x,wy=WY,st=ST,vars=VR)
     nCol <- min(nST,3)
     nRow <- ceiling(nST/3)
     par(mfrow=c(nRow,nCol))
     yLim <- range(ds.cur$ResultU)
     iPlt <<- 1
     cols <<- add.alpha(getCols(length(ST)),0.5)
     
     sapply(ST,function(st) {
       sn <- as.character(site.df$All[which(site.df$All$Site_name==st),'Site_no'])
       hist(ds.cur$ResultU[which(ds.cur$site_no==sn)],col=cols[iPlt],main=WY,
            xlim=yLim,xlab=VR)
       u <- par('usr')
       text(u[1],u[4]-(u[4]-u[3])/20,labels=st,pos=4)
       iPlt <<- iPlt + 1
     })
   })

   output$probPlot <- renderPlot({
     WY <- sort(input$wateryear)
     nWY <- length(WY)
     ST <- sort(input$site)
     nST <- length(ST)
     VR <- sort(input$parameter)
     nVR <- length(VR)
     ds.cur <- extractData(Result.all.lst=r.x,wy=WY,st=ST,vars=VR)
     nCol <- min(nST,3)
     nRow <- ceiling(nST/3)
     
     #     par(mfrow=c(nRow,nCol))
     xLim <- range(ds.cur$ResultU)
     iPlt <<- 1
     cols <<- add.alpha(getCols(length(ST)),0.5)
     addData <<- F
     maxN <- max(table(ds.cur$site_no))
     wy <- paste(WY,collapse=', ')
     par(mfrow=c(1,1),mgp=c(2.3,.5,0),cex.main=1.2)
     sapply(ST,function(st) {
       sn <- as.character(site.df$All[which(site.df$All$Site_name==st),'Site_no'])
       Log <- ''
       yLim <- c(qnorm(.9/maxN),-qnorm(.9/maxN))
       main <- paste('Frequency plot for ',paste(ST,collapse=', '),'\n',
                     'Water years ',wy,sep='')
       pPlotxy(ds.cur$ResultU[which(ds.cur$site_no==sn)],col=cols[iPlt],grid=T, xfgrid=T, line=F, Log=Log,
               main = main,xLim=xLim,yLim=yLim,xlab=VR,cex.axis=.8,cex=.5,
               ylab="Frequency of observations\nless than, %", addData=addData)
       #               cex.axis=psz$cex.axis/1.2,cex.lab=psz$cex.axis,cex=psz$pt.cex,pSize=pSize,addData=addData)  #,addMedian=svalue(gchkAddMedianPP))
       #       xlab=paste(v$fullname,', ',v$units,sep=''),
       u <- par('usr')
       iPlt <<- iPlt + 1
       addData <<- T
     })
     if(length(ST)>1) legend('topleft',legend=ST,bty='n',fill=cols,border=NA,cex=.8)
   }) 
   
   output$boxPlot <- renderPlot({
     WY <- sort(input$wateryear)
     WY <- sort(input$wateryear)
     nWY <- length(WY)
     ST <- (input$site)
     SN <- as.character(site.df$All[which(site.df$All$Site_name %in% ST),'Site_no'])
     nST <- length(ST)
     VR <- sort(input$parameter)
     nVR <- length(VR)
     ds.cur <- extractData(Result.all.lst=r.x,wy=WY,st=ST,vars=VR)
     ds.cur <- ds.cur[which(!is.na(ds.cur$ResultU)),]
     ds.cur$site_no <- factor(ds.cur$site_no,levels=SN)
     xLim <- range(ds.cur$ResultU)
     iPlt <<- 1
     cols <<- add.alpha(getCols(length(ST)),0.5)
     addData <<- F
     maxN <- max(table(ds.cur$site_no))
     sf <- 1
     main <- paste('Water years(s): ',paste(WY, collapse=', '),sep='')
     par(mfrow=c(1,1),las=1,tcl=.5,mar=c(5.1,13.1,4.1,2.1),mgp=c(3.5,.5,0))
     bp <- boxplot(ResultU/sf~site_no,xlim=xLim,data=ds.cur,plot=T,na.rm=T)
     if(length(bp$group) > 0) 
       rnge <- range(bp$group)
     else
       rnge <- 1:length(ST)
     
     rnge <- c(min(rnge)-1,max(rnge)+1)
   
     site_f <- ds.cur$site_no
     log <- ''
     plot(0,.5,type='n',ylim=rnge,xlim=xLim,yaxt='n',ylab='',xlab='',
          yaxt='n',xaxt='n',main='',cex.axis=.8)
     boxplot(ResultU/sf~site_no,xlim=xLim,data=ds.cur,add=T,
             col=cols,horizontal=T,varwidth=T,outline=F,
             xlab=VR,ylab='',yaxt='n',
             main=main,log=log,cex=0.7,pch=19)
     points(jitter(bp$group,0.75)~bp$out,pch=19,cex=.25,col='grey80')
#paste(v$fullname,', ',v$units)
 
     yScale <- paste(SN,ST,sep=' - ')
     axis(2,at=1:length(ST),labels=yScale,las=1)
     mtext(text='Site number', side=2, line=11, las=0)
   })
   
   output$heatMapPlot <- renderPlot({
     
     require(lattice)
     WY <- sort(input$wateryear)
     nWY <- length(WY)
     ST <- sort(input$site)
     nST <- length(ST)
     VR <- sort(input$parameter)
     nVR <- length(VR)
     cat(WY,VR,ST,'\n')
     ds.cur <- extractData(Result.all.lst=r.x,wy=WY,st=ST,vars=VR)
     
     Log <- F  #svalue(gcbLogY)
     dv <- ds.cur[,4]
     WaterYear <- ds.cur$WaterYear
     vID <- input$parameter
     
     cols <- rev(brewer.pal(8, 'Spectral'))
     cols.palette <- colorRampPalette(cols)
     ncuts <- 100  #length(unique(ds.cur$WaterYear))
     
     st <- paste(combineSiteNames(unique(ds.cur$site_no)),collapse=', ')
     main <- paste('Heat map by water year for\n',st,', ',vID,sep='')
     if(Log==T) dv <- log10(dv)
     
     doy <- as.POSIXlt(ds.cur$dateTime,tz='Etc/GMT+8')$yday +
       as.POSIXlt(ds.cur$dateTime,tz='Etc/GMT+8')$hour/24 +
       as.POSIXlt(ds.cur$dateTime,tz='Etc/GMT+8')$min/60
     
     m.day <- c(31,29,31,30,31,30,31,31,30,31,30,31)
     d.at <- cumsum(m.day) - m.day/2
     d.at[d.at > 273] <- d.at[d.at > 273] - 366
     d.list <- month.abb[c(10:12,1:9)]
     doy[doy > 273] <- doy[doy > 273] - 366
     
     print(levelplot(dv ~ doy*factor(WaterYear,levels=unique(ds.cur$WaterYear)),
                     as.table=TRUE, xlab='Month of year', ylab='Water Year',
                     scales=list(y=list(alternating=1),
                                 x=list(alternating=1, rot=0,
                                        format="%m-%y",
                                        cex=0.75, at=d.at,labels=month.abb)),
                     strip=strip.custom(par.strip.text=list(cex=0.75), bg=NA),
                     #   auto.key=list(columns=4, lines=TRUE, points=FALSE, cex=0.75),
                     main=main,cex.main=0.8,
                     col.regions=(cols.palette(ncuts)), cuts=ncuts-1
     ))
 #    box()
     
   })
   
   output$correlationPlot <- renderPlot({
     
     require(lattice)
     WY <- sort(input$wateryear)
     nWY <- length(WY)
     ST <- sort(input$site)
     nST <- length(ST)
     VR <- sort(input$parameter)
     nVR <- length(VR)
     cat(WY,VR,ST,'\n')
     ds.cur <- extractData(Result.all.lst=r.x,wy=WY,st=ST,vars=VR)
     bySite <- F; byWY <- T; byPar <- F
     if(bySite) {
       d.mrg <- mergeSites(ds.cur,ST)
       d.tst <<- d.mrg
     }
     else if(byWY)
       d.mrg <- mergeWaterYears(ds.cur,WY)
     
     Log <- F  #svalue(gcbLogY)

     st <- paste(combineSiteNames(unique(ds.cur$site_no)),collapse=', ')
     
     panel.smoothab <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                                 cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
     {
       ok <- is.finite(x) & is.finite(y) & x > 0 & y > 0
       points(x[ok], y[ok], pch = pch, col = col, bg = bg, cex = cex)
       abline(0,1,col='green')
       if (any(ok))
         lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
               col = col.smooth, ...)
     }
     
     panel.cor <- function(x, y, digits = 3, prefix = "", cex.cor, ...)
     {
       usr <- par("usr"); on.exit(par(usr))
       par(usr = c(0, 1, 0, 1))
       ok <- is.finite(x) & is.finite(y) & x > 0 & y > 0
       r <- abs(cor(x[ok], y[ok], use='pairwise.complete.obs'))
       txt <- format(c(r, 0.123456789), digits = digits)[1]
       prefix <- 'r = '
       txt <- paste0(prefix, txt)
       # if(missing(cex.cor))
       cex.cor <- sqrt(0.58/strwidth(txt))
       text(0.5, 0.5, txt,
            cex = 1.5)  #cex.cor * r)
     }
     
     panel.hist <- function(x, ...)
     {
       usr <- par("usr"); on.exit(par(usr))
       par(usr = c(usr[1:2], 0, 1.5) )
       ok <- x > 0
       h <- hist(x, plot = FALSE)
       breaks <- h$breaks; nB <- length(breaks)
       y <- h$counts; y <- y/max(y)
       rect(breaks[-nB], 0, breaks[-1], y,
            col = "palegoldenrod", ...)
     }
     cat('byPar: ',byPar,', ')
     cat('byWY: ',byWY,', ')
     cat('bySite: ',bySite,'\n')
     wy <- paste(WY,collapse=', ')
     if(byPar) {
       if(length(WY) > 6) wy <- paste(paste(WY[1:5],collapse=', '),', ... ',WY[length(WY)],sep='')
       main <- paste('Correlations of', paste(names(d.mrg)[2:3],collapse=', '),'at', ST,
                     '\nfor water year(s):',wy)
     }
     else if(bySite)
       if(length(ST) > 1)
         main <- paste('Correlations of', paste(VR,collapse=', '),
                       'by site for water year(s):',wy)
       else
         return('Choose more than one site')
     
     else if(byWY)
       main <- paste('Correlations of', paste(VR,collapse=', '),'by water year(s) for site(s):',
                     paste(ST,collapse=', '))
     
     par(oma=c(3,3,0,0))
     pairs(d.mrg[,-1],diag.panel=panel.hist,main=main,
           upper.panel=panel.cor,
           lower.panel=panel.smoothab,
           pch='.',cex=0.1,cex.axis=.86,cex.main=1,
           cex.labels=1, gap=0.5)
     
     v.all <- getParameters()
     if(!byPar){
       v <- v.all[[input$parameter]]
     } else {
       v.1 <- v.all[[names(d.mrg)[2]]]
       v.2 <- v.all[[names(d.mrg)[3]]]
     }
     par(oma=c(0,0,0,0))
     
     
   })

   output$sumTable <- renderPrint({
     WY <- sort(input$wateryear)
     WY <- sort(input$wateryear)
     nWY <- length(WY)
     ST <- sort(input$site)
     nST <- length(ST)
     VR <- sort(input$parameter)
     nVR <- length(VR)
     ds.cur <- extractData(Result.all.lst=r.x,wy=WY,st=ST,vars=VR)
     d.tmp <- sapply(unique(ds.cur$site_no), function(sn) {
       d <- ds.cur[which(ds.cur$site_no==sn & !is.na(ds.cur$ResultU)),]
       nObs <- nrow(d)
       d.tmp <- data.frame(dateTime=seq.POSIXt(min(d$dateTime),max(d$dateTime),by='15 min'))
       d.tmp <- merge(d.tmp,d[,c('dateTime','ResultU')],by='dateTime',all.x=T)
       nMiss <- nrow(d.tmp) - nObs
       buildSummary(d,rCol=4)
     })
     d.tmp
   })
   
   output$dataTable <- renderDataTable({
     WY <- sort(input$wateryear)
     cat('dP: ',nrow(ds.cur),'\n')
     WY <- sort(input$wateryear)
     nWY <- length(WY)
     ST <- sort(input$site)
     nST <- length(ST)
     VR <- sort(input$parameter)
     nVR <- length(VR)
     ds.cur <- extractData(Result.all.lst=r.x,wy=WY,st=ST,vars=VR)
     d.tmp <- datatable(head(ds.cur[,2:7],100))
     d.tmp
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

