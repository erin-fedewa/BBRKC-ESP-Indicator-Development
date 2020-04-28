#<?php 

library(extrafont)
loadfonts()
require(png)
require(grid)
require(Cairo)

# These are things to set between runs
setwd("C:/Users/erin.fedewa/Work/Ecosystem Considerations/Report Cards")

infile        <- "BBRKCindicators.csv"
PLOT_TSTART   <- 1980  
PLOT_CURRENT  <- 2019 
MOVING_WINDOW <- 5          # Number of years for green zone
#Regimes       <- c(PLOT_TSTART+10,1977,1989,1999,PLOT_CURRENT)  #ORIGINAL
Regimes       <- c(PLOT_TSTART,1980,1990,2000,PLOT_CURRENT)   #STEPH CHANGED

# PlotWidth and PlotHeight only used for png version, not pdf version
# PlotWidth  <- 3900; 
# PlotHeight <- PlotWidth/10
# SC         <- 1

# Scaling for pdf
SC <- 0.1

# Chart offsets for areas
PLOT_CURMIN <- PLOT_CURRENT - MOVING_WINDOW + 0.99 
PLOT_CURMAX <- PLOT_CURRENT + 0.01 
PLOT_TEND   <- PLOT_CURRENT + 6    

# Read in data from .csv file 
# Rows needs to be:  Name, Page, Order, Category, [data rows]
# NO COMMAS allowed in fields (in series names).  Left column is Year
din      <- readLines(infile,n=4);  
din      <- strsplit(din,",")
incols   <- 2:length(din[[1]])
in_names <- as.character(din[[1]][incols])
in_page  <- as.numeric  (din[[2]][incols])
in_order <- as.numeric  (din[[3]][incols])
in_cat   <- as.character(din[[4]][incols])
indat    <- read.table(infile,skip=4,header=FALSE,sep=',')
years    <- indat[,1]
in_dat   <- indat[,incols]
Nfigs    <- length(incols) 

# Read in direction icons     #######BEN removed arrows###########
mean_def  <- readPNG("mean_def.png")
mean_hi   <- readPNG("mean_hi.png")  
mean_lo   <- readPNG("mean_lo.png")
trend_def <- readPNG("trend_def.png")
trend_hi  <- readPNG("trend_hi.png")
trend_lo  <- readPNG("trend_lo.png")
nodat     <- readPNG("nodat.png")

ords   <- order(in_page,in_order)  
ordkey <- rbind(in_page[ords],in_order[ords],ords)
pages  <- as.numeric(levels(as.factor(ordkey[1,])))

# Main page loop  

###################################### output file name
# Make this a single multipage document
CairoPDF("BBRKCreportcard.pdf",width=8.5,height=11)
######################################

for (pp in pages){
  
  # Negative pages are skipped
  if (pp>0){
    # Series on that page with position < 999 are drawn, this is list for page 
    series <- ordkey[3,(ordkey[1,]==pp & ordkey[2,]<999)]  	
    # If list has at least one series, no more than 10, draw, otherwise don't
    NS <- length(series)
    if (NS>10 | NS<1){                       # Steph changed from 10 to 11
      cat("WARNING page",pp,"has",NS,"figures\n"); flush.console()
    }
    else{ 
      
      # Start page drawing
      cat("Making page",pp,"\n")
      
      # If you want to make single pages, uncomment this
      #CairoPDF(paste("PageNum_",pp,".pdf",sep=''),width=8.5,height=11)
      
      # Old layout:  par(mfrow=c(12,1))
      # New layout: 1 row per figure, 2 rows for "footer captions"
      layout(matrix(c(1:NS,NS+1,NS+1,rep(0,12-NS-2)),12,1))           # Steph changed 2 instances of 12 to 13
      par(omi=c(1,1,1,1))
      
      # Main Loop for making individual figures
      for (ii in series){   
        # Remove missing values
        est <- in_dat[,ii]
        estX <- years[!is.na(est)]
        estY <- est[!is.na(est)]
        
        # Select "recent window" data
        trendX <- estX[estX>PLOT_CURMIN & estX<PLOT_CURMAX]
        trendY <- estY[estX>PLOT_CURMIN & estX<PLOT_CURMAX]
        
        # Make main graph (don't draw if fewer than 2 data points)
        if (length(estX) < 2){
          cat("Warning: less than 2 data points in",in_names[ii],"\n"); flush.console();
        }
        else {
          # basic statistics
          e_sd <- sd(estY)
          e_md <- mean(estY)
          
          # pick figures for recent mean and recent trend  
          mfig <- mean_def;  mstat <- "mean-00"
          tfig <- trend_def; tstat <- "trend-00"
          # select figure for mean
          if (length(trendX)<2){mfig <- nodat; mstat<-"mean-XX"} 
          else {
            mval <- (mean(trendY) - e_md)/e_sd
            if (mval >  1.0) {mfig <- mean_hi; mstat<-"mean-UP"}
            if (mval < -1.0) {mfig <- mean_lo; mstat<-"mean-DN"}    	
          }
          # select figure for trend
          if (length(trendX)<3){tfig <- nodat; tstat <- "trend-XX"}
          else {
            # regression in recent window
            res   <- summary(lm(trendY~trendX))
            slope <- coef(res)["trendX","Estimate"]
            pval  <- coef(res)["trendX","Pr(>|t|)"]		  	  
            # For now setting plim 1.1 so non-significant slopes still display symbol
            plim <- 1.1 
            if (pval<=plim){
              # Limit is "will go from green zone edge to middle (or vice-versa) within time window" 
              slopelim <- (1.0/MOVING_WINDOW) * e_sd 
              if (slope >  slopelim){tfig <- trend_hi; tstat <- "trend-UP"} 
              if (slope < -slopelim){tfig <- trend_lo; tstat <- "trend-DN"} 	  	 
            }
          }
          
          # Values for a curved spline to connect data points
          #spl <- spline(estX,estY,n=length(estX)*119, method="natural")
          #alternate smoother: lo <- loess(estY~estX,span=.75,degree=2)
          # The "natural" spline above has a lot of oscillations when missing data.
          # This one here minimizes that and removes the anti-murres (by using only
          # local derivatives).
          N <- length(estY)
          dspl<-rep(0,N)
          dspl[2:(N-1)] <- (estY[3:N] - estY[1:(N-2)])/(estX[3:N] - estX[1:(N-2)])
          dspl[1] <- (estY[2]-estY[1])/(estX[2]-estX[1])
          dspl[N] <- (estY[N]-estY[N-1])/(estX[N]-estX[N-1])		  
          splfun  <- splinefunH(estX,estY,dspl)
          x   <- seq(min(estX),max(estX),length.out=length(estX)*119)
          y   <- splfun(x)
          spl <- list(x=x,y=y)
          
          # Base plot
          cat("Making graph",in_names[ii],mstat,tstat,"\n"); flush.console();
          par(mai=c(0.1,0.0,0.15,0.0))
          #par(bty="n")
          
          # This mess is to get nice y-axis scaling with 3 axis marks		  
          yr <- c(min(estY,e_md-e_sd,spl$y),max(estY,e_md+2*e_sd,spl$y))
          yd <- floor(log10(yr[2]-yr[1]))
          y1 <-   floor(yr[1]*10^(-yd)) 
          y2 <- ceiling(yr[2]*10^(-yd)) 
          if (yd<1 & (y2-y1)%%2 ==1){y2 <- y2+1}
          y1 <- y1*10^yd
          y2 <- y2*10^yd
          # Base plot and axis
          plot(years,est,type='n',xlab="",ylab="",bty="n", 
               xlim=c(PLOT_TSTART,PLOT_TEND),xaxt="n",yaxt="n",ylim=c(y1,y2))
          axis(2,lwd=0.3,cex.axis=0.55,las=1,yaxp=c(y1,y2,2),tck=-0.05,mgp=c(-3,-0.6,-1))
          
          # top orange infill   
          topX <- spl$x[spl$y>e_md+e_sd]
          topY <- spl$y[spl$y>e_md+e_sd]
          if (length(topY)>0){
            segments(topX,topY,topX,e_md+e_sd,lwd=2*SC,col="#FFCC00",lend="square" )
          }  
          # bottom blue infill		
          botX <- spl$x[spl$y<e_md-e_sd]
          botY <- spl$y[spl$y<e_md-e_sd]
          if (length(botY)>0){
            segments(botX,botY,botX,e_md-e_sd,lwd=2*SC,col="#0000FF",lend="square"  )        
          }
          # shaded green area for recent data
          polygon(c(PLOT_CURMIN-0.5,PLOT_CURMIN-0.5,PLOT_CURMAX+0.5,PLOT_CURMAX+0.5),
                  c(e_md+e_sd,e_md-e_sd,e_md-e_sd,e_md+e_sd),col="#B2CCB2",border=NA)					
          # dotted line for mean
          segments(PLOT_TSTART,e_md,PLOT_CURMAX+0.5,e_md,col="#338033",lty=2,lwd=2*SC)       
          # add +/- green bars for whole time series
          segments(c(PLOT_TSTART,PLOT_TSTART),c(e_md+e_sd,e_md-e_sd),
                   c(PLOT_CURRENT,PLOT_CURRENT),c(e_md+e_sd,e_md-e_sd),
                   col="#338033",lwd=2*SC)           		
          # add thick green +/- bars for 5-year window
          segments(c(PLOT_CURMIN-0.5,PLOT_CURMIN-0.5), c(e_md+e_sd,e_md-e_sd),
                   c(PLOT_CURMAX+0.5,PLOT_CURMAX+0.5), c(e_md+e_sd,e_md-e_sd), 
                   lwd=6*SC,col="#338033")				 
          # add brown regime lines
          segments(Regimes,y1,Regimes,y2,col="#995500",lty=2,lwd=2*SC)
          # add spline (red), data points (black) and title
          lines(spl,lwd=4*SC,col="#FF0000") 		
          points(years,est,pch=21,cex=2*SC,col="black",bg="black") 
          text(PLOT_TSTART, e_md+1.25*e_sd, in_names[ii],  
               family="Times", font=1, cex=12*SC, adj=c(0,0))    	  # Steph changed cex from 10 to 12
          # add trend and mean icons
          # NOTE: this works differently on pngs versus pdfs due to
          # coordinate reversal.  written for pdfs only.	  
          # ndc units are in fraction of device (page) size
          yy      <-  grconvertY(e_md,from="user",to="ndc") #plotm/11
          xx1     <- 0.80
          xx2     <- 0.84
          pheight <- 0.35/12
          ##cat(xx1,xx2,yy,"\n"); flush.console
          grid.raster(image = mfig, x=xx1, y=yy, height=pheight, just="left") 
          grid.raster(image = tfig, x=xx2, y=yy, height=pheight, just="left") 
          
        } # end of >2 data points condition for drawing
      } # end of individual graph loop 
      
      # Page Footer
      # Year labels
      par(mai=c(0.0,0.0,0.0,0.0))
      plot(years,seq(0,1,length.out=length(years)),type='n',xlab="",ylab="", 
           bty="n",xlim=c(PLOT_TSTART,PLOT_TEND),xaxt="n",yaxt="n",ylim=c(0,1))     
      text(Regimes,rep(0.95,length(Regimes)),Regimes,family="Times",font=3)
      
      x1=grconvertX(0.27,from="ndc",to="user")
      x2=grconvertX(0.57,from="ndc",to="user")
      yrs <- paste(PLOT_CURRENT-MOVING_WINDOW+1,PLOT_CURRENT,sep="-")                
      text(c(x1,x2),c(0.7,0.7),c(paste(yrs,"Mean"),paste(yrs,"Trend")),
           family="Helvetica",font=3,cex=1.2,pos=4)
      
      x1=grconvertX(0.3,from="ndc",to="user")
      x2=grconvertX(0.6,from="ndc",to="user")
      legtext <- c("1 s.d. above mean", 
                   "1 s.d. below mean",
                   "within 1 s.d. of mean",
                   "fewer than 2 data points",
                   "increase by 1 s.d. over time window", 
                   "decrease by 1 s.d. over time window", 
                   "change <1 s.d. over window",
                   "fewer than 3 data points")
      legx    <- c(x1,x1,x1,x1,x2,x2,x2,x2)
      legy	 <- c(0.54,0.37,0.19,0.03,0.54,0.37,0.19,0.03)    
      text(legx,legy,legtext,family="Helvetica",pos=4)
      
      legk    <- list( mean_hi,  mean_lo,  mean_def, nodat,  
                       trend_hi, trend_lo, trend_def, nodat)	 
      pheight <- 0.25/12
      for (ff in 1:length(legk)){
        yy <- grconvertY(legy[ff],from="user",to="ndc")
        xx <- grconvertX(legx[ff],from="user",to="ndc")
        grid.raster(image = legk[[ff]], x=xx, y=yy, height=pheight, just="right") 	
      }
      
      # If making single pages, uncomment this
      #dev.off()
      mtext("Bristol Bay Red King Crab", side=3, line=1, outer=TRUE)       
    }} # end page-creation conditionals 
}  # end of page loop 
dev.off()