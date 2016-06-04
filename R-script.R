## Loading required libraries
library(xts)

## Reading data set
data <- read.csv(file.choose(),check.names=F,stringsAsFactors=F)

## Cleaning data
data <- data[complete.cases(data),]					## Removing NA's
date_data <- data[,1]								## Saving date information separate
data <- data[,-1]									## Keep only data
data[,2] <- as.numeric(gsub(",",".",data[,2]))		## Fixing one anomaly in data
data <- as.matrix(data)								## Convert to matrix
data[1:3021,1] <- NA
data[1:979,10] <- NA
date_data <- as.Date(date_data,"%m/%d/%y")
T <- xts(data,date_data)

#####################################################################
## Plotting data set ##
#####################################################################
## Smoothing the surface by making rows splines thus adding more columns intermediate
smoother=10
if(smoother>1)
Z <- as.xts(t(apply(T,1,function(x) spline(as.vector(coredata(x)), n=smoother*length(x))$y)))

## Removing the NA data fed as Zero
Z[1:3021,1:smoother] <- NA

## Defining x axis ticks 
time_axis <- axTicksByTime(T)

## Cooking colours
col_blue <- colorRampPalette(c("white","dodgerblue4"))
col_cooked <- matrix(data=rep(c("black",col_blue(NCOL(Z)-1)),each=(NROW(Z))),ncol=ncol(Z))
col_cooked[1:3021,smoother+1] <- "black"
col_cooked <- as.vector((col_cooked))

## Preparing device for saving the plot
png("yield_curve.png",width=1500,height=1000)

## Forming the surface
par(mar=c(3,1,1,1))
pm 	<-	persp(z=Z, shade=.2, ltheta=30, r=5, theta=30, scale=F, border=NA, box=F,
		x=(1:NROW(Z))/(length(time_axis)*10),
		y=(1:NCOL(Z))/smoother,
		col=col_cooked)
		
## Calculating the distance to place ticks
x_axis <- seq(1, NROW(Z), length.out=length(time_axis))/(length(time_axis)*10)
y_axis <- seq(1, NCOL(Z), length.out=NCOL(Z)/smoother)/smoother

## Plotting x axis
cnames <- colnames(T)		
xy0 <- trans3d(x_axis,y_axis[1],0,pm)
xy1 <- trans3d(x_axis,y_axis[1]-0.3,0,pm)
lines(trans3d(x_axis,y_axis[1],0,pm),col="#555555")
segments(xy0$x,xy0$y,xy1$x,xy1$y, col="#555555")
text(xy1$x, xy1$y, labels=format(as.Date(names(time_axis),"%b %d %Y"),"%b-%y"), pos=2, offset=.5,cex=1.2, srt=30)

# Plotting y axis
xy0 <- trans3d(x_axis[length(x_axis)], y_axis, 0, pm)
xy1 <- trans3d(x_axis[length(x_axis)]+.3, y_axis, 0, pm)
yz0 <- trans3d(x_axis[length(x_axis)], y_axis, coredata(Z)[NROW(Z),seq(1,NCOL(Z),by=smoother)], pm) # vertical y
lines(trans3d(x_axis[length(x_axis)], y_axis, 0, pm),col="#555555")
segments(xy0$x,xy0$y,xy1$x,xy1$y,col="#555555")
text(xy1$x, xy1$y, labels=paste(cnames,"Month"), pos=4, offset=0.5,cex=1.2,srt=350)

# Plotting z axis
z_axis <- seq(trunc(min(Z,na.rm=TRUE)), round(max(Z, na.rm=TRUE)))
xy0 <- trans3d(x_axis[length(x_axis)], y_axis[length(y_axis)], z_axis, pm)
xy1 <- trans3d(x_axis[length(x_axis)]+0.3, y_axis[length(y_axis)], z_axis, pm)
lines(trans3d(x_axis[length(x_axis)], y_axis[length(y_axis)], z_axis, pm))
segments(xy0$x,xy0$y,xy1$x,xy1$y)
labels_z <- paste(z_axis,'%',sep='')
labels_z[1] <- ""
text(xy1$x, xy1$y, labels=labels_z, pos=4, offset=0,cex=1.2)

## Put title
title("The Yield Curve")
			
## Turning off the graphic device
dev.off()	
			