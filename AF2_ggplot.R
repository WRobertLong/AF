require(foreign)
require(ggplot2)
require(car)
require(reshape2)

# Source code for Atrial Fibrillation paper published in BMJ 
# This script produces manipulates the data and uses ggplot2
# to create the figures ready for publication

dt <- read.csv("AF_by_age.csv")
dt$pop.o65 <- dt$pop.065
dt$pop.065 <- NULL
attach(dt)


new.bin <- list()
dt1 <- data.frame()
for (i in 1:20) {
	lower.bound <- max(0,(((i-1)*500) -1 ))
	upper.bound <- (((i*500) -1 ))
	new.bin[[i]] <- dt[dt$pop.total > lower.bound & dt$pop.total < upper.bound ,]
	qq <- as.list(quantile(new.bin[[i]]$total.AF))
	qq$lower <- lower.bound
	qq$upper <- upper.bound
	qq$n <- nrow(new.bin[[i]])
	dt1 <- rbind(dt1,as.data.frame(qq))
}

for (i in 21:30) {
	lower.bound <- ((((i-20)-1)*1000) -1 )+10000
	upper.bound <- (((i-20)*1000) -1 )+10000
	new.bin[[i]] <- dt[dt$pop.total > lower.bound & dt$pop.total < upper.bound ,]
	qq <- as.list(quantile(new.bin[[i]]$total.AF))
	qq$lower <- lower.bound
	qq$upper <- upper.bound
	qq$n <- nrow(new.bin[[i]])
	dt1 <- rbind(dt1,as.data.frame(qq))
}

for (i in 31:34) {
	lower.bound <- ((((i-30)-1)*5000) -1 )+20000
	upper.bound <- (((i-30)*5000) -1 )+20000
	new.bin[[i]] <- dt[dt$pop.total > lower.bound & dt$pop.total < upper.bound ,]
	qq <- as.list(quantile(new.bin[[i]]$total.AF))
	qq$lower <- lower.bound
	qq$upper <- upper.bound
	qq$n <- nrow(new.bin[[i]])
	dt1 <- rbind(dt1,as.data.frame(qq))
}

names(dt1) <- c("Q0","Q25","Q50","Q75","Q100","lower","upper")

dt1$Q0 <- NULL
dt1$Q100 <- NULL

x11()

plot(dt1$lower,dt1$Q75,col=4,lty=3,xlab="practice size",ylab="AF cases")
points(dt1$lower,dt1$Q25,col=2,lty=2)

abline(lm(Q75~-1+lower,data=dt1), col=4) 
abline(lm(Q25~-1+lower,data=dt1), col=2) 


###########################
#
#  Binning on over 65

new.bin <- list()
dt1 <- data.frame()

#### Bin sizes
#  For all data
#  All 200...32 binds
#  All 250....26 bins


for (i in 1:22) {
	bin.size <- 250
	lower.bound <- max(0,(((i-1)*bin.size) -1 ))
	upper.bound <- (((i*bin.size) -1 ))
	new.bin[[i]] <- dt[dt$pop.o65 > lower.bound & dt$pop.o65 < upper.bound ,]
	qq <- as.list(quantile(new.bin[[i]]$total.AF))
	qq$lower <- lower.bound
	qq$upper <- upper.bound
	dt1 <- rbind(dt1,as.data.frame(qq))
}

for (i in 23:23) {
	bin.size <- 1000
	lower.bound <- ((((i-22)-1)*bin.size ) -1 )+5500
	upper.bound <- (((i-22)*bin.size ) -1 )+5500
	new.bin[[i]] <- dt[dt$pop.o65 > lower.bound & dt$pop.o65 < upper.bound ,]
	qq <- as.list(quantile(new.bin[[i]]$total.AF))
	qq$lower <- lower.bound
	qq$upper <- upper.bound
	dt1 <- rbind(dt1,as.data.frame(qq))
}

#####################
#
#  100 bin size up to 500

new.bin <- list()
dt1 <- data.frame()

n.bin.1 <- 20
bin.size.1 <- 100
for (i in 1:n.bin.1) {
	bin.size <- bin.size.1
	lower.bound <- max(0,(((i-1)*bin.size.1) -1 ))
	upper.bound <- (((i*bin.size.1) -1 ))
	new.bin[[i]] <- dt[dt$pop.o65 > lower.bound & dt$pop.o65 < upper.bound ,]
	qq <- as.list(quantile(new.bin[[i]]$total.AF))
	qq$lower <- lower.bound
	qq$upper <- upper.bound
	dt1 <- rbind(dt1,as.data.frame(qq))
}

bin.size.2 <- 250

for.2.from <-  n.bin.1+1
for.2.to <- n.bin.1 + (5500-( n.bin.1 * bin.size.1 ))/bin.size.2


for (i in for.2.from : for.2.to ) {
	lower.bound <- ((((i-n.bin.1)-1)*bin.size.2) -1 )+ ( n.bin.1 * bin.size.1 )
	upper.bound <- (((i-n.bin.1)*bin.size.2) -1 )+ ( n.bin.1 * bin.size.1 )
	new.bin[[i]] <- dt[dt$pop.o65 > lower.bound & dt$pop.o65 < upper.bound ,]
	qq <- as.list(quantile(new.bin[[i]]$total.AF))
	qq$lower <- lower.bound
	qq$upper <- upper.bound
	dt1 <- rbind(dt1,as.data.frame(qq))
}

for (i in (for.2.to + 1)  : (for.2.to + 1) ) {
	bin.size <- 1000
	lower.bound <- ((((i-for.2.to)-1)*bin.size ) -1 )+5500
	upper.bound <- (((i-for.2.to)*bin.size ) -1 )+5500
	new.bin[[i]] <- dt[dt$pop.o65 > lower.bound & dt$pop.o65 < upper.bound ,]
	qq <- as.list(quantile(new.bin[[i]]$total.AF))
	qq$lower <- lower.bound
	qq$upper <- upper.bound
	dt1 <- rbind(dt1,as.data.frame(qq))
}

#
#
#######################



for (i in 21:30) {
	lower.bound <- ((((i-20)-1)*1000) -1 )+10000
	upper.bound <- (((i-20)*1000) -1 )+10000
	new.bin[[i]] <- dt[dt$pop.total > lower.bound & dt$pop.total < upper.bound ,]
	qq <- as.list(quantile(new.bin[[i]]$total.AF))
	qq$lower <- lower.bound
	qq$upper <- upper.bound
	dt1 <- rbind(dt1,as.data.frame(qq))
}

for (i in 31:34) {
	lower.bound <- ((((i-30)-1)*5000) -1 )+20000
	upper.bound <- (((i-30)*5000) -1 )+20000
	new.bin[[i]] <- dt[dt$pop.total > lower.bound & dt$pop.total < upper.bound ,]
	qq <- as.list(quantile(new.bin[[i]]$total.AF))
	qq$lower <- lower.bound
	qq$upper <- upper.bound
	dt1 <- rbind(dt1,as.data.frame(qq))
}

names(dt1) <- c("Q0","Lower quartile","Q50","Upper quartile","Q100","lower","upper")

dt1$Q0 <- NULL
dt1$Q100 <- NULL
dt1$Q50 <- NULL

x11()

plot(dt1$lower,dt1$"Upper quartile",col=4,lty=3,xlab="Over 65s",ylab="AF cases")
abline(lm(Upper quartile~lower,data=dt1), col=4) 

points(dt1$lower,dt1$"Lower quartile",col=2,lty=2)
abline(lm(Q25~lower,data=dt1), col=2) 

dt1.melt <- melt(dt1,c(3,4))

x11()

ggplot(dt1.melt, aes(x=(upper+lower)/2, y=value, group=variable, colour=variable)) +
    geom_point(shape=1) +    
    geom_smooth(method=lm, se=F ) +
    ylab("AF cases") +
    xlab("Patients aged 65 and over") +
    opts(legend.position="top", legend.direction="horizontal") +
    scale_colour_discrete("") +
    opts(legend.position=c(.25,0.9)) +
    opts(legend.background = theme_rect(colour = 'grey', fill = 'grey90', size = 1, linetype='solid'))



x11()
ggplot(dt1.melt, aes(x=(upper+lower)/2, y=value, group=variable, colour=variable)) +
    geom_point(shape=1) +    
    geom_smooth(method=lm) +
    ylab("AF cases") +
    xlab("Patients aged 65 and over") +
    opts(legend.position="top", legend.direction="horizontal") +
    scale_colour_discrete("")



