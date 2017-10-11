library(MASS)  # in case it is not already loaded 
set.seed(101)
n <- 1000
X <- mvrnorm(n, mu=c(.5,2.5), Sigma=matrix(c(1,.6,.6,1), ncol=2))

## some pretty colors
library(RColorBrewer)
k <- 11
my.cols <- rev(brewer.pal(k, "RdYlBu"))

## compute 2D kernel density, see MASS book, pp. 130-131
z <- kde2d(X[,1], X[,2], n=50)

plot(X, xlab="X label", ylab="Y label", pch=19, cex=.4)
contour(z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE)
abline(h=mean(X[,2]), v=mean(X[,1]), lwd=2)
legend("topleft", paste("R=", round(cor(X)[1,2],2)), bty="n")



library(MASS)
library(ggplot2)
n <- 1000
x <- mvrnorm(n, mu=c(.5,2.5), Sigma=matrix(c(1,.6,.6,1), ncol=2))
df = data.frame(x); colnames(df) = c("x","y")

commonTheme = list(labs(color="Density",fill="Density",
                        x="RNA-seq Expression",
                        y="Microarray Expression"),
                   theme_bw(),
                   theme(legend.position=c(0,1),
                         legend.justification=c(0,1)))


boxplot(density(ols.models$t[,3:9]))

library(reshape2); density.table <- data.frame(Var.Name= double(), Var.X = double(), Var.Y = double())
for(i.lag in 0:6){
  density.table <- rbind(density.table,data.frame(Var.Name = i.lag, Var.X = density(ols.models$t[,3+i.lag])$x,Var.Y=density(ols.models$t[,3+i.lag])$y)) }

par(mfrow=c(1,1))
boxplot(data=density.table)
par(mfrow=c(1,2))
density.reshape <- dcast(density.table, Var.X ~ Var.Name, value.var = "Var.Y", fun=mean) 

par(mfrow=c(1,1))
plot(density(ols.models$t[,4]),col=rgb(0,0,1,0.2),ylim=c(0,15),xlim=c(-0.5,0.5),lwd=3)
for(i.chart in 1:6){lines(density(ols.models$t[,4+i.chart]),col=rgb(1/6*i.chart,1/i.chart,1,0.2),lwd=3,lty=i.chart)}

plot(density.reshape$Var.X, density.reshape$`1`,xlim=c(-0.15,0.55))

p <- ggplot(density.table, aes(x=reorder(Var.Name,Var.X),y=Var.Y))

p <- ggplot(ols.models$t)

p <- ggplot(density.table,aes(Var.Y))
p + geom_density()

library(data.table)
density.reshape <- as.data.table(lapply(na.locf(density.reshape),as.numeric)); density.reshape <- replace(density.reshape,is.na(density.reshape),0)
plot(density.reshape$Var.X,density.reshape$`6`)


p <- ggplot(density.re``)

ols.reshape <- ""
mycols <- topo.colors(100,0.5)
#Plot the density
par(mfrow=c(1,1))

#y seq
y.seq <- seq(min(density.reshape$Var.X),max(density.reshape$Var.X),((max(density.reshape$Var.X)-min(density.reshape$Var.X))/nrow(density.reshape)))[-1]

7*length(y.seq)
length.()

boxplot(density.reshape[,2:8])

z = array(unlist(as.matrix(density.reshape[,2:8])),dim=c(length(0:6),length(density.reshape$Var.X)))
image(x=0:6,y=density.reshape$Var.Y,z=t(density.reshape[,2:8]),col=mycols,
      main="Hello")
contour(0:6,density.reshape$Var.X,as.matrix(t(as.matrix(density.reshape[,2:8]))),add=T)
lines(MA.renr.reshape$BestLag,1:months.to.analyse,type="l",col="red",lwd=2)

x = 0:6
y= as.numeric(density.reshape$Var.X)


x=1:20
y=1:10

z=array(seq(1,2000,1),dim=c(length(y),length(x)))
filled.contour(y,x,z,plot.title=title(main="Test",xlab="X",ylab="Y")  )

ggplot()

ggplot(data=df,aes(x,y)) + 
  geom_density2d(aes(colour=..level..)) + 
  scale_colour_gradient(low="green",high="red") + 
  geom_point() + commonTheme


ggplot(data=df,aes(x,y)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point() + commonTheme


###Bank of England River of Blood chart
library(fanplot)
head(boe)
y0 <- 2013
boe0 <- subset(boe,time0==y0)
k <- nrow(boe0)

p <- seq(0.05,0.95,0.05) #set percentiles to plot
p <- c(0.01,p,0.99)

#quantiles of split-normal dist for each row and each point
cpival <- matrix(NA, nrow=length(p),ncol=k)
for(i in 1:k){
  cpival[,i] <- qsplitnorm(p,mode=boe0$mode[i],
                           sd = boe0$uncertainty[i],
                           skew=boe0$skew[i])}

#columsn - years
#rows - probabilities
plot(cpi, type = "l", col = "tomato", lwd = 2, 
     xlim = c(y0 - 5, y0 + 3), ylim = c(-2, 7), 
     xaxt = "n", yaxt = "n", ylab="")
# background
rect(y0 - 0.25, par("usr")[3] - 1, y0 + 3, par("usr")[4] + 1, 
     border = "gray90", col = "gray90")
#add fan
fan(data = cpival, data.type = "values", probs = p, 
    start = y0, frequency = 4, 
    anchor = cpi[time(cpi) == y0 - 0.25], 
    fan.col = colorRampPalette(c("tomato", "gray90")),  
    ln = NULL, rlab = NULL)

# boe aesthetics
axis(2, at = -2:7, las = 2, tcl = 0.5, labels = FALSE)
axis(4, at = -2:7, las = 2, tcl = 0.5)
axis(1, at = 2008:2016, tcl = 0.5)
axis(1, at = seq(2008, 2016, 0.25), labels = FALSE, tcl = 0.2)
abline(h = 2)  #boe cpi target
abline(v = y0 + 1.75, lty = 2)  #2 year line
