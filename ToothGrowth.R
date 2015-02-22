
library(datasets)
library(ggplot2)
library("gridExtra")
data(ToothGrowth)
library(reshape2)
#
#Look at the data
tg=ToothGrowth
head(ToothGrowth)
#
id = 1:10
tgnew = cbind(id, tg)
tgnew = dcast(tgnew, id + supp ~ dose, value.var = "len")
names(tgnew)[3:5] = c("dose1", "dose2", "dose3")
tgnew = tgnew[order(tgnew$supp),]
d1=tgnew[tgnew[,c("supp")]=="OJ",]
d2=tgnew[tgnew[,c("supp")]=="VC",]
# paired data
oj1=d1[,c("dose1")]
vc1=d2d1[,c("dose1")]

difference <- oj1-vc1
mn <- mean(difference); s <- sd(difference); n =10
mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
t.test(difference)

oj2=d1[,c("dose2")]
vc2=d2d1[,c("dose2")]

difference <- oj2-vc2
mn <- mean(difference); s <- sd(difference); n =10
mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
t.test(difference)

oj3=d1[,c("dose3")]
vc3=d2[,c("dose3")]

difference <- oj3-vc3
mn <- mean(difference); s <- sd(difference); n =10
mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
t.test(difference)


plot1=function(supp)
{
  x=teeth[,c("supp")]==supp
  d1=teeth[x,c("dose1","dose2","dose3")]
  # note you need the mode here or else a dataframe is still returned by as.vector
  c1=as.vector(cumsum(d1)[nrow(d1),],mode="numeric")
  p1=barplot(c1, main=supp, 
          xlab="dose(mg)",col=c("red","green","blue"))
}
d1=teeth[teeth[,c("supp")]=="OJ",]
d2=teeth[teeth[,c("supp")]=="VC",]

t.test(d1$dose1, d2$dose1, paired = FALSE)
t.test(d1$dose2 ,d2$dose2, paired = FALSE)
t.test(d1$dose3, d2$dose3, paired = FALSE)



library(ggplot2)
ggplot(data=ToothGrowth, aes(x=as.factor(dose), y=len, fill=supp)) +
  geom_bar(stat="identity",) +
  facet_grid(. ~ supp) +
  xlab("Dosage(mg)") +
  ylab("Length(mm)") +
  guides(fill=guide_legend(title="Delivery"),
         col=c("blue","green","orange")
         )
require(gridExtra)
plot1 <- qplot(1)
plot2 <- qplot(1)
grid.arrange(plot1, plot2, ncol=2)


######  This is just an example of the side by side plot I want 
library(ggplot2)

# Create first plot and assign it to variable
p1 = qplot(wt, mpg, data = mtcars,
           xlab = 'Car weight', ylab = 'Mileage')

# Create second plot and assign it to variable
p2 = qplot(wt, mpg, color = factor(cyl), data = mtcars,
           geom = c('point', 'smooth'),
           xlab = 'Car weight', ylab = 'Mileage')

# Define grid layout to locate plots and print each graph
pushViewport(viewport(layout = grid.layout(1, 2)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
#
#
counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution", 
        xlab="Number of Gears",col=c("red","green","blue"))


#
#
# I have 4 tables like this:
satu <- array(c(5,15,20,68,29,54,84,119), dim=c(2,4), dimnames=list(c("Negative", "Positive"), c("Black", "Brown", "Red", "Blond")))
dua <- array(c(50,105,30,8,29,25,84,9), dim=c(2,4), dimnames=list(c("Negative", "Positive"), c("Black", "Brown", "Red", "Blond")))
tiga <- array(c(9,16,26,68,12,4,84,12), dim=c(2,4), dimnames=list(c("Negative", "Positive"), c("Black", "Brown", "Red", "Blond")))
empat <- array(c(25,13,50,78,19,34,84,101), dim=c(2,4), dimnames=list(c("Negative", "Positive"), c("Black", "Brown", "Red", "Blond")))
# rbind() the tables together
TAB <- rbind(satu, dua, tiga, empat)
# Do the barplot and save the bar midpoints
mp <- barplot(TAB, beside = TRUE, axisnames = FALSE)
# Add the individual bar labels
mtext(1, at = mp, text = c("N", "P"),
      line = 0, cex = 0.5)
# Get the midpoints of each sequential pair of bars
# within each of the four groups
at <- t(sapply(seq(1, nrow(TAB), by = 2),
               function(x) colMeans(mp[c(x, x+1), ])))
# Add the group labels for each pair
mtext(1, at = at, text = rep(c("satu", "dua", "tiga", "empat"), 4),
      line = 1, cex = 0.75)
# Add the color labels for each group
mtext(1, at = colMeans(mp), text = c("Black", "Brown", "Red", "Blond"), line = 2)