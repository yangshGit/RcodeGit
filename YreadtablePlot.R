getwd()
setwd("E:/Rcode/RcodeGit");getwd()

library(ggplot2)
a=read.table("data/1.txt",header=TRUE)
plot(a$a)
plot(density(a$a),lwd=2)
plot(density(a$a,bw = 'SJ'),lwd=2)
plot(density(a$a,bw = 'SJ',from = 0),lwd=2)
plot(density(a$a,from = 0,bw = 0.1,n=100),lwd=2)
plot(density(a$a,bw = "SJ")) #更敏感真实
#密度曲线
qplot(b,data=a,geom = "density",main = "density",bw="SJ")
ggplot(data=a,aes(x=b))+geom_density(bw='SJ')
ggplot(data=a,aes(x=b))+geom_density(color="red",size=2,n=100,bw=0.1,show.legend = TRUE)
#bandwith=0.1统计窗口为0.1，作图点的个数为100

plot(a$a,a$b)
qqplot(a$a,a$b)
############################################

#readxls
library(readxl)
cyp<- read_excel("data/stat-test.xlsx",sheet = "统计详细", col_names = TRUE)
View(cyp)
plot(density(cyp$样本,na.rm=TRUE,bw=10,n=300,to=53000))
plot(density(cyp$样本,na.rm=TRUE,bw=100,n=300,to=53000))
#bw bandwidth统计带宽，n作图点个数，作图到X=100(bw越大，n越大，则越平滑)
#真实的bw=bw的值*adjust

plot(density(cyp$样本),lwd=6)
library(ggplot2)
qplot(样本,data=cyp,geom = "density",main = "density",lwd=6)
ggplot(data=cyp,aes(x=样本))+geom_density(color="red",size=2)
ggplot(data=cyp,aes(x=样本))+stat_density(color="red",size=2,n=100,bw=10)

matplot(cyp$样本)
?matplot
####################################
library(readxl)
somatic<- read_excel("data/59717somatic.xlsx",sheet = "Mutation", col_names = TRUE)
View(somatic)
plot(density(somatic$xjratio),lwd=2)
plot(density(somatic$xjratio,bw = 'SJ'),lwd=2)
plot(density(somatic$xjratio,bw = 'SJ',from = 0),lwd=2)
plot(density(somatic$xjratio,from = 0,bw = 0.01,n=100),lwd=2)
plot(density(a$a,bw = "SJ")) #更敏感真实
#密度曲线
qplot(xjratio,data=somatic,geom = "density",main = "density",bw="SJ",color="red")+xlim(-0.2,1.2)
ggplot(data=somatic,aes(x=xjratio))+geom_density(bw='SJ')+xlim(-0.2,1.2)
ggplot(data=somatic,aes(x=xjratio))+geom_density(color="red",size=2,n=1000,bw=0.01,show.legend = TRUE)+xlim(-0.2,1.2)
#bandwith=0.1统计窗口为0.1，作图点的个数为100


#########################

b=read.table("data/2.txt",header=TRUE)
plot(b$POS)
plot(density(b$POS))
plot(b$CHROM,b$POS)

c=read.table("data/3.vcf",header=TRUE)
plot(c$POS)
plot(density(c$POS))
plot(c$CHROM,c$POS)
########################

##ggplot  http://www.17bigdata.com/%E7%94%A8%E4%BA%8E%E7%BB%98%E5%9B%BE%E7%9A%84r%E8%AF%AD%E8%A8%80%E6%89%A9%E5%B1%95%E5%8C%85%EF%BC%9Aggplot2.html
#library(ggplot2)
p <- ggplot(data=mpg,aes(x=displ,y=hwy,colour=factor(cyl)))
p + geom_point() 
p + geom_smooth()
p + geom_point() + geom_smooth()

p <- ggplot(mpg,aes(x=displ,y=hwy))
p + geom_point(aes(colour=factor(cyl))) + geom_smooth()

library( ggplot2)
p <- ggplot(data = mpg,aes(x = hwy))
p <- p + geom_histogram()
summary(p)
#图层控制与直方图
p <- ggplot(mpg,aes(hwy))
p + geom_histogram(position = 'identity',
                   alpha=0.5,
                   aes(y = ..density..,
                       fill = factor(year))) +
  stat_density(geom = 'line',
               position = 'identity',
               aes(colour = factor(year)))
#散点图
library(ggplot2)
p <- ggplot(mpg, aes(cty, hwy))
p1 <- p + geom_point(aes(colour = factor(year),shape = factor(year), size = displ), alpha = 0.6, position = 'jitter')
print(p1)

ggplotgui::ggplot_shiny(dataset = c)
## others
library(class)
data(iris)
names(iris)
m1<-knn.cv(iris[,1:4],iris[,5],k=3,prob=TRUE)
attributes(.Last.value)
library(MASS)
m2<-lda(iris[,1:4],iris[,5]) # 与判别分析进行比较
b<-data.frame(Sepal.Length=6,Sepal.Width=4,Petal.Length=5,Petal.Width=6)
p1<-predict(m2,b,type="class")

##qplot
library(ggplot2)
x <- 1:1000
y <- rnorm(n = 1000,mean = 0,sd = 1)
plot(density(y))
plot(x, y, main="Scatter plot by plot()")
qplot(x,y, main="Scatter plot by qplot()")
qplot(a$a, a$b)

str(str)
t=head(a,n = 5)
t.sam=t[sample(5,4,replace=TRUE),seq(1,2)]
print(t.sam)

set.seed(1000) # 设置随机种子，使随机取样具有可重复性
datax<- diamonds[sample(53940, 500), seq(1,7)]
head(datax, 4)
plot(x=datax$carat, y=datax$price, xlab="Carat", ylab="Price", main="plot function")
qplot(x=carat, y=price, data=datax, xlab="Carat", ylab="Price", main="qplot function")
#分类作图
theme_set(theme_gray(base_size = 10))
qplot(x=carat, y=price, data=datax, color=cut, shape=cut, main="qplot function")

qplot(x=carat, y=price, data=datax, color=cut, geom="line", main="geom=\"line\"")
qplot(x=carat, y=price, data=datax, color=cut, shap=cut,geom=c("line", "point"), main="geom=c(\"line\", \"point\")")


qplot(carat, price, data = diamonds, color=cut, geom = "smooth", main = "smooth")
qplot(cut, price, data = diamonds, fill=cut, geom = "boxplot", main = "boxplot")
qplot(price, data = diamonds, fill=cut, geom = "histogram", main = "histogram")
qplot(price, data = diamonds, color=cut, geom = "density", main = "density")

qplot(price, data = diamonds, color=cut, geom = "bar", main = "bar")
qplot(cut, price,data = diamonds, color=cut, geom = "point", main = "point")
qplot(cut, price, data = diamonds, color=cut, geom = "line", main = "line")
qplot(cut, price, data = diamonds, color=cut, geom =  c("jitter"), main = "jitter")
qplot(cut, price, data = diamonds, color=cut, geom =  c("jitter","boxplot"), main = "jitter")

library(ggplot2)
sale=read.table("saleN.txt",header=TRUE)
saleN <- factor(sale$N)
str(sale)
print(sale)
###fill color必须是fator因子
qplot(a,b, data = sale,  fill=a, geom = "boxplot", main = "boxplot")
qplot(b, data = sale, color=a, geom = "density", main = "density")
qplot(b, data = sale, color=saleN, geom = "density", main = "density")


