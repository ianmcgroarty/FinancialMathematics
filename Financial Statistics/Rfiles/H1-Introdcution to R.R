# Module 1, Lecture 8: Intro to R
2+2
exp(-2)
x <- 2
x
x+x
x/x
x*x
exp(x)
log(exp(x))
10^x

weight<-c(60,72,57,90,72)
weight[2]
weight[-2]
weight<-c(weight[1:4],95,weight[5])
weight[-c(2:4)]
height<-c(1.75,1.8,1.65,1.9,1.74,1.91)
bmi<-weight/height^2

bmi>20
sum(bmi>20)
!(bmi>20)
sum(!bmi>20)
weight
bmi
bmi>20 | weight>=60
bmi>20 & weight <60
sum(bmi>20&weight<60)
max(bmi)
min(bmi)
range(bmi)
sd(bmi)
mean(bmi)


x<-1:4
x<-seq(2,20,2)
x<-rep(c(1,2,3),3)
x
x1<-1:4
x2<-5:8
x3<-9:12
mat1<-cbind(x1,x2,x3)
mat1
mat2<-rbind(x1,x2,x3)
mat2

t(mat2)
matrix(c(1:12),nrow=4)
rowSums(mat1)
rowMeans(mat1)
mat2%*%mat1
mat2%*%mat1[,3]>200
solve(mat2%*%mat1)
noise<-matrix(runif(9,0,10),nrow=3)
solve(mat2%*%mat1+noise)


#create a function
test<-function(x,a){
    sum(x^a)
}
test
test(c(2,3),1)
#4+9 = 13
test(c(2,3),2)
