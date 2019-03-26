chisq_pdf_gen = function(df){
  plot(0,0,type="n",xlim=c(0, max(df)*2),ylim=c(0,0.7), 
       xlab="x",ylab="f(x)", main = paste("卡方分配PDF"))
  for (i in 1:length(df)){#不同自由度畫圖
    x = seq(0, max(df)*2, by = .01)
    fx = dchisq(x, df[i])
    lines(x,fx, col = i)
  }
  legend("right", legend = c(df),col = 1:length(df), lty = 1, title = "自由度")
}
t_pdf_gen = function(df){
  plot(0,0,type="n",xlim=c(-5, 5),ylim=c(0,0.5), 
       xlab="x",ylab="f(x)", main = paste("t分配PDF"))
  for (i in 1:length(df)){#不同自由度畫圖
    x = seq(-5, 5, by = .01)
    fx = dt(x, df[i])
    lines(x,fx, col = i)
  }
  legend("right", legend = c(df),col = 1:length(df), lty = 1, title = "自由度")
}



#prob 1
1.
df = c( 1, 2, 5, 10, 30, 50)
chisq_pdf_gen(df)

#2.
df = c( 1, 2, 5, 10, 30, 50)
t_pdf_gen(df)

#prob 2
n = c(500, 5000)
m = 2000
df = c(1)
seed = 7051
#相對次數
set.seed(seed)
par(mfrow = c(2, 1))
for (i in 1:length(n)){#每次從Population抽n個值
  res = c()
  for (j in 1:m) #抽m次
  {
    res[j] = mean(rt(n[i], df = df))
  }
  hist(res, ylab = "相對次數",
       main = paste("t(1)抽樣 m =", m, "df = ", df, "n = ", n[i]))
}

#pdf
r_t_gen = function(m, n, df, seed = NULL){
  #設定種子碼
  if(is.null(seed) == F)
  {
    set.seed(seed)
  }
  #設定圖範圍
  plot(0,0,type="n",xlim=c(-8, 8),ylim=c(0, 2), 
       xlab="Density estimate",ylab="f(x)", main = paste("t分配抽樣 m =", m, "df = ", df))
  for (i in 1:length(n)){#每次從Population抽n個值
    res = c()
    for (j in 1:m) #抽m次
    {
      res[j] = mean(rt(n[i], df = df))
    }
    lines(density(res), col = i)
  }
  legend("right", legend = c(n),col = 1:length(n), lty = 1, title = "樣本數")
}
r_t_gen(m = m, n = n, df = df, seed = seed)

#using r

#7.12
m = 50; n=20; p = .5; # toss 20 coins 50 times,
alpha = 0.10
zstar = qnorm(1-alpha/2)
set.seed(7051)
phat = rbinom(m,n,p)/n # divide by n for proportions
SE = sqrt(phat*(1-phat)/n) # compute SE
sum(phat - zstar*SE < p & p < phat + zstar * SE)/m
#graph
matplot(rbind(phat - zstar*SE, phat + zstar*SE), rbind(1:m,1:m),type="l",lty=1)
abline(v=p, ) # indicate parameter value
axis(1, at = p, labels = "p")

#7.14
ibrary(UsingR)
install.packages('BSDA')
library(BSDA)
z.test
alpha = 0.90
(z = qnorm(1 - alpha/2))
(mean = mean(stud.recs$sat.m))
SE = sd(stud.recs$sat.m)/sqrt(length(stud.recs$sat.m))
mean + z*SE
#7.16

#7.20

#7.22


