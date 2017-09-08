#灰色预测模型GM(1,1)
#wangmingemail@163.com 2012-3-4
#用法：
#假设数列1 2 3 4 5.5 6 7.5 为已知数据，你要预测后面3项，gm11([1 2 3 4 5.5 6 7.5],10) # 10=7+3
# 序列输入格式为：x<-c(1,2,3,4,5.5,6,7.5)
gm11<-function(x,k)
{
  #x为行向量数据
  #做一次累加
  n<-length(x)
  x1<-numeric(n);
  for(i in 1:n)
  {
    x1[i]<-sum(x[1:i]);
  }
  #x1的均值数列
  z1<-numeric(n)
  m<-n-1
  for(j in 1:m)
  {
    z1[j+1]<-(0.5*x1[j+1]+0.5*x1[j])
  }
  Yn=t(t(x[2:n]))
  B<-matrix(1,nrow=n-1,ncol=2)
  B[,1]<-t(t(-z1[2:n]))
  #solve(M)求M的逆
  #最小二乘法求解参数列
  u<-solve(t(B)%*%B)%*%t(B)%*%Yn;
  a<-u[1];
  b<-u[2];
  #预测
  x2<-numeric(k);
  x2[1]<-x[1];
  for(i in 1:k-1)
  {
    x2[1+i]=(x[1]-b/a)*exp(-a*i)+b/a;
  }
  x2=c(0,x2);
  #还原数据
  y=diff(x2);
  y
}

#调用函数

x<-c(32,36,3,95,1)
gm11(x,8)
