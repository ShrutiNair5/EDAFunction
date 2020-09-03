
#============considering all values for T-Test and chi test=====================
getwd()                                 #get the working directory
setwd("C:\\Users\\User\\Documents\\DataScience\\Semester2\\R")  #set the working directory
fram = read.csv("framingham.csv") 
car=read.csv("cars.csv")
num=c()
fac=c()
inte=c()

edaFunc<-function(data)
{
  
  for(i in 1:ncol(data))
  {
    if(is.numeric(data[,i]))
    {
      num<-append(num,i)
    }
    else if(is.factor(data[,i]))
    {
      fac<-append(fac,i)
    }
    
  }
  
  print("Numeric Variable")
  for(i in 1:length(num))
  {
    if(i==length(num))
    {
      break
      
    }
    h<-length(num)-i
    for(j in i+1:h)
    {
      # print(i)
      #print(j)
      m<-num[i]
      n<-num[j]
      #print(m)
      #print(n)
      
      pval<-t.test(data[,m],data[,n])$p.value
      if(pval<0.05)
      {
        print(paste("t test column",names(data)[m],"with column",names(data)[n],pval))
      }
    }
  }
  print("Categorical Variable")
  for(i in 1:length(fac))
  {
    if(i==length(fac))
    {
      break
      
    }
    h<-length(fac)-i
    for(j in i+1:h)
    {
      # print(i)
      #print(j)
      c<-fac[i]
      d<-fac[j]
      #print(m)
      #rint(n)
      pval<-chisq.test(data[,c],data[,d],correct=FALSE,simulate.p.value=TRUE)$p.value
      if(pval<0.05)
      {
        print(paste("chi sq test column",names(data)[c],"with column",names(data)[d],pval))
      }
    }
  }
}

edaFunc(car)


#==================Taking input from user about t-test and chi-test=================



car=read.csv("cars.csv")
num=c()
fac=c()
inte=c()
#<-grep("MPG", colnames(car))
edafuncn<-function(data,col=c(),target)
{
  vars=c()
  for(i in 1:length(col))
  {
    z<-grep(col[i], colnames(data))
    vars[i]=z
  }
  for(i in 1:length(vars))
  {
    j<-vars[i]
    if(is.numeric(data[,j]))
    {
      num<-append(num,j)
    }
    else if(is.factor(data[,j]))
    {
      fac<-append(fac,j)
    }
    
  }
  y<-grep(target[1], colnames(data))
  print("Numeric Variable")
  if(length(num)==0)
  {
    print("No numerical variable")
  }
  else
  {
    for(i in 1:length(num))
    {
      if(i==length(num))
      {
        m<-num[i]
        pval<-t.test(data[,m],data[,y])$p.value
        if(pval<0.05)
        {
          
          print(paste("t test column",names(data)[m],"with column",names(data)[y],pval))
        }
        break
      }
      else
      { # print(i)
        #print(j)
        m<-num[i]
        
        #print(m)
        #rint(n)
        pval<-t.test(data[,m],data[,y])$p.value
        if(pval<0.05)
        {
          print(paste("t test column",names(data)[m],"with column",names(data)[y],pval))
        }
      }
    }
  }
  print("Categorical Variable")
  if(length(fac)==0)
  {
    print("No categorical variable")
  }
  else
  {
    for(i in 1:length(fac))
    {
      if(i==length(fac))
      {
        m<-fac[i]
        pval<-pval<-chisq.test(data[,m],data[,y],correct=FALSE,simulate.p.value=TRUE)$p.value
        if(pval<0.05)
        {
          
          print(paste("chi sq test column",names(data)[m],"with column",names(data)[y],pval))
        }
        break
      }
      else
      { # print(i)
        #print(j)
        m<-fac[i]
        
        #print(m)
        #rint(n)
        pval<-pval<-chisq.test(data[,m],data[,y],correct=FALSE,simulate.p.value=TRUE)$p.value
        if(pval<0.05)
        {
          print(paste("chi sq test column",names(data)[m],"with column",names(data)[y],pval))
        }
      }
    }
  }
  
}
v=c()


ans='y'
while(ans=='y')
{
  ans<- readline(prompt="Do you want to continue")
  if(ans=='n')
  {
    break 
  }
  m<- readline(prompt="Enter variable ")
  v<-append(v,m)
  
  #ans<- readline(prompt="Do you want to continue")
}
y
ya=c()
n<- readline(prompt="Enter taget ")
ya<-append(ya,n)

edafuncn(car,v,ya)


#==================== Rearranging columns ============================


setwd("C:\\Users\\User\\Documents\\DataScience\\Semester2\\R")  #set the working directory
fram = read.csv("framingham.csv") 
car=read.csv("cars.csv")
num=c()
fac=c()
inte=c()
r=c()
Newdf=data.frame()
rearrange<-function(data)
{
  for(i in 1:ncol(data))
  {
    if(is.numeric(data[,i]))
    {
      num<-append(num,i)
    }
    
    else if(is.factor(data[,i]))
    {
      fac<-append(fac,i)
    }
    else 
    {
      inte<-append(inte,i)
    }
  }
  r<-append(r,num)
  r<-append(r,fac)
  r<-append(r,inte)
  data <- data[r]
  View(data)
  
}
rearrange(car)



















