
library(MASS)
?Boston
#SUGGESTION 1:
#Often we are not required the graphs for all the numeric variables. Try to improve the code
#by adding an additional parameter "variable" that can take a vector of variable index and 
#return the graphs for only those variables.
#
#Example: Graphs(Boston, var=c(1,3,4))
#Will generate the graphics for only the numerical variables among the variables 1,3 & 4
#in the data Boston
vars=c()
Column_name=c("crim","age","tax")
for(i in 1:length(Column_name))
{
  
  z<-grep(Column_name[i], colnames(Boston))
  vars[i]=z
}


Graphs <- function(data,dfcol)
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in 1:length(dfcol))
  {
    j<-dfcol[i]
    if(is.numeric(data[,j]))
    {
      
      png(paste(names(data)[j], ".png", sep="")) #NOTE this step
      
      par(mfrow=c(2,1))
      boxplot(data[,j], main = paste("Boxplot of", names(data)[j]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,j], main = paste("Histogram of", names(data)[j]), 
           xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)
      
      
      
      dev.off()  #NOTE this step
      
    }
    
  }
}
setwd("C:\\Users\\User\\Documents\\DataScience\\Semester2\\R\\Assignment\\Univariate\\Q1")
Graphs(Boston,vars)

#-------------------------------------------------------------------------------

#SUGGESTION 2:
#Improve the code in suggestion 1 such that if the argument variable is ignored then it will
#return the graphs of all the numeric variables in the data by default.
Graphs <- function(data,dfcol)
{
  
  if(length(dfcol)==0)
  {
    for(i in 1:ncol(data))
    {
      if(is.numeric(data[,i]))
      {
        
        png(paste(names(data)[i], ".png", sep="")) #NOTE this step
        
        par(mfrow=c(2,1))
        boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
                ylab = names(data)[i], col = "maroon", border = "grey5",
                horizontal = T)
        
        hist(data[,i], main = paste("Histogram of", names(data)[i]), 
             xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)
        
        
        
        dev.off()  #NOTE this step
        
      }
      
    }
    
  }
  else
  {
    for(i in 1:length(dfcol))
    {
      j<-dfcol[i]
      if(is.numeric(data[,j]))
      {
        png(paste(names(data)[j], ".png", sep="")) #NOTE this step
        par(mfrow=c(2,1))
        boxplot(data[,j], main = paste("Boxplot of", names(data)[j]), 
                ylab = names(data)[i], col = "maroon", border = "grey5",
                horizontal = T)
        hist(data[,j], main = paste("Histogram of", names(data)[j]), 
             xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)
        dev.off()  #NOTE this step
        
      }
      
    }
  }
}
setwd("C:\\Users\\User\\Documents\\DataScience\\Semester2\\R\\Assignment\\Univariate\\Q2")

vars=c()
Graphs(Boston,vars)

#------------------------------------------------------------------------------

#SUGGESTION 3:
#We ignored the cateorical variables in our discussion. Make some improvement in your codes
#in suggestion 2 such that the function will take the argument "data" and "variable" and will
#return boxplots & histograms for the numerical variables and barplots and pie charts for
#the categorical variables.
#
#Example:
#Graphs(mtcars)
#will get the necessary graphics for all numeric variables and categorical variables in the
#data

getwd()                                 #get the working directory
setwd("C:\\Users\\User\\Documents\\DataScience\\Semester2\\R")  #set the working directory
fram = read.csv("framingham.csv") 
car=read.csv("cars.csv")
factor(fram$male)
class(fram$male)
is.xcharacter(fram$male)


Graphs <- function(data)
{
  for(i in 1:ncol(data))
  {
    if(is.factor(data[,i])|is.character(data[,i]))
    {
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step
      par(mfrow=c(2,1))
      barplot(prop.table(table(data[,i])),col=c('gold','magenta','maroon'))
      #barplot(table(data[,i]))
      percent=100*table(data[,i])/length(data[,i])
      pie(x=percent, label=paste(percent, "%"),  col=rainbow(length(names)), main=paste("Percentage of", names(data)[i]) )
      
      
      
      
      
    }
    else
    {
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step
      
      par(mfrow=c(2,1))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]), 
           xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)
      
      
      dev.off()  #NOTE this step
    }
    
  }
  
}
setwd("C:\\Users\\User\\Documents\\DataScience\\Semester2\\R\\Assignment\\Univariate\\Q3")

Graphs(car)

#-------------------------------------------------------------------------------


#SUGGESTION 4:
#Probably you need not want to mess up your working directory with so many image files...
#Create an additional argument for the function "dir" (directory), such that the function
#exports all the files to your specified folder (which need not necessaryly be your working
#directory).
#
#Example:
#Graphs(Boston, Variable = c(1,3,4), dir = ".../Praxis/LearntSometingNew/Graphs")
#will generate the necessary graphics for the variables 1, 3 and 4 in the specified location
#in your system i.e. ".../Praxis/LearntSometingNew/Graphs"
data("Boston")

vars=c()
Column_name=c("crim","age","tax")
for(i in 1:length(Column_name))
{
  
  z<-grep(Column_name[i], colnames(Boston))
  vars[i]=z
}


Graphs <- function(data,dfcol,dir)
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in 1:length(dfcol))
  {
    j<-dfcol[i]
    if(is.numeric(data[,j]))
    {
      
      png(paste(names(data)[j], ".png", sep="")) #NOTE this step
      
      par(mfrow=c(2,1))
      boxplot(data[,j], main = paste("Boxplot of", names(data)[j]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,j], main = paste("Histogram of", names(data)[j]), 
           xlab = names(data)[i], ylab = names(data)[i], col = "lightgreen", border=F)
      
      
      setwd(dir)
      dev.off()  #NOTE this step
      
    }
    
  }
}
#setwd("C:\\Users\\User\\Documents\\DataScience\\Semester2")
Graphs(Boston,vars,"C:\\Users\\User\\Documents\\DataScience\\Semester2\\R\\Assignment\\Univariate\\Q4")
 