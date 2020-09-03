#SUGGESTION 1:
#Often we are not required the graphs for all the numeric variables. Try to improve the code
#by adding an additional parameter "variable" that can take a vector of variable index and 
#return the graphs for only those variables.
#
#Example: Graphs(Boston, var=c(1,3,4))
#Will generate the graphics for only the numerical variables among the variables 1,3 & 4
#in the data Boston

library(MASS)

Graphs_s1 <- function(data,var)
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in var)
  {
    if(is.numeric(data[,i]))
    { 
      par(mfrow=c(2,1))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]), 
           xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)
      
    }
  }
}

cars= read.csv('cars (1).csv')
View(cars)
Graphs_s1(cars,var = c(2,4,9))
Graphs_s1(Boston,var=c(2,5,4))

#SUGGESTION 2:
#Improve the code in suggestion 1 such that if the argument variable is ignored then it will
#return the graphs of all the numeric variables in the data by default.

Graphs_s2 <- function(data, var=1:ncol(data))
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in var)
  {
    if(is.numeric(data[,i]))
    {
      par(mfrow=c(2,1))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]), 
           xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)
      
    }
  }
}

Graphs_s2(cars)


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

Graphs_s3 <- function(data,var=1:ncol(data))
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in var)
  {
    if(is.numeric(data[,i]))
    {
      par(mfrow=c(2,1))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]), 
           xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)
    } 
    else
    {
      par(mfrow=c(2,1))
      barplot(table(data[,i]), main = paste("Barplot of", names(data)[i]), 
              xlab = names(data[]),ylab = 'Number of houses', col = "maroon")
      
      pie(table(data[,i]),main=paste("Piechart of",names(data)[i]),radius = 1,
          clockwise = T)
    }
  }
}

Graphs_s3(cars)


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


Graphs_s4 <- function(data,var=1:ncol(data),dir)
{
  setwd(dir)
  
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in var)
  {
    if(is.numeric(data[,i]))
    {
      png(paste(names(data)[i], ".png", sep=""))
      
      par(mfrow=c(2,1))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]), 
           xlab = names(data)[i], ylab = "Frequency", col = "lightgreen", border=F)
      dev.off()
    
    }
    else
    {
      png(paste(names(data)[i], ".png", sep=""))
      par(mfrow=c(2,1))
      barplot(table(data[,i]), main = paste("Barplot of", names(data)[i]), 
              xlab = names(data[]),ylab = 'Frequency', col = "maroon")
      
      pie(table(data[,i]),main=paste("Piechart of",names(data)[i]),radius = 1,
          clockwise = T,)
      
      dev.off()
    }
  }
}

Graphs_s4(iris,dir = ("C:/Users/balla/OneDrive/Pictures/Screenshots/"))

#--------------------THANK YOU---------------------------------#