#SUGGESTION 5
## Create a T test Function such that it will only use the numerical variables
#from the dataframe and ignore all the categorical variables
#return a dataframe/table with 2 columns variable name and P-values where 
#the target variable is BINARY


t_Test<-function(data,variable=c(names(data)),target)
{
  Pvalues<-c()
  Variables<- c() 
  Interpretation<- c()
  count= 1
  
  if(length(unique(data[,target])==2))
  {
    for(i in variable)
    {
      if(length(unique(data[,i])) > 5)
      {
        t=t.test(data[,i]~data[,target])
        Pvalues[count]=t$p.value
        Variables[count]=i
        Interpretation=ifelse(Pvalues<0.05,yes = 'Significant',no = 'Not Significant')
        count= count+1
      }
    }
    T_TEST_NUM_CAT<-data.frame(Variables,target,Pvalues,Interpretation)
    View(T_TEST_NUM_CAT)
  }
}

fram <- read.csv('framingham.csv')
t_Test(fram, target = 'TenYearCHD')
t_Test(fram,variable = c('sysBP','totChol'),target = 'TenYearCHD')





#SUGGESTION 6
## Create a Chi-square Function such that it will only use the factor variables
#from the dataframe and ignore all the numerical variables
#return a dataframe/table with 2 columns variable name and P-values

chi_Sq_test <- function(data, variable=c(names(data)), target)
{ 
  Variables <- c()
  Pvalue <-  c()
  Interpretation <- c()
  count=1
  for(i in variable)
  { 
    if((length(unique(data[,i])) < 10) & (i != target))
    {
      c= chisq.test(table(data[,target], data[,i]))
      Pvalue[count] = c$p.value
      Interpretation=ifelse(Pvalue<0.05,yes = 'Significant',no = 'Not Significant')
      Variables[count]=i
      count=count+1
    }
  }
  CHISQ_CAT_CAT <- data.frame(Variables,target, Pvalue,Interpretation)
  View(CHISQ_CAT_CAT)
}

chi_Sq_test(fram,c('diabetes','currentSmoker'),target = 'TenYearCHD')
chi_Sq_test(fram,target = 'TenYearCHD')

#SUGGESTION 7
## Create a rearrange Function such that it will rearrange the columns of the
#dataframe based on the variable types(num, int , fact, char)

rearrange_variables<- function(data)
{
  num=c()
  int=c()
  fact=c()
  char=c()
  
  for(i in 1:ncol(data))
  {
    if(is.numeric(data[,i]))
    {
      if(is.integer(data[,i]))
      {
        int=c(int,i)
      }
      else
      {
        num=c(num,i)
      }
    }
    else if(is.factor(data[,i])) 
    {
      fact = c(fact, i)
    }
    else if(is.character(data[,i]))
    {
      char = c(char, i)
    }
  }
  types = c(num,int,fact,char)
  df = data.frame(data[, types])
  View(df)
} 

str(fram)
rearrange_variables(fram)

#----------------THANK YOU-----------------------------------------