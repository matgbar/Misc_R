################################################################

#TITLE:     Custom Recoding Script

#NOTES:     -Function recodes character variables into factors 
#           -Function allows for the creation of numeric values 
#           -Numeric values are appended to dataset (Or can be saved in new object)

################################################################

#Install forcats if it doesn't exist in your libraries
if(!require(forcats)) install.packages('forcats')

#Reoving possible NAs (either blank cells or only including a space)
dat[dat=='']<-NA
dat[dat==' ']<-NA
#Custom function

custom_recode<-function(DF, vars=NULL, new_levels=NULL, make_int=TRUE){
  #browser()
  if(length(vars)>1){
    for(i in 1:length(vars)){
      if(length(unique(!is.na(DF[,vars[i]])))==length(new_levels)){
        DF[,vars[i]]<-forcats::fct_relevel(DF[,vars[i]], new_levels)
        if(make_int==TRUE){
          DF[,paste0(vars[i], '_num')]<-as.numeric(DF[,vars[i]])
          DF[,paste0(vars[i], '_num')][DF[,vars[i]]=='Refused']<-NA
        } 
      }
      else print(paste(vars[i], 'does not match new_levels'))
    }
  }
  else{
    DF[,vars]<-forcats::fct_relevel(DF[,vars], new_levels)
    if(make_int==TRUE){
      DF[,paste0(vars, '_num')]<-as.numeric(DF[,vars])
      #Removes Refused from the numeric variables
      DF[,paste0(vars, '_num')][DF[,vars]=='Refused']<-NA
    } 
  }
  return(DF)
  #NOT RUN:
  
  #The way this works: 
  #1. Specify the new order of levels that you want: 
  #new.levels<-c('Never', 
  #              'Seldom', 
  #              'Once or twice a month', 
  #              'A few times a week', 
  #              'Every day', 
  #              'Refused')
  
  #2. Specify the variables that you want to recode 
  #   These variables should all have the same levels 
  #   You should also only select variables that you want to have the same factor levels 
  
  #sel_vars<-'q1'
  #dat<-custom_recode(DF=dat, vars = sel_vars, new_levels = new.levels, make_int = TRUE)
  
  #If you do have more than one variable you want to recode 
  #new.levels<-c('No', 
  #              'Yes')
  
  #sel_vars<-paste0('q7', letters[1:7])
  #dat<-custom_recode(DF=dat, vars = sel_vars, new_levels = new.levels, make_int = TRUE)
  
  #Hopefully you'll be able to get this code working for you 
}