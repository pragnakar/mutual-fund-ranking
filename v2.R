rm(list=ls())


# select data source 

#########################################################################################

notes = "
data set contains 

1) name of mutual fund 
2) Fund symbols
3) 5 year return
4) 2 year return
5) 1 year return
6) assests in billions
7) expense ratio

Rank  Reciprocal  method is used to  calculate weights and recommend  mutual fund to invest in 
"



####################################################################################

cat(" select the appropriate data set  \n")
mf_data = read.csv( file.choose())
#names of measures
measure_names<-function(){
  cat( ' measure 1: 5 year return  \n')
  cat( ' measure 2: 2 year return  \n')
  cat( ' measure 3: 1 year return  \n')
  cat( ' measure 4: assests in billions \n')
  cat( ' measure 5: expense ratio  \n')
}

# display weights 
display_weights<- function(rank_df){
  for(  i in 1:5){
    cat("\n measure ", i ," weight", rank_df$weight[i])
  }
}


#modify weights 
modify_weights<-function(rank_df){
  measure_names()
  display_weights(rank_df)
  cat("\n enter the measure you want to modify \n")
  i= as.numeric(readline())
  
  
  cat( "\n do you want to modify meassure ", i," [ y, n]:" )
  input =readline()
  if( input == 'y'){
    cat(" enter new weight \n")
    new_weight= as.numeric(readline())
    old_weight=rank_df$weight[i]
    difference_weight = new_weight - old_weight
    difference_weight = difference_weight/(nrow(rank_df)-1)
    rank_df$weight<- rank_df$weight - difference_weight
    rank_df$weight[i]=new_weight
    cat("new weights \n")
    measure_names()
    display_weights(rank_df)
  }
  
  return(rank_df)
}

# rank-

rank<- function(){
  ranks<-c()
  measure_names()
  for( i in 1:5){
    cat("enter rank for measure ", i, ': ')
    a = readline()
    
    ranks<-c(ranks,a)
  }
  rank_df<-data.frame(ranks)
  
  rank_df<- transform(rank_df, ranks = as.numeric(ranks))
  
  rank_df$r.inverse <- rank_df$ranks ^-1
  rank_df$weight<- rank_df$r.inverse/sum(rank_df$r.inverse)
  return(rank_df)
}
#normalizing measures 
transform_data<- function( mf_data){

mf<- transform(mf_data,X5.year.return = as.numeric( (X5.year.return - min(X5.year.return))/(max(X5.year.return)-min(X5.year.return))))#higher better
mf<- transform(mf,X3.year.return = as.numeric( (X3.year.return - min(X3.year.return))/(max(X3.year.return)-min(X3.year.return))))#higher better
mf<- transform(mf,X1.year.return = as.numeric( (X1.year.return - min(X1.year.return))/(max(X1.year.return)-min(X1.year.return))))#higher better
mf<- transform(mf,assests = as.numeric( (assests - min(assests))/(max(assests)-min(assests))))#higher better
mf<- transform(mf,Expense.Ratio = as.numeric( (max(Expense.Ratio)- Expense.Ratio)/(max(Expense.Ratio)-min(Expense.Ratio)))) #lower better 
return( mf)
}

# applying weights 

apply_weights <- function( mf,rank_df){
  mf_weights <- mf
  mf_weights <- transform(mf_weights, X5.year.return = mf_weights$X5.year.return * rank_df$weight[1])
  mf_weights <- transform(mf_weights, X3.year.return = mf_weights$X3.year.return * rank_df$weight[2])
  mf_weights <- transform(mf_weights, X1.year.return = mf_weights$X1.year.return * rank_df$weight[3])
  mf_weights <- transform(mf_weights, Expense.Ratio  = mf_weights$Expense.Ratio * rank_df$weight[5])
  mf_weights <- transform(mf_weights, assests = mf_weights$assests * rank_df$weight[4])
  
  mf_weights$score<- mf_weights$X5.year.return + mf_weights$X3.year.return + 
    mf_weights$X1.year.return + mf_weights$Expense.Ratio + mf_weights$assests
  return( mf_weights)
  
}

#generate ranking 
ordering<-function(mf_weights){
  
  temp<- data.frame(subset(mf_weights,select = c(1,2,8)))
  order <-temp[rev(order(temp$score)),]
  
  order$rank <- c(1: nrow(order)) 
  
  
  head(order)
  return( order)
}

# mannual modifing weights 
modify_weights_mannual<- function(rank_df){
  measure_names()
  display_weights(rank_df)
  cat(" \nwhich meassure weight you want to change ? \n")
   i = as.numeric( readline())
    cat( "\n do you want to modify weight of meassure   ", i," [ y, n]:" )
    input =readline()
    if( input == 'y'){
      cat(" enter new weight \n")
      new_weight= as.numeric(readline())
      rank_df$weight[i]=new_weight
    }
    return( rank_df)
}


###################################################################################
###################################################################################
##################################################################################

#console 

console<- function(mf_data){
  cat("\014")
  mf<- transform_data(mf_data)
  cat(" data set we are working on \n")
  measure_names()
  View(mf_data)
  input = 1
  while( input != 0)
  {
    cat(" \n \n\nperform steps 1, 2, 3 before generating final-output \n perform steps 2,3 if you modify weights \n")
    cat( " enter 1 to  rank meassures \n")
    cat( " enter 2 to apply weights \n")
    cat(" enter 3 to genrate best mutual fund  order based on user preference\n")
    cat( " enter 4 to modify weight and distribute the residual weight automatically \n")
    cat( " enter 5 to modify weight mannually, make sure the weights add up to 1 after modification!!!\n")
    cat(" enter 6 to view original data \n")
    cat(" enter 7 to  view weights \n")
    cat(" enter 8 to view recomendation [ final output ] \n")
    cat(" enter 9 to see sum of weights \n")
    cat(" enter 0 to exit \n")
    cat(" \nenter input \n")
    input= as.numeric(readline())
    if( input == 1){
      cat("\014") 
      rank_df<-rank()
      cat("performed step 1 \n")
      }
    else if( input == 2 ){
      cat("\014") 
      mf_weights<- apply_weights(mf,rank_df)
      cat("performed step 2 \n")
      }
    else if( input == 3){
      cat("\014") 
      order <- ordering(mf_weights)
      cat("performed step 3 \n")
    }
    
    else if( input == 4){
      cat("\014") 
      rank_df<- modify_weights(rank_df)
      }
    else if( input == 5){
      cat("\014") 
      rank_df<- modify_weights_mannual(rank_df)
      }
    else if( input == 6){
      cat("\014") 
      View(mf_data)
    }
    else if( input == 7){
      cat("\014") 
      View(rank_df)
    }
    
    else if( input == 8){
      cat("\014") 
      View(order)
    }
    else if( input == 9){
      cat("\014") 
      cat(" sum of weights:  " ,sum(rank_df$weight))
    }
    
    
    }
}


####### run this 
console(mf_data)



