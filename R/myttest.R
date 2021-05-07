#' @title A constructor function for t tests
#'
#' @description   This is an example of what can be done with S3 OOP
#'
#' @details The constructor function makes a list that can be worked and further processed with an appropriate method(s)
#' Returns either to accept or reject the null hypothesis with alpha = 0.05
#'
#' @param x vector of data
#' @param y vector of data
#' @param paired TRUE or FALSE, FALSE by default
#' @param alpha alpha level between 0,1
#'
#' @importFrom utils data
#'
#' @return list containing the data, acceptance/rejection of Null hypothesis, and  t test object
#' @export
#'
#' @keywords ttest t.test testing NHST
#'
#'
#'
#' @seealso \code{\link{t.test}} for more information about t tests
#'
#' @examples
#' myttest(x=rnorm(30,10,12), y=rnorm(40, 7, 10))
myttest<-function(x,y, paired=FALSE, alpha=0.05){

  data_dbl <- c(mode = "numeric", length = length(x) + length(y)) #data vector (atomic)
  v_dbl <- c(mode = "list", length = length(data_dbl)) #combined the numbers using c and put in vector list v (atomic)

  if(paired=="FALSE"){ # not paired then ...
    vt=var.test(x,y)

    if(vt$p.value>alpha){
      tt<-t.test(x,y,var.equal = TRUE, conf.level=1-alpha)
    }
    else{
      tt<-t.test(x,y,var.equal = FALSE, conf.level=1-alpha)
    }
    if(tt$p.value>0.05){
      print("Accept Null Hypothesis")
    }
    else{
      print("Reject Null Hypothesis")
    }
  }
  else{ # paired so check lengths
    stopifnot(length(x)==length(y))
    tt<-t.test(x,y,paired=TRUE,conf.level=1-alpha)

    if(tt$p.value>0.05){
      print("Accept Null Hypothesis")
    }
    else{
      print("Reject Null Hypothesis")
    }
  }

  data <- c(x,y)
  v <- rep(c("x","y"), c(length(x),length(y))) # Creation of qual var
  df = data.frame("data" = data, "v" = v)
  lst=list(ttest=tt, df=df, paired = paired)
  class(lst)<-"mytt"# New class
  lst
}

#-----------------------------------------------------------------------------

#' @title Structures of the Data Set
#' @description Provides structure and attributes of data inputted.
#' @details detail structure of data
#' @param x vector of data
#' @param y vector of data
#' @param Var.equal
#' @param paired
#'
#' @return
#' @export
#'
#' @examples
#'myttest(x=rnorm(30,10,12), y=rnorm(40, 7, 10))
mynewf<-function(x, y, Var.equal= True, paired= True){
  typeof(x)
  attr(x, "name") = "random data set"
  attr(x, "data") = c(x,y)
  str(attributes(x))
  class(x)
}

#--------------------------------------------------------------------


#' @title Normal Distribution histogram of Random Variable X data
#'
#' @description This is a visualization of the distribution of the data set for random continuous variable X
#'
#' @details The constructor function creates a histogram of the data set X.
#'
#' @param x vector of data
#'
#' @return histogram plot
#' @export
#'
#' @examples
#'mynewf1(x=rnorm(100))
mynewf1<-function(x){
    ggplot(NULL, aes(x=x))+
    geom_histogram(binwidth = 0.5,
    fill = "turquoise",
    color = ("white"))+
    xlab("z-scores")+
    ylab("Frequency")
}

#-------------------------------------------------------------------


#' @title Print Environment of Function Data
#'
#' @description shows the parent of the environment and returns the bindings of all the functions.
#'
#' @details Need RLANG package to operate. Calls the function and looks inside its enclosure with the active time of evaluation
#'
#' @param x vector of data
#' @param y vector of data
#'
#' @return Environment of data
#' @export
#'
#' @examples
#'mynewf2(x=rnorm(30,10,12), y=rnorm(40, 7, 10))
mynewf2<-function(x, y, Var.equal= True, paired= True){
  env_names(mynewf2)
  env_print(mynewf2)
  fn_env(mynewf2)
  env_bind_active(current_env(), activtime = function(tmp) {Sys.time()})
  activtime

}


#-----------------------------------------------------------------

#' @title A constructor function for t-test with categorical and continuous data
#' @description The constructor function creates a t-test that will read in categorical data for x and continuous data for y
#'   implementing subsetting a list in this code. Essentially it reads in a data frame.
#'
#' @details Need tidyverse, dplyr, and magrittr packages to run this.
#'
#' @param x vector of categorical data (need different sample code than the one provided for the project)
#' @param y vector of continuous data
#'
#' @return t-test result
#' @export
#' @examples
#'Can read in a csv file such as dat <- read.csv("..."). This example is different than one that must be shown for project.
#'example:
#'women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
#'men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4)
# Create a data frame
#' my_data <- data.frame(
#' group = rep(c("Woman", "Man"), each = 9),
# 'weight = c(women_weight,  men_weight), stringsAsFactors = T)
# 'my_data$group= as.factor(my_data$group)  #lets turn the column group to factors so that it can work


mynewf3<-function(x, y, Var.equal= True, paired= True){
  # equal variance test
  hj = cbind(x, y)
  p= as.list(unique(hj[,1]))
  stopifnot(length(p)==2) #if the length is not equal to length of data frame then stop

  # I am subdividing the data into two parts that will make it easy to get the means
  mean_category= subset(hj, hj[,1]==p[1])
  mean_category_2= subset(hj, hj[,1]==p[2])

  diff <- mean(mean_category[,2]) - mean(mean_category_2[,2])
  sd(mean_category[,2])/sqrt(length(mean_category_2))


  se <- sqrt(
    var(mean_category[,2])/length(mean_category[,2]) +
      var(mean_category_2[,2])/length(mean_category_2))
  tstat <- diff/se
  #getting the p-values

  righttail <- 1 - pnorm(abs(tstat))

  lefttail <- pnorm(-abs(tstat))
  pval <- lefttail + righttail

  # the step here  calculates the welch test for the data and gets the pvalues that will be used.


  # getting the means
  Ma= mean(mean_category[,2])
  mb= mean(mean_category_2[,2])

  #getting the variances
  Sa= var(mean_category[,2])
  Sb= var(mean_category_2[,2])
  Welch_statistic= (Ma-mb)/sqrt((Sa/nrow(mean_category)) + (Sa/nrow(mean_category_2)))

  # then we get the p-value
  righttailw <- 1 - pnorm(abs(Welch_statistic))
  lefttailw <- pnorm(-abs(Welch_statistic))
  pvalw <- lefttailw + righttailw
  # printing the p-valeus that will be used in the analysis
  if (Var.equal==TRUE){

    print("student T", pval)
  }
  if (Var.equal==FALSE){
    print( "welch test p-value",pvalw)
  }
}

# this whole function gets the t-test and the welch test
#mynewf3(my_data$group, my_data$weight, Var.equal = T)
#mynewf3(my_data$group, my_data$weight, Var.equal = F)

#-----------------------------------------------------------------------




#' @title A constructor function for t tests
#'
#' @description   This is an example of what can be done with S3 OOP
#'
#' @details The constructor function makes a list that can be worked and further processed with an appropriate method(s)
#' Returns either to accept or reject the null hypothesis with alpha = 0.05
#'
#' @param x vector of data
#' @param y vector of data
#' @param paired TRUE or FALSE, FALSE by default
#' @param alpha alpha level between 0,1
#'
#' @importFrom utils data
#'
#' @return list containing the data, acceptance/rejection of Null hypothesis, and  t test object
#' @export
#'
#' @keywords ttest t.test testing NHST
#'
#'
#'
#' @seealso \code{\link{t.test}} for more information about t tests
#'
#' @examples
#' myttest(x=rnorm(30,10,12), y=rnorm(40, 7, 10))
mynewf4<-function(x,y, paired=FALSE, alpha=0.05){

  data_dbl <- c(mode = "numeric", length = length(x) + length(y)) #data vector (atomic)
  v_dbl <- c(mode = "list", length = length(data_dbl)) #combined the numbers using c and put in vector list v (atomic)


  map_if(paired=="FALSE",data_dbl,v_dbl, all()) %>%
    vt=var.test(x,y)

    if(vt$p.value>alpha){
      tt<-t.test(x,y,var.equal = TRUE, conf.level=1-alpha)
    }
    else{
      tt<-t.test(x,y,var.equal = FALSE, conf.level=1-alpha)
    }
    if(tt$p.value>0.05){
      print("Accept Null Hypothesis")
    }
    else{
      print("Reject Null Hypothesis")
    }

  map_if(stopifnot(length(x)==length(y)),data_dbl,v_dbl, all())%>%
    tt<-t.test(x,y,paired=TRUE,conf.level=1-alpha)

    if(tt$p.value>0.05){
      print("Accept Null Hypothesis")
    }
    else{
      print("Reject Null Hypothesis")
    }


  data <- c(x,y)
  v <- rep(c("x","y"), c(length(x),length(y))) # Creation of qual var
  df = data.frame("data" = data, "v" = v)
  lst=list(ttest=tt, df=df, paired = paired)
  class(lst)<-"mytt"# New class
  lst
}

