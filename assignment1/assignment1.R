## Assignment1
## Nabil Shadman


## Problem 1
## Define function calCircle that takes calculation and radius
## arguments, and returns either area of circle, circumference 
## of circle, volume of sphere, or area of sphere

calCircle <- function (calculation, radius) {
  calc = toupper(calculation)
  if (calc == "AC") {
    return (round(pi*radius^2,3))
  } else if (calc == "CC") {
    return (round(2*pi*radius,3))
  } else if (calc == "VS") {
    return (round((4/3)*pi*radius^3,3))
  } else if (calc == "AS") {
    return (round(4*pi*radius^2,3))
  } else {
    stop ("your method is not supported")
  }
}

## Test calCircle function
calCircle('ac',4)
calCircle('Vs',1:5)


## Problem 2
## Define function calCircle2 takes in any combination of
## calculation arguments among area of circle, circumference 
## of circle, volume of sphere, and/or area of sphere,
## and radii arguments, and returns results combined in a list

calCircle2 <- function (calculations, radii){
  result = list()
  for (i in 1:length(calculations)) {
    result = c(result, list(calCircle(calculations[i], radii)))
  }
  names(result) = toupper(calculations)
  return(result)
}

## Test calCircle2 function
calCircle2(c('cc'), 1:4)
calCircle2(c('AC', 'VS'), seq(5,25,5))
calCircle2(c('AC', 'VS', "cc", "aS"), 3:10)


## Problem 3
## Define function table1 that takes a data frame and
## any subset of variables as arguments, and returns
## their descriptive statistics in a matrix

load("patient_num.RData")
patient_num

table1 <- function (dataframe, variables) {
  mat1 = matrix(,length(variables),5)
  rownames(mat1) = variables
  colnames(mat1) = c("Mean", "Median", "SD", "N", "N_miss")
  for (i in 1:length(variables)) {
    mat1[i,1] = round(mean(dataframe[,variables[i]],na.rm=T),2)
    mat1[i,2] = round(median(dataframe[,variables[i]],na.rm=T),2)
    mat1[i,3] = round(sd(dataframe[,variables[i]],na.rm=T),2)
    mat1[i,4] = sum(!is.na(dataframe[,variables[i]]))
    mat1[i,5] = sum(is.na(dataframe[,variables[i]]))
  }
  return (mat1)
}

## Test table1 function
table1(patient_num, c("GLUC", "TGL", "HDL", "LDL"))
table1(patient_num, c("GLUC", "TGL"))
table1(patient_num, c("TGL", "HDL", "LDL"))
table1(patient_num, c("LDL"))


## Problem 4
## Define function myTtest that takes data frame, 
## classification variable, and any combination of numeric
## variables as arguments, performs two-sample t-tests, and 
## returns results in a data frame

chol <- read.table("chol.txt")
head(chol)

myTtest <- function (dat, classVar, numVar) {
  df1 = data.frame(Varname=character(),
                  F.mean=double(),
                  M.mean=double(),
                  t=double(),
                  df=double(),
                  p=double()
  )
  for (i in 1:length(numVar)) {
    result = t.test(dat[[numVar[i]]] ~ dat[[classVar]])
    Varname = numVar[i]
    F.mean = as.vector(result$estimate[1])
    M.mean = as.vector(result$estimate[2])
    t = as.vector(result$statistic)
    df = as.vector(result$parameter)
    p = result$p.value
    df2 = data.frame(I(Varname), F.mean, M.mean, t, df, p)
    df1 = rbind(df1, df2)
  }
  return (df1)
}

## Test myTtest function
myTtest (dat=chol, classVar = "sex", numVar = c("age", "chol", "tg"))
myTtest (dat=chol, classVar = "sex", numVar = c("age", "chol"))
myTtest (dat=chol, classVar = "sex", numVar = "bmi")
myTtest (dat=chol, classVar = "sex", numVar = names(chol)[-1])
