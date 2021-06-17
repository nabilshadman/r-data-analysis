## Assignment 2
## Nabil Shadman


## Data
load("Assignment2.RData")
ls()


## Problem 1
## Define function getMaxSBP that takes sbp data frame and returns
## data frame that contains one observation for each patient with
## variables newID, Sex, maxsbp, maxvisit

head(sbp)

getMaxSBP <- function(df) {
  df1 = data.frame(newID=character(),
                      Sex=character(),
                      maxvisit=integer(),
                      maxsbp=integer())
  for (i in 1:nrow(df)) {
    newID = ifelse(is.element(substr(as.character(df$id[i]),1,1), "x"),
                             substr(as.character(df$id[i]),2,
                                    nchar(as.character(df$id[i]))-1),
                             substr(as.character(df$id[i]),1,
                                    nchar(as.character(df$id[i]))-1)
                             )
    Sex = toupper(substr(as.character(df$id[i]),nchar(as.character(df$id[i])),
                        nchar(as.character(df$id[i]))))
    maxvisit = df$visit[i]
    maxsbp = ifelse(df$sbp[i] > 300, NA,
                              df$sbp[i])
    df2 = data.frame(I(newID), I(Sex), maxvisit, maxsbp)
    df1 = rbind(df1, df2)
  }
  df1 = df1[order(df1$newID, -df1$maxsbp),]
  df3 = data.frame()
  df3 = rbind(df3, df1[1,])
  for (i in 2:nrow(df1)) {
    if (df1$newID[i] != df1$newID[i-1])
      df3 = rbind(df3, df1[i,])
  }
  rownames(df3) <- 1:nrow(df3)
  return (df3)
}

## Test getMaxSBP function
getMaxSBP(sbp)


## Problem 2
## Define function myCorTest that calculates pairwise correlation
## between one variable with list of given variables

head(chol)
age_chol <- cor.test(chol$age, chol$chol)
str(age_chol)
age_chol$estimate
age_chol$p.value

myCorTest <- function(dat, mainVar, varlist) {
  df1 = data.frame()
  for (i in 1:length(varlist)) {
    cor_test = cor.test(dat[,mainVar], dat[,varlist[i]])
    var1 = mainVar
    var2 = varlist[i]
    R = cor_test$estimate
    P = cor_test$p.value
    df2 = data.frame(var1, var2, R, P)
    df1 = rbind(df1, df2)
  }
  rownames(df1) = varlist
  return (df1)
}

## Test myCorTest function
myCorTest (chol, "wt", "age")
myCorTest (chol, "wt", c("age", "chol", "tg", "ht"))
myCorTest (chol, "bmi", c("sbp", "dbp", "vldl", "hdl", "ldl"))


## Problem 3
## Define function that takes data frame, numeric variables, and
## categorical variables, and returns a list of descriptive
## statistics for the numeric variables and count of the
## categorical variables

patient

table1 <- function(dat, numvar = NULL, charvar = NULL) {
  if ((is.null(numvar)) & (is.null(charvar))) {
    list1 <- list(numericStats = NULL, FactorStats = NULL)
  }
  if ((!is.null(numvar)) & (is.null(charvar))) {
    df1 = data.frame()
    for (i in 1:length(numvar)) {
      varName = numvar[i]
      MEAN = mean(dat[,numvar[i]],na.rm=T)
      MEDIAN = median(dat[,numvar[i]],na.rm=T)
      SD = sd(dat[,numvar[i]],na.rm = T)
      NMiss = sum(is.na(dat[,numvar[i]]))
      df2 = data.frame(I(varName), MEAN, MEDIAN, SD, NMiss)
      df1 = rbind(df1, df2)
    }
    list1 = list(numericStats = df1, FactorStats = NULL)
  }
  if ((is.null(numvar)) & (!is.null(charvar))) {
    df3 = dat
    df3[,6] = as.character(df3[,6])
    df3[,7] = as.character(df3[,7])
    df3[,8] = as.character(df3[,8])
    for (i in 1:nrow(df3)) {
      if (is.na(dat[i,6])) {
        df3[i,6] = "NMiss"
      }
      if (is.na(dat[i,7])) {
        df3[i,7] = "NMiss"
      }
      if (is.na(dat[i,8])) {
        df3[i,8] = "NMiss"
      }
    }
    df4 = data.frame()
    for (i in 1:length(charvar)) {
      tab = table(df3[[charvar[i]]])
      tab.dat = as.data.frame(tab)
      one.table = data.frame(
        varName = c(charvar[i], rep(" ", nrow(tab.dat)-1)),
        group = tab.dat$Var1,
        count = tab.dat$Freq
      )
      df4 = rbind(df4, one.table)
    }
    list1 = list(numericStats = NULL, FactorStats = df4)
  }
  if ((!is.null(numvar)) & (!is.null(charvar))) {
    df1 = data.frame()
    for (i in 1:length(numvar)) {
      varName = numvar[i]
      MEAN = mean(dat[,numvar[i]],na.rm=T)
      MEDIAN = median(dat[,numvar[i]],na.rm=T)
      SD = sd(dat[,numvar[i]],na.rm = T)
      NMiss = sum(is.na(dat[,numvar[i]]))
      df2 = data.frame(I(varName), MEAN, MEDIAN, SD, NMiss)
      df1 = rbind(df1, df2)
    }
    df3 = dat
    df3[,6] = as.character(df3[,6])
    df3[,7] = as.character(df3[,7])
    df3[,8] = as.character(df3[,8])
    for (i in 1:nrow(df3)) {
      if (is.na(dat[i,6])) {
        df3[i,6] = "NMiss"
      }
      if (is.na(dat[i,7])) {
        df3[i,7] = "NMiss"
      }
      if (is.na(dat[i,8])) {
        df3[i,8] = "NMiss"
      }
    }
    df4 = data.frame()
    for (i in 1:length(charvar)) {
      tab = table(df3[[charvar[i]]])
      tab.dat = as.data.frame(tab)
      one.table = data.frame(
        varName = c(charvar[i], rep(" ", nrow(tab.dat)-1)),
        group = tab.dat$Var1,
        count = tab.dat$Freq
      )
      df4 = rbind(df4, one.table)
    }
    list1 = list(numericStats = df1, FactorStats = df4)
  }
  return (list1)
}

## Test table1 function
table1 (dat=patient, numvar=c("TGL", "HDL", "LDL"), charvar=c("HRT", "MAMM"))
table1 (dat=patient, numvar="LDL", charvar=c("HRT", "MAMM","SMOKE"))
table1 (dat=patient, numvar=c("HDL", "LDL"))
table1 (dat=patient, charvar=c("HRT", "MAMM","SMOKE"))


## Problem 4
## Define function impute, which is used to replace the 
## missing value with meaningful value. For numeric variable, 
## replace the missing value with median of the variable. 
## For character variable, replace missing with value of highest frequency.


impute <- function (dat, varlist= NULL) {
  if (is.null(varlist)) {
    df1 = dat
    for (i in 1:ncol(dat)) {
      if (class(dat[,i]) == "integer") {
        a = median(dat[,i],na.rm=T)
        for (j in 1:nrow(dat[i])) {
          if (is.na(dat[j,i])) {
            df1[j,i] = a
          }
        }
      }
      if (class(dat[,i]) == "factor") {
        a = table(dat[i])
        b = as.data.frame(a)
        max_var1 = b[1,1]
        max_freq = b[1,2]
        for (j in 1:nrow(b)) {
          if (b[j,2] > max_freq) {
            max_var1 = b[j,1]
          }
        }
        df1[,i] = as.character(df1[,i])
        for (j in 1:nrow(dat[i])) {
          if (is.na(df1[j, i])) {
            df1[j,i] = as.character(max_var1)
          }
        }
      }
    }
  }
  if (!is.null(varlist)) {
    df1 = dat[varlist]
    for (i in 1:length(varlist)) {
      if (class(dat[,varlist[i]]) == "integer") {
        a = median(dat[,varlist[i]],na.rm=T)
        for (j in 1:nrow(df1[i])) {
          if (is.na(df1[j,i])) {
            df1[j,i] = a
          }
        }
      }
      if (class(dat[,varlist[i]]) == "factor") {
        a = table(dat[varlist[i]])
        b = as.data.frame(a)
        max_var1 = b[1,1]
        max_freq = b[1,2]
        for (j in 1:nrow(b)) {
          if (b[j,2] > max_freq) {
            max_var1 = b[j,1]
          }
        }
        df1[,i] = as.character(df1[,i])
        for (j in 1:nrow(dat[varlist[i]])) {
          if (is.na(df1[j, i])) {
            df1[j,i] = as.character(max_var1)
          }
        }
      }
    }
  }
  return (df1)
}

## Test impute function
impute (dat=patient)
impute (dat=patient, varlist=c("ID", "GLUC", "TGL", "HDL"))
impute (dat=patient, varlist=c("LDL", "HRT", "MAMM"))
impute (dat=patient, varlist="HRT")
