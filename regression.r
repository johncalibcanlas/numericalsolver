#previous exer
#pivot function
pivot <- function(m,i){
  options(max.print = 1000000)
  m2 = abs(m)
  pivotRow = which(m2 == max(m2[i:nrow(m2),i]), arr.ind = TRUE)
  print("pivot row")
  print(pivotRow)
  row = 0
  for(j in 1:length(pivotRow)){
    if(i <= pivotRow[j]){
      row = pivotRow[j]
      break
    }
  }
  print("row")
  print(row)
  if(m[row,i] != 0){
    temp = m[i,]
    m[i,] = m[row,]
    m[row,] = temp;
    return(m)
  }
  else{
    return(NA)
  }
}

#Gauss-Jordan
GaussJordan<-function(f,verbose){
  coefMatrix = f
  pivotElement = 0
  multiplier = 0
  for(i in 1:nrow(coefMatrix)){
    #partial pivot
    coefMatrix = pivot(coefMatrix,i);
    #print(coefMatrix)
    #normalize pivot row
    pivotElement = coefMatrix[i,i]
    coefMatrix[i,] = coefMatrix[i,] / pivotElement
    #print(coefMatrix)
    #normalize matrix
    for(j in 1:nrow(coefMatrix)){
      if(i != j){
        pivotElement = coefMatrix[i,i]
        multiplier = coefMatrix[j,i] / pivotElement
        NormalizedRow = coefMatrix[i,] * multiplier
        coefMatrix[j,] = coefMatrix[j,] - NormalizedRow
      }
    }
    if(verbose == TRUE){
      print(coefMatrix)
      if(i == nrow(coefMatrix)){
        verbose = FALSE
      }
    }
  }
  #values of Unknowns
  x = coefMatrix[,ncol(coefMatrix)]
  if(verbose == FALSE){
    print(coefMatrix)
    print(x)
  }
  return(list(forelem = coefMatrix, xs = x))
}

#start of regression
PolynomialRegression <- function(iv, dv, deg,x){
  #checks inputs
  if (length(iv) != length(dv)){
    return(NA)
  }
  if (deg < 1){
    return(NA)
  }
  if(length(iv) < deg){
    return(NA)
  }
  
  mdata = c()
  #loop for 1 to deg
  #loop for 1 to deg + row
  for(i in 0:(deg)){
    for(j in 0:(deg)){
      sumx = sum((iv ^(i+j)))
      mdata = c(mdata, c(sumx))
    }
    sumxy = sum((iv ^ i) *dv)
    mdata = c(mdata,c(sumxy))
  }
  #puts the data in augmented coeffecient matrix
  augData = matrix(mdata, nrow = deg + 1, ncol = deg + 2, byrow = TRUE)
  GJ = GaussJordan(augData, TRUE)
  
  #converts the coefficients to string
  s = ""
  print(GJ[[2]][1])
  for(i in length(GJ[[2]]):1){
    si = paste(GJ[[2]][i],"*x^",(i-1),sep="")
    if(s == ""){
      s = si
    }else{
      s = paste(s,si, sep=" + ")
    }
    
  }
  
  #string to functions
  s = paste("function(x)",s,sep=" ")
  newf = eval(parse(text = s))
  value = newf(x)
  
  #returns coefficients and the created functions
  return(list(f = s,estimate = value))
  
}






