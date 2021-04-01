#solve system of linear equations
#pivot function
pivot <- function(m,i){
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

#quadratic spline
spline <- function(input, estimate){
  print("at spline")
  point <- as.matrix(input)
  size = 3*(nrow(point) - 1)
  eq = matrix(0, nrow = size-1, ncol = size+1)
  
  print(point)
  print(eq)
  row = 1
  col = 1
  #condition 1
  for(i in 2:(nrow(point)-1)){
    #i-1
    eq[row,col] = point[i,1]^2
    eq[row,col+1] = point[i,1]
    eq[row,col+2] = 1
    eq[row,ncol(eq)] = point[i,2]
    #i
    eq[row+1,col+3] = point[i,1]^2
    eq[row+1,col+4] = point[i,1]
    eq[row+1,col+5] = 1
    eq[row+1,ncol(eq)] = point[i,2]
    row = row + 2
    col = col + 3
  }
  
  #condition 2
  #f0
  eq[row,1] = point[1,1]^2 #condition 4: a1 = 0
  eq[row,2] = point[1,1]
  eq[row,3] = 1
  eq[row,ncol(eq)] = point[1,2]
  row = row + 1
  #fn
  eq[row,ncol(eq)-3] = point[nrow(point),1]^2
  eq[row,ncol(eq)-2] = point[nrow(point),1]
  eq[row,ncol(eq)-1] = 1
  eq[row,ncol(eq)] = point[nrow(point),2]
  
  row = row + 1
  colCon3 = 1
  #condtion 3
  for (i in 2:(nrow(point)-1)) {
    print(i)
    eq[row,colCon3] = point[i,1]*2
    eq[row,colCon3+1] = 1
    eq[row,colCon3+2] = 0
    eq[row,colCon3+3] = -(point[i,1]*2)
    eq[row,colCon3+4] = -1
    eq[row,colCon3+5] = 0
    eq[row,ncol(eq)] = 0
    
    row = row + 1
    colCon3 = colCon3 + 3
  }
  
  finalEq = matrix(0, nrow =size-1, ncol = size)
  #populate final eqs
  for (i in 1:ncol(finalEq)) {
    finalEq[,i] = eq[,i+1]
  }
  
  print(finalEq)
  
  ans = GaussJordan(finalEq,TRUE)
  #todo: set all as function
  #find estimate base of range: check if x is within the range
  
  counter = 1
  finalEqs = c()
  li = ""
  
  for (i in 1:((length(ans$xs)/3)+1)) {
    if(i == 1){
      li = paste(0,"*x^2",sep = "")
      li = paste(li,"+",signif(ans$xs[counter],4),"*x",sep = "")
      li = paste(li,"+",signif(ans$xs[counter+1],4),sep = "")
      finalEqs = c(finalEqs,li)
      counter = counter + 2
      #print(finalEqs)
    }
    else{
      li = paste(signif(ans$xs[counter],4),"*x^2",sep = "")
      li = paste(li,"+",signif(ans$xs[counter+1],4),"*x",sep = "")
      li = paste(li,"+",signif(ans$xs[counter+2],4),sep = "")
      finalEqs = c(finalEqs,li)
      counter = counter + 3
      #print(finalEqs)
    }
    li = ""
  }
  
  
  #convert to strings to functions
  fs = c()
  strfs = c()
  l = ""
  for(i in 1:length(finalEqs)){
    l = paste("function(x)",finalEqs[i],sep="")
    print(l)
    strfs = c(strfs,l)
    f = eval(parse(text = l))
    fs= c(fs,f)
    
    l = ""
  }
  
  x = NA
  isExist = FALSE
  #get estimated value
  for (i in 1:(nrow(point)-1)) {
    if(point[i,1] <= estimate && point[i+1,1] >= estimate){
      x = fs[[i]](estimate)
      isExist = TRUE
    }
  }
  #return fs and estimated value
  print(x)
  
  #concat ffs
  outstrfs = ""
  liststr = c()
  for (i in 1:length(strfs)) {
    outstrfs = paste("[",point[i,1],",",point[i+1,1],"] = ",outstrfs,strfs[i],"\n",sep = "")
    liststr = c(liststr,outstrfs)
    outstrfs = ""
  }
  
  return(list(solution = isExist,ffs = liststr,v = x))
}


temp <- read.csv("sampleQuadraticSpline.csv",header = FALSE)
#temp1 <- as.matrix(temp) 
spline(temp,2)

