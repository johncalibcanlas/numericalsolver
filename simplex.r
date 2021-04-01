    #simplex minimize
  
  #solutionchecker
  check <-function(mat){
    for(i in 1:24){
      if(mat[16,i] < 0){
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
  #find highest negative magnitude
  highNegative <- function(mat){
    pivotCol = 1
    for(i in 1:24){
      if(mat[16,i] < mat[16,(pivotCol)]){
        pivotCol = i
      }
    }
    return(pivotCol)
  }
  
  lowPositive <- function(mat,pivotCol){
    pivotRow = 0
    for(i in 1:15){
      if(mat[i,pivotCol] > 0){
        if(pivotRow == 0){
          pivotRow = i
        }
        else{
          if((mat[i,25]/mat[i,pivotCol]) < (mat[pivotRow,25]/mat[pivotRow,pivotCol])){
            pivotRow = i
          }
        }
      }
    }
    return(pivotRow)
  }
    
  simplex <- function(){
    temp <- read.csv("MyData.csv")
      cost <- as.matrix(temp)
      #print(cost)
      #create tableau
      tab = matrix(0, nrow = 9, ncol = 16)
      for(i in 1:(length(cost[1,])-1)){
        tab[1,i] = cost[1,i]
        tab[2,5+i] = cost[2,i]
        tab[3,10+i] = cost[3,i]
      }
      
      
      
      print("new")
      print(tab)
      
      tab[4,1] = cost[1,1];tab[4,6] = cost[2,1];tab [4,11] = cost[3,1];
      tab[5,2] = cost[1,2];tab[5,7] = cost[2,2];tab [5,12] = cost[3,2];
      tab[6,3] = cost[1,3];tab[6,8] = cost[2,3];tab [6,13] = cost[3,3];
      tab[7,4] = cost[1,4];tab[7,9] = cost[2,4];tab [7,14] = cost[3,4];
      tab[8,5] = cost[1,5];tab[8,10] = cost[2,5];tab [8,15] = cost[3,5];
      
      #equal
      tab[1,16] = cost[1,6]; tab[2,16] = cost[2,6]; tab[3,16] = cost[3,6];
      tab[4,16] = cost[4,1]; tab[5,16] = cost[4,2]; tab[6,16] = cost[4,3]; tab[7,16] = cost[4,4]; tab[8,16] = cost[4,5]; tab[9,16] = 0
      
      tab[9,1] = cost[1,1]; tab[9,2] = cost[1,2]; tab[9,3] = cost[1,3]; tab[9,4] = cost[1,4]; tab[9,5] = cost[1,5];
      tab[9,6] = cost[2,1]; tab[9,7] = cost[2,2]; tab[9,8] = cost[2,3]; tab[9,9] = cost[2,4]; tab[9,10] = cost[2,5];
      tab[9,11] = cost[3,1]; tab[9,12] = cost[3,2]; tab[9,13] = cost[3,3]; tab[9,14] = cost[3,4]; tab[9,15] = cost[3,5];
      
      #objFunction
      obj = paste("Z<=",tab[9,1],"*x1+",tab[9,2],"*x2+",tab[9,3],"*x3+",tab[9,4],"*x4+",tab[9,5],"*x5+",tab[9,6],"*x6+",tab[9,7],"*x7+",tab[9,8],"*x8+",tab[9,9],"*x9+",tab[9,10],"*x10+",tab[9,11],"*x11+",tab[9,12],"*x12+",tab[9,13],"*x13+",tab[9,14],"*x14+",tab[9,15],"*x15",sep = "")
      #constraint 1
      con1 = paste(tab[9,1],"*x1+",tab[9,2],"*x2+",tab[9,3],"*x3+",tab[9,4],"*x4+",tab[9,5],"*x5","<=",tab[1,16], sep = "")
      con2 = paste(tab[9,6],"*x6+",tab[9,7],"*x7+",tab[9,8],"*x8+",tab[9,9],"*x9+",tab[9,10],"*x10",tab[9,11],"<=",tab[2,16],sep = "")
      con3 = paste(tab[9,11],"*x11+",tab[9,12],"*x12+",tab[9,13],"*x13+",tab[9,14],"*x14+",tab[9,15],"*x15","<=",tab[3,16],sep = "")
      #constraint 2
      con4 = paste(tab[9,1],"*x1+",tab[9,6],"*x6+",tab[9,11],"*x11",">=",tab[4,16],sep = "")
      con5 = paste(tab[9,2],"*x2+",tab[9,7],"*x6+",tab[9,12],"*x11",">=",tab[5,16],sep = "")
      con6 = paste(tab[9,3],"*x3+",tab[9,8],"*x6+",tab[9,13],"*x11",">=",tab[6,16],sep = "")
      con7 = paste(tab[9,4],"*x4+",tab[9,9],"*x6+",tab[9,14],"*x11",">=",tab[7,16],sep = "")
      con8 = paste(tab[9,5],"*x5+",tab[9,10],"*x6+",tab[9,15],"*x11",">=",tab[8,16],sep = "")
      
      outObj = c(obj,con1,con2,con3,con4,con5,con6,con7,con8)

      
      #change inequality
      tab[4,] = -tab[4,]
      tab[5,] = -tab[5,]
      tab[6,] = -tab[6,]
      tab[7,] = -tab[7,]
      tab[8,] = -tab[8,]
      
      print(tab)
      tab = t(tab)
      
      newTab = matrix(0, nrow = 16, ncol = 25)
      newTab[,25] = tab[,9];
      for(i in 1:8){
        newTab[,i] = tab[,i]
      }
      
      newTab[16,] = -newTab[16,]
      
      for(i in 9:24){
        newTab[i-8,i] = 1 
      }
      
      for(i in 1:8){
        newTab[,i] = -newTab[,i]
      }
      
      #set matrix to 1
      for(i in 1:15){
        for(j in 1:8){
          newTab[i,j] = newTab[i,j] /newTab[i,25]
        }
      }
      
      print(newTab)
      initTab <- as.data.frame(newTab)
      iter = 0
      
      iterList = list()
      
      #real deal
      while(check(newTab)){
        #iterations
        print(paste("Iteration",iter,sep=" "))
        print(newTab)
        
        #get pivot element
        pivotCol = highNegative(newTab)
        pivotRow = lowPositive(newTab,pivotCol)
        pivotElement = newTab[pivotRow,pivotCol]
        print(paste("pivot Row:",pivotRow, sep =""))
        print(paste("pivot Col:",pivotCol, sep =""))
        print(paste("pivot element:",pivotElement, sep = ""))
        
        #return if no feabisible solution
        if(pivotRow == 0){
          return(list(feas = FALSE))
        }
        
        #normalize row
        for(i in 1:25){
          newTab[pivotRow,i] = (newTab[pivotRow,i] / pivotElement)
        }
        
        #prevtab
        prevtab = newTab
        
        #need previous table
        #update table
        for(i in 1:16){
          if(i != pivotRow){
            for(j in 1:25){
              newTab[i,j] = (-prevtab[i,pivotCol] * prevtab[pivotRow,j]) + newTab[i,j]
            }
          }
        }
        
        iter = iter + 1
        iterList[[iter]] = newTab
      }
      
      print("final")
      print(newTab)
      
      #create min matrix
      min = matrix(0,nrow = 4, ncol = 6,dimnames = list(c("Denver","Phoenix","Dallas","Total"),c("Sacramento","Salt_Lake","Albuquerque","Chicago","New_York","Total")))
      values = 9
      for(i in 1:3){
        for(j in 1:5){
          min[i,j] = newTab[16,values]
          values = values+1
        }
        min[i,6] = sum(min[i,])
      }
      
      for (i in 1:6) {
        min[4,i] = sum(min[,i])
      }
      minData <- as.data.frame(min)
      
      return(list(feas = TRUE,mincost = newTab[16,25],x = minData, obs = outObj,initial = initTab,iterslist = iterList))
  }
    #temp1 <- as.matrix(temp) 
    
    simplex()
