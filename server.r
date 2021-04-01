library(shiny)
library(rhandsontable)
source("quadspline.R")
source("simplex.r")
source("regression.r")


# Creating dataset
a = c(10,6,3,180)
b = c(8,5,4,80)
c = c(6,4,5,200)
d = c(5,3,5,160)
e = c(4,6,9,220)
f = c(310,260,280,0)
df1 = data.frame(Sacramento=a, Salt_Lake=b, Albuquerque=c,Chicago=d,New_York=e,Total=f)
#output
df2 = data.frame(Sacramento=c(0,0,0,0), Salt_Lake=c(0,0,0,0), Albuquerque=c(0,0,0,0),Chicago=c(0,0,0,0),New_York=c(0,0,0,0),Total=c(0,0,0,0))
out = NA
row.names(df1) <- c("Denver","Phoenix","Dallas","Supply")
out = list(c = df2,constraint = "",iters = "")
row.names(df2) <- c("Denver","Phoenix","Dallas","Supply")
shinyServer(function(input,output,session){
  simplexButton <-reactiveValues(pressed = 0)
  
  
  # returns rhandsontable type object - editable excel type grid data
  output$table <- renderRHandsontable({
    rhandsontable(df1) # converts the R dataframe to rhandsontable object
  })
  
  # on click of button the file will be saved to the working directory
  observeEvent(input$saveBtn,{
    write.csv(hot_to_r(input$table), file = "MyData.csv",row.names = FALSE)
    out = simplex()
    
    
    if(out$feas != FALSE){
      #render minimum Cost
      output$minTable <- renderTable({
        out$x
      })
      
      output$min <-renderText({
        out$mincost
      })
      
      #render Constraints
      output$obs <- renderText({
        out$obs[1]
      })
      output$con1 <- renderText({
        out$obs[2]
      })
      output$con2 <- renderText({
        out$obs[3]
      })
      output$con3 <- renderText({
        out$obs[4]
      })
      output$con4 <- renderText({
        out$obs[5]
      })
      output$con5 <- renderText({
        out$obs[6]
      })
      output$con6 <- renderText({
        out$obs[7]
      })
      output$con7 <- renderText({
        out$obs[8]
      })
      output$con8 <- renderText({
        out$obs[9]
      })
      #initial Tableau
      output$initialTab <- renderTable({
        out$initial
      })
      
      #iterations
      output$iters <- renderUI({
        #loop table output
        iters_output_list <-lapply(1:length(out$iterslist),function(i){
          itersname <- paste("iterslist",i,sep = "")
          tableOutput(itersname)
        })
        do.call(tagList,iters_output_list)
      })
      
      for(i in 1:length(out$iterslist)){
        local({
          temp1 <- i
          itersname = paste("iterslist",temp1,sep="")
          noOfIters = paste("Iteration ",temp1,sep = "")
          output[[itersname]] <- renderTable(data.frame(out$iterslist[[temp1]]),caption = noOfIters)
        })
      }
      
      output$sample1 <-renderText({
        "sample"
      })
    }
    else{
      showNotification("No Feasible Solution!")
    }
    
    
  })
  
  # hot_to_r() converts the rhandsontable object to r data object
  output$minTable <- renderTable({
    if(simplexButton$pressed){
      df2
    }
  })
  
  #constraints
  output$obs <- renderText({
    "Click change button"
  })
  
  #interpolating spline
  observeEvent(input$splineCompute,{
    splineFile = input$sfile
    output$inputSpline <- renderTable({
      if(!(is.null(splineFile))){
        temp = read.csv(splineFile$datapath,header = FALSE)
      }
    })
    
    if(!(is.null(splineFile))){
      temp = read.csv(splineFile$datapath,header = FALSE)
      ans = spline(temp,input$splineX)
      
      #functions
      output$funcList <- renderUI({
        #loop table output
        iters_output_list <-lapply(1:length(ans$ffs),function(i){
          itersname <- paste("ffs",i,sep = "")
          textOutput(itersname)
        })
        do.call(tagList,iters_output_list)
      })
      
      for(i in 1:length(ans$ffs)){
        local({
          temp1 <- i
          itersname = paste("ffs",temp1,sep="")
          output[[itersname]] <- renderText(ans$ffs[[temp1]])
        })
      }
      
      #estimated Value
      output$estimate <- renderText({
        ans$v
      })
      
      
    }
  })
  
  #polynomial Regression
  observeEvent(input$polyCompute,{
    polyFile = input$pfile
    if(!is.null(polyFile)){
      temp = read.csv(polyFile$datapath,header = FALSE)
      output$polyInput <- renderTable({
        temp
      })
      data = as.matrix(temp)
      print(data[,1])
      print(data[,2])
      if(input$polydegree > 0 && input$polydegree < length(data[,1])){
        ans = PolynomialRegression(data[,1],data[,2],input$polydegree,input$polyX)
        
        output$polyF <- renderText({
          ans$f
        })
        output$polyOut <- renderText({
          ans$estimate
        })
      }
      
    }
  })
  
})