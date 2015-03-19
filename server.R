require(shiny)
require(rCharts)
data(iris)
iris$Species=as.character(iris$Species)
shinyServer(function(input, output) {
  #' pie chart: ok
  output$piechart <- renderChart({
    x=unique(as.character(iris$Species))
    y=as.numeric(table(iris$Species))
    a <- Highcharts$new()
    a$title(text = "Number of Species in Iris dataset")
    a$data(type='pie',x=x,y=y  ) 
    a$addParams(dom = 'piechart')
    return(a)
  })
  #' multiline basics: ok
  output$multiline_petal <- renderChart({
    cat(input$x[1],'\n')
    a <- Highcharts$new()
    a$title(text = "Comparison of Petal")
    a$xAxis(title=list(text="Observations") )
    a$series(name=input$x[1], data= iris[,input$x[1]] )
    a$series(name=input$x[2], data= iris[,input$x[2]] )      
    a$addParams(dom = 'multiline_petal')
    return(a)
  })
  output$multiline_sepal <- renderChart({    
    a <- Highcharts$new()
    a$title(text = "Comparison of Sepal")
    a$xAxis(title=list(text="Observations") )
    a$series(name=input$y[1], data= iris[,input$y[1]] )
    a$series(name=input$y[2], data= iris[,input$y[2]] )
    a$addParams(dom = 'multiline_sepal')
    return(a)
  })
  #' scatter plot: OK
  output$scatterplot <- renderChart({
    a <- Highcharts$new()
    a$chart(type="scatter")
    a$title(text = "Sepal vs Petal by species")
    a$xAxis(title=list(enabled=TRUE,text=input$x[1]))
    a$yAxis(title=list(enabled=TRUE,text=input$y[1]))
    a$data(name='setosa',color= 'rgba(223, 83, 83,.5)',
           x=iris[as.character(iris$Species)=='setosa',input$x[1]],y=iris[as.character(iris$Species)=='setosa',input$y[1]])
    a$data(name='versicolor',color= 'rgba(119, 152, 191, .5)',
           x=iris[as.character(iris$Species)=='versicolor',input$x[1]],y=iris[as.character(iris$Species)=='versicolor',input$y[1]])
    a$data(name='virginica',
           x=iris[as.character(iris$Species)=='virginica',input$x[1]],y=iris[as.character(iris$Species)=='virginica',input$y[1]])
    
    a$addParams(dom = 'scatterplot')
    return(a)
  })
  output$regression <- renderChart({
    #' scatter plot + regressions:
    lm_setosa=lm(iris[as.character(iris$Species)=='setosa',input$y[1]]~iris[as.character(iris$Species)=='setosa',input$x[1]],data=iris[as.character(iris$Species)=='setosa',])
    lm_versicolor=lm(iris[as.character(iris$Species)=='versicolor',input$y[1]]~iris[as.character(iris$Species)=='versicolor',input$x[1]],data=iris[as.character(iris$Species)=='versicolor',])
    lm_virginica=lm(iris[as.character(iris$Species)=='virginica',input$y[1]]~iris[as.character(iris$Species)=='virginica',input$x[1]],data=iris[as.character(iris$Species)=='virginica',])
    data_setosa=cbind(iris[as.character(iris$Species)=='setosa',input$x[1]],lm_setosa$fitted.values)
    data_versicolor=cbind(iris[as.character(iris$Species)=='versicolor',input$x[1]],lm_versicolor$fitted.values)
    data_virginica=cbind(iris[as.character(iris$Species)=='virginica',input$x[1]],lm_virginica$fitted.values)
    
    
    list_setosa=list()
    for ( i in 1:nrow(data_setosa) ) {
      list_setosa[[i]]=data_setosa[i,]
    }
    list_versicolor=list()
    for ( i in 1:nrow(data_versicolor) ) {
      list_versicolor[[i]]=data_versicolor[i,]
    }
    list_virginica=list()
    for ( i in 1:nrow(data_setosa) ) {
      list_virginica[[i]]=data_virginica[i,]
    }
    #
    a <- Highcharts$new()
    a$title(text="Sepal vs Petal by species(+ regression lines)")
    a$xAxis(min=min(iris[,input$x[1]]),title=list(enabled=TRUE,text=input$x[1]))
    a$yAxis(title=list(enabled=TRUE,text=input$y[1]))
    a$series(type='line',
             name='fitted values setosa',color= 'rgba(223, 83, 83,.5)',data=list_setosa)
    a$series(type='line',
        name='fitted values versicolor',color= 'rgba(119, 152, 191, .5)',data=list_versicolor)
    a$series(type='line',
        name='fitted values virginica',data=list_virginica)
    
    a$data(type='scatter',name='setosa',color= 'rgba(223, 83, 83,.5)',
          x=iris[as.character(iris$Species)=='setosa',input$x[1]],y=iris[as.character(iris$Species)=='setosa',input$y[1]])
    a$data(type='scatter',name='versicolor',color= 'rgba(119, 152, 191, .5)',
          x=iris[as.character(iris$Species)=='versicolor',input$x[1]],y=iris[as.character(iris$Species)=='versicolor',input$y[1]])
    a$data(type='scatter',name='virginica',
          x=iris[as.character(iris$Species)=='virginica',input$x[1]],y=iris[as.character(iris$Species)=='virginica',input$y[1]])
    
    a$addParams(dom = 'regression')
    return(a)
  })
  #' histogram + mean line:
  output$combo_sepal <- renderChart({
    #data preparation:
    x1=mean(iris[iris$Species=='setosa',input$x[1]])
    x2=mean(iris[iris$Species=='versicolor',input$x[1]])
    x3=mean(iris[iris$Species=='virginica',input$x[1]])
    x=cbind(x1,x2,x3)
    #
    a <- Highcharts$new()
    a$chart(type = "column") #you can change to "bar"
    a$title(text = "Mean Sepal by species (+Average)")
    a$xAxis(categories = unique(as.character(iris$Species)))
    a$yAxis(title = list(text = input$x[1]))
    a$data(x,name='Mean')
    a$series(type='spline',name='average'
             ,data= x )
    #a$series(type='pie',
     #        name='Pourcentage par Evolution',
      #       x=colnames(x),y=apply(x,2,sum)/sum(x),
       #      color=colnames(x))
    a$addParams(dom = 'combo_sepal')
    return(a)
  })
  output$combo_petal <- renderChart({
    #data preparation:
    x1=mean(iris[iris$Species=='setosa',input$y[1]])
    x2=mean(iris[iris$Species=='versicolor',input$y[1]])
    x3=mean(iris[iris$Species=='virginica',input$y[1]])
    x=cbind(x1,x2,x3)
    #
    a <- Highcharts$new()
    a$chart(type = "column") #you can change to "bar"
    a$title(text = "Mean Petal by species (+Average)")
    a$xAxis(categories = unique(as.character(iris$Species)))
    a$yAxis(title = list(text = input$y[1]))
    a$data(x,name='Mean')
    a$series(type='spline',name='average'
             ,data= x )
    
    a$addParams(dom = 'combo_petal')
    return(a)
  })
  #' single bar chart: ok
#   output$single_barchart <- renderChart({
#     #data preparation:
#     x=table(base_enq2013$FJUR,base_enq2013$EV2T13)
#     colnames(x)=c('Baisse','Stable','Hausse')
#     rownames(x)=c('SA','SARL','EI')
#     x=cbind(x[,'Baisse'],x[,'Stable'],x[,'Hausse'])
#     x=as.data.frame(x)
#     colnames(x)=c('Baisse','Stable','Hausse')
#     #
#     a <- Highcharts$new()
#     a$chart(type = "column") #you can change to "bar"
#     a$title(text = "Evolution de la production selon les SA")
#     a$xAxis(categories = colnames(x))
#     a$yAxis(title = list(text = "Pourcentage"))
#     a$data(as.data.frame(x[1,]))
#     a$addParams(dom = 'single_barchart')
#     return(a)
#   })
#   #' bar stacked: ok
#   output$barstacked <- renderChart({
#     #data preparation:
#     x=table(base_enq2013$FJUR,base_enq2013$EV2T13)
#     colnames(x)=c('Baisse','Stable','Hausse')
#     rownames(x)=c('SA','SARL','EI')
#     x=cbind(x[,'Baisse'],x[,'Stable'],x[,'Hausse'])
#     x=as.data.frame(x)
#     colnames(x)=c('Baisse','Stable','Hausse')
#     #aggregate(x=iris$Sepal.Length,by=list(as.character(iris$Species)),FUN=mean)
#     a <- Highcharts$new()
#     a$chart(type = "bar") #you can change "bar" to "column"
#     a$plotOptions(bar=list(stacking="normal")) #you can change "bar" to "column"
#     a$title(text = "Mean of characteristics by species")
#     a$xAxis(categories = rownames(x))
#     a$yAxis(title = list(text = "Pourcentage"))
#     a$data(x)
#     a$addParams(dom = 'barstacked')
#     return(a)
#   })
#   output$multibar <- renderChart({
#     #data preparation:
#     x=table(base_enq2013$FJUR,base_enq2013$EV2T13)
#     colnames(x)=c('Baisse','Stable','Hausse')
#     rownames(x)=c('SA','SARL','EI')
#     x=cbind(x[,'Baisse'],x[,'Stable'],x[,'Hausse'])
#     x=as.data.frame(x)
#     colnames(x)=c('Baisse','Stable','Hausse')
#     #
#     a <- Highcharts$new()
#     a$chart(type = "bar") #you can change to "bar"
#     a$title(text = "Evolution de la production")
#     a$xAxis(categories = rownames(x))
#     a$yAxis(title = list(text = "Pourcentage"))
#     a$data(x)
#     a$addParams(dom = 'multibar')
#     return(a)
#   })
  output$colrange <- renderChart({
    #
    a <- Highcharts$new()
    a$chart(type = "columnrange",inverted=T)
    a$title(text = "Variation des Opinions par FJ")
    a$xAxis(categories = rownames(x))
    a$series(data=list(c(-3,2),c(-2,0.5),c(-1.5,3)))
    a$addParams(dom = 'colrange')
    return(a)
  })
})