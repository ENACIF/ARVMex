library(hrbrthemes)
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(RColorBrewer)
library(dplyr)
library(shinydashboardPlus)
library(shinyWidgets)
#library(showtext)


server = function(input, output, session){
  
  observeEvent(input$Esp, {
    updateTabItems(session, "sidebar", "Esp")
  })
  
  url1<-"File_1"
  url2<-"File_2"
  url3<-"File_3"
  url4<-"File_4"
  
  url5<-"File_5"
  url6<-"File_6"
  url7<-"File_7"
  url8<-"File_8"
  
  ad1 <- read_csv(url1)
  ad2 <- read_csv(url2)
  ad3 <- read_csv(url3)
  ad4 <- read_csv(url4)
  ad5 <- read_csv(url5)
  ad6 <- read_csv(url6)
  ad7 <- read_csv(url7)
  ad8 <- read_csv(url8)
  
  df1<-as.data.frame(ad1)
  df2<-as.data.frame(ad2)
  df3<-as.data.frame(ad3)
  df4<-as.data.frame(ad4)
  df5<-as.data.frame(ad5)
  df6<-as.data.frame(ad6)
  df7<-as.data.frame(ad7)
  df8<-as.data.frame(ad8)
  
  db1<- df1 %>% rename(F0=Mean_pitch)  %>% mutate_if(is.numeric, round,digits=5)
  db2<- df2 %>% rename(F0=Median_pitch)  %>% mutate_if(is.numeric, round,digits=5)
  db3<- df3 %>% rename(F0=Min_pitch)  %>% mutate_if(is.numeric, round,digits=5)
  db4<- df4 %>% rename(F0=Max_pitch)  %>% mutate_if(is.numeric, round,digits=5)
  db5<- df5 %>% rename(F0=Mean_pitch)  %>% mutate_if(is.numeric, round,digits=5)
  db6<- df6 %>% rename(F0=Median_pitch)  %>% mutate_if(is.numeric, round,digits=5)
  db7<- df7 %>% rename(F0=Min_pitch)  %>% mutate_if(is.numeric, round,digits=5)
  db8<- df8 %>% rename(F0=Max_pitch)  %>% mutate_if(is.numeric, round,digits=5)
  
  age <- reactive({
      req(input$db,input$met,input$age,input$grade,input$vowels,input$cons_libre,input$cons_ant_trabada,input$cons_traba_despues)
      if (input$db=='Interview')
      {if(input$met=='Mean'){
        datos<-db1} 
        if(input$met=='Midpoint'){
          datos<-db2}
        if(input$met=='Minimum'){
          datos<-db3}
        if(input$met=='Maximum'){
          datos<-db4}
      }
      else
      {if(input$met=='Mean'){
        datos<-db5} 
        if(input$met=='Midpoint'){
          datos<-db6}
        if(input$met=='Minimum'){
          datos<-db7}
        if(input$met=='Maximum'){
          datos<-db8}}
      if (any(input$age != "All"))
      {
        datos<- datos %>% filter(ID %in% str_subset(datos$ID,input$age))
      }
      if (input$grade != "All")
      {
        datos<- datos %>% filter(ID %in% str_subset(datos$ID,input$grade))
      }
      if (input$sex != "All")
      {
        datos<- datos %>% filter(ID %in% str_subset(datos$ID,paste0('S',input$sex)))
      }
      if (input$tonicidad != "All")
      {
        # datos = datos[grep(paste0('\\', input$labels, '$'), datos$Labels),]
        datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0(input$tonicidad, '$')))
      }
      if (input$vowels != "All")
      {
        # datos = datos[grep(paste0('[^aeiou]', input$vowels, '[^aeiou]'),datos$Labels),]
        datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0('[^aeiou]', input$vowels, '[^aeiou]')))
      }
      if (input$position == "Open")
      {
        
        #datos = datos[grep(paste0('[aeiou]', '_(A|T)$'), datos$Labels),]
        datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0('[aeiou]_(A|T)$')))
      }
      if (input$position == "Closed")
      {
        # datos = datos[grep('[aeiou]+[A-Z]', datos$Labels),]
        datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,'[aeiou]+[A-Z]'))
      }
      if (input$cons_libre != "All")
      {
        # datos = datos[grep(paste0(input$cons_libre,'[aeiou]','_(A|T)$'), datos$Labels),]
        datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0(input$cons_libre,'[aeiou]', '_(A|T)$')))
      }
      if (input$cons_ant_trabada != "All")
      {
        datos = datos[grep(paste0(input$cons_ant_trabada,'[aeiou]'), datos$Labels),]
        datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0(input$cons_ant_trabada,'[aeiou]')))
      }
      if (input$cons_traba_despues != "All")
      {
        # datos = datos[grep(paste0('[aeiou]',input$cons_traba_despues), datos$Labels),]
        datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0('[aeiou]',input$cons_traba_despues)))
      }
      else
      {
        datos
      }
  })
  
  age1 <- reactive({
    req(input$db,input$met,input$age,input$grade,input$vowels,input$cons_libre,input$cons_ant_trabada,input$cons_traba_despues)
    if(input$met=='Mean'){
      datos<-db1} 
    if(input$met=='Midpoint'){
      datos<-db2}
    if(input$met=='Minimum'){
      datos<-db3}
    if(input$met=='Maximum'){
      datos<-db4}
    if (any(input$age != "All"))
    {
      datos<- datos %>% filter(ID %in% str_subset(datos$ID,input$age))
    }
    if (input$grade != "All")
    {
      datos<- datos %>% filter(ID %in% str_subset(datos$ID,input$grade))
    }
    if (input$sex != "All")
    {
      datos<- datos %>% filter(ID %in% str_subset(datos$ID,paste0('S',input$sex)))
    }
    if (input$tonicidad != "All")
    {
      # datos = datos[grep(paste0('\\', input$labels, '$'), datos$Labels),]
      datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0(input$tonicidad, '$')))
    }
    if (input$vowels != "All")
    {
      # datos = datos[grep(paste0('[^aeiou]', input$vowels, '[^aeiou]'),datos$Labels),]
      datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0('[^aeiou]', input$vowels, '[^aeiou]')))
    }
    if (input$position == "Open")
    {
      
      #datos = datos[grep(paste0('[aeiou]', '_(A|T)$'), datos$Labels),]
      datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0('[aeiou]_(A|T)$')))
    }
    if (input$position == "Closed")
    {
      # datos = datos[grep('[aeiou]+[A-Z]', datos$Labels),]
      datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,'[aeiou]+[A-Z]'))
    }
    if (input$cons_libre != "All")
    {
      # datos = datos[grep(paste0(input$cons_libre,'[aeiou]','_(A|T)$'), datos$Labels),]
      datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0(input$cons_libre,'[aeiou]', '_(A|T)$')))
    }
    if (input$cons_ant_trabada != "All")
    {
      datos = datos[grep(paste0(input$cons_ant_trabada,'[aeiou]'), datos$Labels),]
      datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0(input$cons_ant_trabada,'[aeiou]')))
    }
    if (input$cons_traba_despues != "All")
    {
      # datos = datos[grep(paste0('[aeiou]',input$cons_traba_despues), datos$Labels),]
      datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0('[aeiou]',input$cons_traba_despues)))
    }
    else
    {
      datos
    }
    
  })
  
  age2 <- reactive({
    req(input$db,input$met,input$age,input$grade,input$vowels,input$cons_libre,input$cons_ant_trabada,input$cons_traba_despues)
    if(input$met=='Mean'){
      datos<-db5} 
    if(input$met=='Midpoint'){
      datos<-db6}
    if(input$met=='Minimum'){
      datos<-db7}
    if(input$met=='Maximum'){
      datos<-db8}
    if (any(input$age != "All"))
    {
      datos<- datos %>% filter(ID %in% str_subset(datos$ID,input$age))
    }
    if (input$grade != "All")
    {
      datos<- datos %>% filter(ID %in% str_subset(datos$ID,input$grade))
    }
    if (input$sex != "All")
    {
      datos<- datos %>% filter(ID %in% str_subset(datos$ID,paste0('S',input$sex)))
    }
    if (input$tonicidad != "All")
    {
      # datos = datos[grep(paste0('\\', input$labels, '$'), datos$Labels),]
      datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0(input$tonicidad, '$')))
    }
    if (input$vowels != "All")
    {
      # datos = datos[grep(paste0('[^aeiou]', input$vowels, '[^aeiou]'),datos$Labels),]
      datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0('[^aeiou]', input$vowels, '[^aeiou]')))
    }
    if (input$position == "Open")
    {
      
      #datos = datos[grep(paste0('[aeiou]', '_(A|T)$'), datos$Labels),]
      datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0('[aeiou]_(A|T)$')))
    }
    if (input$position == "Closed")
    {
      # datos = datos[grep('[aeiou]+[A-Z]', datos$Labels),]
      datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,'[aeiou]+[A-Z]'))
    }
    if (input$cons_libre != "All")
    {
      # datos = datos[grep(paste0(input$cons_libre,'[aeiou]','_(A|T)$'), datos$Labels),]
      datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0(input$cons_libre,'[aeiou]', '_(A|T)$')))
    }
    if (input$cons_ant_trabada != "All")
    {
      datos = datos[grep(paste0(input$cons_ant_trabada,'[aeiou]'), datos$Labels),]
      datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0(input$cons_ant_trabada,'[aeiou]')))
    }
    if (input$cons_traba_despues != "All")
    {
      # datos = datos[grep(paste0('[aeiou]',input$cons_traba_despues), datos$Labels),]
      datos<- datos %>% filter(Labels %in% str_subset(datos$Labels,paste0('[aeiou]',input$cons_traba_despues)))
    }
    else
    {
      datos
    }
  })
  
  output$table <- renderDataTable(datatable({
    age() %>% select(4:9)
  }, options = list(searching = FALSE),extensions = 'Responsive'))
  
  
  output$summary <- DT::renderDataTable(DT::datatable({
    {
      my.summary <- function(x, na.rm=TRUE){
        result <- c(n=length(x),
                    'Minimum'=min(x, na.rm=na.rm),
                    'Maximum'=max(x, na.rm=na.rm),
                    'Q1'=quantile(x, probs = 0.25, na.rm = TRUE,names=FALSE),
                    Median=median(x, na.rm=na.rm),
                    'Q3'=quantile(x, probs = 0.75, na.rm = TRUE,names=FALSE),
                    Mean=mean(x, na.rm=na.rm),
                    'Standard deviation'=sd(x, na.rm=na.rm),
                    Variance=var(x,na.rm=TRUE),
                    'Coefficient of variation'=sd(x, na.rm=na.rm)/mean(x, na.rm=na.rm))
      }
      b<-round(as.data.frame(sapply(select(age(),4:9), my.summary)),5)
    }
  },options = list(searching = FALSE),extensions = 'Responsive'))
  
  observeEvent(input$position, {
    updateSelectizeInput(session, "cons_libre", selected = 'All',choices=c("All",'COSB','COSD','COSV','COB', 'COD','COV','CFA','CFL','CFV','CFP','CL','CNB','CNA','CNP','CVS','CVM','CAP'))
    updateSelectizeInput(session, "cons_ant_trabada", selected = 'All',choices=c("All",'COSB','COSD','COSV','COB', 'COD','COV','CFA','CFL','CFV','CFP','CL','CNB','CNA','CNP','CVS','CVM','CAP'))
    updateSelectizeInput(session, "cons_traba_despues", selected = 'All',choices=c("All",'COSB','COSD','COSV','COB', 'COD','COV','CFA','CFL','CFV','CFP','CL','CNB','CNA','CNP','CVS','CVM','CAP'))
    
  })
  
  output$Mean_pitch <-
    renderPlot({
      ggplot(age(), aes(y=age()$F0)) + 
        geom_boxplot(fill="#E59866", alpha=0.2) + 
        xlab("F0")+ylab("Hz")+  theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())})
  
  output$F1 <-
    renderPlot({
      ggplot(age(), aes(y=F1)) + 
        geom_boxplot(fill= "#F8C471", alpha=0.2) + 
        xlab("F1")+ylab("Hz")+theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())})
  
  output$F2 <-
    renderPlot({
      ggplot(age(), aes(y=F2)) +
        geom_boxplot(fill="#73C6B6", alpha=0.2) +
        xlab("F2")+ylab("Hz")+theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())})
  
  output$F3 <-
    renderPlot({
      ggplot(age(), aes(y=F3)) +
        geom_boxplot(fill="#7FB3D5", alpha=0.2) +
        xlab("F3")+ylab("Hz")+theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())})
  
  output$F4 <-
    renderPlot({
      ggplot(age(), aes(y=F4)) +
        geom_boxplot(fill="#BB8FCE", alpha=0.2) +
        xlab("F4")+ylab("Hz")+theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())})
  
  output$Duration <-
    renderPlot({
      ggplot(age(), aes(y=log(Duration))) +
        geom_boxplot(fill="#D98880", alpha=0.2) +
        xlab("Duration")+ylab("Seconds (Logarithm)")+theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())})
  
 output$mean_pitch_d <-
    renderPlot({
      ggplot(data = age()) + geom_density(aes(x=F0),alpha = 0.4,fill="#F0B27A")+
        ggtitle('F0')+xlab("Hz")+ylab("Density")+theme(legend.position = "none")})
  
  output$F1_d <-
    renderPlot({
      ggplot(data = age()) + geom_density(aes(x=F1),fill="#F7DC6F",alpha = 0.4)  +
        ggtitle('F1')+xlab("Hz")+ylab("Density")+theme(legend.position = "none")})
  
  output$F2_d <-
    renderPlot({
      ggplot(data = age()) + geom_density(aes(x=F2),fill="#76D7C4",alpha = 0.4)  +
        ggtitle('F2')+xlab("Hz")+ylab("Density")+theme(legend.position = "none")})
  
  output$F3_d <-
    renderPlot({
      ggplot(data = age()) + geom_density(aes(x=F3),fill="#85C1E9",alpha = 0.4)  +
        ggtitle('F3')+xlab("Hz")+ylab("Density")+theme(legend.position = "none")})
  
  output$F4_d <-
    renderPlot({
      ggplot(data = age()) + geom_density(aes(x=F4),fill="#C39BD3",alpha = 0.4)  +
        ggtitle('F4')+xlab("Hz")+ylab("Density")+theme(legend.position = "none")})
  
  output$duration_d <-
    renderPlot({
      ggplot(data = age()) + geom_density(aes(x=log(Duration)),fill="#F5B7B1",alpha = 0.4)+
        ggtitle('Duration')+xlab("Seconds (Logarithm)")+ylab("Density")+theme(legend.position = "none")})
  
  output$media_d <- renderUI({
    if (nrow(age()) == 0)
      return("There is no data with those parameters")
    plotOutput("mean_pitch_d")
  })
  
  output$efeuno_d <- renderUI({
    if (nrow(age()) == 0)
      return("There is no data with those parameters")
    plotOutput("F1_d")
  })
  
  output$efedos_d <- renderUI({
    if (nrow(age()) == 0)
      return("There is no data with those parameters")
    plotOutput("F2_d")
  })
  
  output$efetres_d <- renderUI({
    if (nrow(age()) == 0)
      return("There is no data with those parameters")
    plotOutput("F3_d")
  })
  
  output$efecuatro_d <- renderUI({
    if (nrow(age()) == 0)
      return("There is no data with those parameters")
    plotOutput("F4_d")
  })
  
  output$duracion_d <- renderUI({
    if (nrow(age()) == 0)
      return("There is no data with those parameters")
    plotOutput("duration_d")
  })
  
  output$media <- renderUI({
    if (nrow(age()) == 0)
      return("There is no data with those parameters")
    plotOutput("Mean_pitch")
  })
  
  output$efeuno <- renderUI({
    if (nrow(age()) == 0)
      return("There is no data with those parameters")
    plotOutput("F1")
  })
  
  output$efedos <- renderUI({
    if (nrow(age()) == 0)
      return("There is no data with those parameters")
    plotOutput("F2")
  })
  
  output$efetres <- renderUI({
    if (nrow(age()) == 0)
      return("There is no data with those parameters")
    plotOutput("F3")
    
  })
  
  output$efecuatro <- renderUI({
    if (nrow(age()) == 0)
      return("There is no data with those parameters")
    plotOutput("F4")
    
  })
  
  output$duracion <- renderUI({
    if (nrow(age()) == 0)
      return("There is no data with those parameters")
    plotOutput("Duration")
  })
  
  output$d1<- renderPlotly({
    data1<-age1()
    data2<-age2()
    col1 <- "#E59866"
    col2 <- "#F0B27A"
    
    fig <- plot_ly(colors=c(col1, col2))
    fig <- fig %>% add_trace(y = data1$F0, type="box",quartilemethod="linear", name="Interview",color=I(col1))
    fig <- fig %>% add_trace(y = data2$F0, type="box",quartilemethod="linear", name="Telephone",color=I(col2))
    fig <- fig %>% layout(title = "F0")
  })
  
  output$d2<- renderPlotly({
    data1<-age1()
    data2<-age2()
    col1 <- "#F8C471"
    col2 <- "#F7DC6F"
    
    fig <- plot_ly(colors=c(col1, col2))
    fig <- fig %>% add_trace(y = data1$F1, type="box",quartilemethod="linear", name="Interview",color=I(col1))
    fig <- fig %>% add_trace(y = data2$F1, type="box",quartilemethod="linear", name="Telephone",color=I(col2))
    fig <- fig %>% layout(title = "F1")
    
  })
  
  output$d3<- renderPlotly({
    data1<-age1()
    data2<-age2()
    col1 <- "#73C6B6"
    col2 <- "#76D7C4"
    
    fig <- plot_ly(colors=c(col1, col2))
    fig <- fig %>% add_trace(y = data1$F2, type="box",quartilemethod="linear", name="Interview",color=I(col1))
    fig <- fig %>% add_trace(y = data2$F2, type="box",quartilemethod="linear", name="Telephone",color=I(col2))
    fig <- fig %>% layout(title = "F2")
    
  })
  
  output$d4<- renderPlotly({
    data1<-age1()
    data2<-age2()
    col1 <- "#7FB3D5"
    col2 <- "#85C1E9"
    
    fig <- plot_ly(colors=c(col1, col2))
    fig <- fig %>% add_trace(y = data1$F3, type="box",quartilemethod="linear", name="Interview",color=I(col1))
    fig <- fig %>% add_trace(y = data2$F3, type="box",quartilemethod="linear", name="Telephone",color=I(col2))
    fig <- fig %>% layout(title = "F3")
    
  })
  
  output$d5<- renderPlotly({
    data1<-age1()
    data2<-age2()
    col1 <- "#BB8FCE"
    col2 <- "#C39BD3"
    
    fig <- plot_ly(colors=c(col1, col2))
    fig <- fig %>% add_trace(y = data1$F4, type="box",quartilemethod="linear", name="Interview",color=I(col1))
    fig <- fig %>% add_trace(y = data2$F4, type="box",quartilemethod="linear", name="Telephone",color=I(col2))
    fig <- fig %>% layout(title = "F4")
  })
  
  output$d6<- renderPlotly({
    data1<-age1()
    data2<-age2()
    col1 <- "#D98880"
    col2 <- "#F5B7B1"
    
    fig <- plot_ly(colors=c(col1, col2))
    fig <- fig %>% add_trace(y = log(data1$Duration), type="box",quartilemethod="linear", name="Interview",color=I(col1))
    fig <- fig %>% add_trace(y = log(data2$Duration), type="box",quartilemethod="linear", name="Telephone",color=I(col2))
    fig <- fig %>% layout(title = "Duration")
  })
  
  output$download <- downloadHandler(
    filename = function(){"data.csv"}, 
    content = function(fname){
      write.csv(age(), fname)
    }
  )
}
