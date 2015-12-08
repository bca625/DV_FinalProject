# server.R
require(jsonlite)
require(RCurl)
require(ggplot2)
require(dplyr)
require(tidyr)
require(shiny)
require(leaflet)
require(shinydashboard)
require(DT)

shinyServer(function(input, output) {
  
  df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from GENERAL_STATS order by pos"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_ba7433', PASS='orcl_ba7433', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))
  
  tm <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from TEAMS "'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_ba7433', PASS='orcl_ba7433', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))
  
  
  
  
  
  
  #Points By Position Bar Chart
  output$pointsbyteam <- renderPlot({
    NBA <- df%>%filter(TM != "TOT", TM != "Tm")
    plot2 <- ggplot(NBA, aes(TM, PTS, fill=TM)) + geom_bar(stat="identity") + coord_flip()
    
    return(plot2)
    
  })
  
  
  #Points By Division Bar Chart
  output$pointsbypos <- renderPlot({
    NBA <- df%>%filter(TM != "TOT", TM != "Tm")
    plot3 <- ggplot(NBA, aes(TM, PTS, fill=POS)) + geom_bar(stat="identity") + coord_flip()
    
    return(plot3)
  })
  
  
  output$assistsbypos <- renderPlot({
    NBA <- df%>%filter(TM != "TOT", TM != "Tm")
    plot4 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='Assists By Position') +
      labs(x="Minutes Played", y=paste("Assists")) +
      layer(data=NBA, 
            mapping=aes(x=as.numeric(as.character(MP)), y=as.numeric(as.character(AST)), color=POS), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(), 
            #position=position_identity()
            position=position_jitter(width=0.3, height=0)
      )
    
    return(plot4)
    
  })
  
  output$rebsbypos <- renderPlot({
    NBA <- df%>%filter(TM != "TOT", TM != "Tm")
    plot5 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='Rebounds By Position') +
      labs(x="Minutes Played", y=paste("Total Rebounds")) +
      layer(data=NBA, 
            mapping=aes(x=as.numeric(as.character(MP)), y=as.numeric(as.character(TRB)), color=POS), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(), 
            #position=position_identity()
            position=position_jitter(width=0.3, height=0)
      )
    
    return(plot5)
    
  })
  
  output$stealsbypos <- renderPlot({
    NBA <- df%>%filter(TM != "TOT", TM != "Tm")
    plot6 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='Steals By Position') +
      labs(x="Personal Fouls", y=paste("Steals")) +
      layer(data=NBA, 
            mapping=aes(x=as.numeric(as.character(PF)), y=as.numeric(as.character(STL)), color=POS), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(), 
            #position=position_identity()
            position=position_jitter(width=0.3, height=0)
      )
    
    return(plot6)
    
  })
  
  output$blocksbypos <- renderPlot({
    NBA <- df%>%filter(TM != "TOT", TM != "Tm")
    plot7 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='Blocks By Position') +
      labs(x="Personal Fouls", y=paste("Blocks")) +
      layer(data=NBA, 
            mapping=aes(x=as.numeric(as.character(PF)), y=as.numeric(as.character(BLK)), color=POS), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(), 
            #position=position_identity()
            position=position_jitter(width=0.3, height=0)
      ) 
    
    return(plot7)
    
  })
  
  
  output$crosstab <- renderPlot({
    teams <- tm%>%rename(TM = TEAM)
    foo <- right_join(NBA, teams, by="TM")
    
    KPI_Low_Max_value = input$KPI1     
    KPI_Medium_Max_value = input$KPI2
    
    
    crosstab <- foo%>%
      filter(DIVISION=="Southwest") %>%
      group_by(NAME, POS)%>%
      mutate(PPG = as.numeric(as.character(PTS)) / as.numeric(as.character(G))) %>% 
      summarize(avg_pts = mean(PPG)) %>% 
      mutate(kpi = ifelse(avg_pts <= KPI_Low_Max_value, 'Low', ifelse(avg_pts <= KPI_Medium_Max_value, 'Medium', 'High'))) %>% 
      rename(KPI=kpi)

    plot8 <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title='Points by Position in The Southwest Division') +
      labs(x=paste("TEAM"), y=paste("POSITION")) +
      layer(data=crosstab, 
            mapping=aes(x=NAME, y=POS, label=round(avg_pts, 2)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=crosstab, 
            mapping=aes(x=NAME, y=POS, fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      )
    
    return(plot8)
    
  })
  
  
  
  
  
})