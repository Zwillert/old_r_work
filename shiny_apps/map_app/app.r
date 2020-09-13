library(dplyr)
library(readr)
library(stringr)
library(tidyr)
#library(readxl)
library(classInt)
library(RColorBrewer)
#library(maptools)
#library(readr)
#library(plotly)
library(shiny)
library(ggplot2)
library(gridExtra)
library(mapproj)
## from https://github.com/tonmcg/County_Level_Election_Results_12-16/blob/master/US_County_Level_Presidential_Results_08-16.csv

newdt = read.csv( "prez_elec_history_data.csv")
newdt = newdt[,-c(58:82)]
full_county_data = read.csv( "full_county_data.csv")
full_county_data_names = read.csv( "county_facts_dictionary.csv")

countydata = read.csv("finCountyData.csv")

count_data_types = names(countydata[c(13:length(names(countydata)))])
pol_names_formal = c("2008 Election Results by County", "2012 Election Results by County", "2016 Election Results by County", "2008-2012 Change in Election Results by County", "2012-2016 Change in Election Results by County", "2008-2016 Change in Election Results by County")
data_names_formal = as.character(full_county_data_names[c(9, 1:8, 10:nrow(full_county_data_names)),2])

data_names_formal2 = as.character(full_county_data_names[,2])
#print(c(data_names_formal2, pol_names_formal, data_names_formal))


ui = navbarPage( 
  # Application title
  title = ("County and Electoral Data"),
  tabPanel("Map",
           sidebarLayout(
             
             sidebarPanel(
               
               selectInput("pol_data_type",
                           "Electoral Data Type:", 
                           #count_data_types[c((length(count_data_types) -5) : length(count_data_types))], 
                           pol_names_formal,
                           selected = "dem_gop_2012_spread_pct"),
               selectInput("data_type",
                           "Census Data Type:", 
                           data_names_formal,
                           #count_data_types[c(1:(length(count_data_types) -25))]
                           selected = "White alone, percent, 2014"),
               selectInput("transformation",
                           "Data transformation:", 
                           c("standard", "difference from average"), selected = "standard"),
               selectInput("state",
                           "State:", 
                           c("nation", unique(as.character(countydata$region))), selected = "nation"),
               submitButton(),
               p("Welcome to this data visualizer. Are you wondering what happened in the 2016 election? Here is a tool to explore various electoral and economic/socio-demographic data. The top map will show electoral data from any of the last 3 elections, and can be changed by changing the selection in the 'Electoral Data Type' list above. The second map shows the electoral results from the 2016 Presidential election. And finally, the bottom map show various Census data. You can change the selection on the 'State' list to change the area of inspection. Make sure to hit 'Apply Changes' to update the maps. Enjoy! And if you have any feedback, comments or questions, please feel free to email me at:", a("zach.willert@gmail.com", href = "mailto:zach.willert@gmail.com"))
             ),
             mainPanel(
               
               h3("Variable Electoral Data Map"),
               plotOutput("polPlot"), 
               h3("2016 Election Data Map"),
               plotOutput("elecPlot"),
               h3("Variable Census Data Map"),
               plotOutput("distPlot")
               #,
               #plotOutput("residPlot")
             )))
  , 

  
  tabPanel("Regression",
           selectInput("var1",
                       "X- Axis:", 
                       c(data_names_formal2, pol_names_formal), selected = "2008-2016 Change in Election Results by County"),
           selectInput("var2",
                       "Y- Axis:", 
                       c(data_names_formal2, pol_names_formal), selected = "Bachelor's degree or higher, percent of persons age 25+, 2009-2013"),
           selectInput("var3",
                       "Color:", 
                       c(data_names_formal2, pol_names_formal), selected = "White alone, percent, 2014"),
           
           submitButton("Apply Changes"),
           plotOutput("regPlot"), 
           textOutput("regRes"))
)



server = function(input, output){
  output$regRes <- renderText({
    #var1 = which(names(countydata) == input$var1)
    #var2 = which(names(countydata) == input$var2)
    
    val1 = which(c(data_names_formal2, pol_names_formal) == input$var1) + 6
    val2 = which(c(data_names_formal2, pol_names_formal) == input$var2) + 6
    var1 = newdt[,val1]
    var2 = newdt[,val2]
    
   # print(input$var1)
   # print(val1)
   # print(colnames(newdt)[val1])
   # print(val2)
   # print(colnames(newdt))
    print(summary(lm(var1~var2)))
    
    
    
})
  
  output$regPlot <- renderPlot({ 
    val1 = which(c(data_names_formal2, pol_names_formal) == input$var1) + 6
    val2 = which(c(data_names_formal2, pol_names_formal) == input$var2) + 6
    val3 = which(c(data_names_formal2, pol_names_formal) == input$var3) + 6
    var1 = newdt[,val1]
    var2 = newdt[,val2]
    var3 = newdt[,val3]
    col = input$var3
    #plot(var1, var2, xlab = val1)
    if(col == "none"){var3 = "black"} 
    ggplot(newdt, aes(var1, var2, color = var3, alpha = .2)) + geom_point() + xlab(input$var1) + ylab(input$var2) + scale_color_distiller(palette = "Spectral") + guides(col = guide_legend(title = input$var3), alpha = F)
    #+ theme(legend.title = "") # +guides(fill = guide_legend(title = input$var3)) + ggplot2::scale_colour_brewer(palette = "Spectral")
  }) 
  
  output$distPlot <- renderPlot({
    inp = input$data_type
    #inpval = which(names(countydata) == inp)
    #print(names(countydata))
    #print(data_names_formal)
    inpval = which(data_names_formal == inp) + 12
    inptype = input$transformation
    region = input$state
    dex = which(!is.na(countydata))
    print(inp)
    print(inpval)
    mapend = coord_map("bonne", parameters=45)
    mapfill = scale_fill_distiller(palette = "Spectral", direction = 1) 
    mapguide = guides(fill=guide_legend(title=""))
    if(region != "nation"){
      dex = which(countydata$region == region)
      mapbase = ggplot(countydata[dex,])
      print("howdy")
    } else {
      dex = c(1:nrow(countydata))
      mapbase = ggplot(countydata)
    }
    mapbase = ggplot(countydata[dex,])
    meanv = mean(na.omit(countydata[dex,inpval]))
    if(inptype == "standard"){
      mapmid = geom_polygon(aes(long, lat, group = group, fill = countydata[dex,inpval]), size = 0.1)
    } 
    if(inptype == "difference from average"){
      print(meanv)
      val = (countydata[dex,inpval] - meanv)
      mapmid = geom_polygon(aes(long, lat, group = group, fill = countydata[dex,inpval]-meanv),color = "grey", size = 0.1) 
    }
    
    if(inptype == "logged val"){
      mapmid = geom_polygon(aes(long, lat, group = group, fill = log(countydata[dex,inpval])),color = "grey", size = 0.1) 
    }
    mapbase + mapmid + mapend + mapfill  + ggtitle(inp) + mapguide
  })
  
  output$polPlot <- renderPlot({
    inp = input$pol_data_type
    #inpval = which(names(countydata) == inp)
    inpval = which(pol_names_formal == inp) + (length(names(countydata)) - 6)
    #inp = pol_names_formal[inpval - (length(names(countydata)) - 6)]
    inptype = input$transformation
    region = input$state
    dex = which(!is.na(countydata))
    print(inp)
    print(inpval)
    mapend = coord_map("bonne", parameters=45)
    mapfill = scale_fill_distiller(palette = "RdBu", direction = 1) 
    #mapguide = guides(fill=guide_legend(title=input$pol_data_type))
    mapguide = guides(fill=guide_legend(title=""))
    if(region != "nation"){
      dex = which(countydata$region == region)
      mapbase = ggplot(countydata[dex,])
      print("howdy")
    } else {
      dex = c(1:nrow(countydata))
      mapbase = ggplot(countydata)
    }
    mapbase = ggplot(countydata[dex,])
    meanv = mean(na.omit(countydata[dex,inpval]))
    if(inptype == "standard"){
      mapmid = geom_polygon(aes(long, lat, group = group, fill = countydata[dex,inpval]), size = 0.1)
    } 
    if(inptype == "difference from average"){
      print(meanv)
      val = (countydata[dex,inpval] - meanv)
      mapmid = geom_polygon(aes(long, lat, group = group, fill = countydata[dex,inpval]-meanv),color = "grey", size = 0.1) 
    }
    mapbase + mapmid + mapend + mapfill + mapguide + ggtitle(inp)
  })
  
  output$elecPlot <- renderPlot({
    inpval = which(names(countydata) == "dem_gop_2016_spread_pct")
    inptype = input$transformation
    region = input$state
    dex = which(!is.na(countydata))
    mapend = coord_map("bonne", parameters=45)
    mapfill = scale_fill_distiller(palette = "RdBu", direction = 1) 
    mapguide = guides(fill=guide_legend(title= ""))
  
   if(region != "nation"){
      dex = which(countydata$region == region)
      mapbase = ggplot(countydata[dex,])
      print("howdy")
    } else {
      dex = c(1:nrow(countydata))
      mapbase = ggplot(countydata)
    }
    mapbase = ggplot(countydata[dex,])
    meanv = mean(na.omit(countydata[dex,inpval]))
    if(inptype == "standard"){
      mapmid = geom_polygon(aes(long, lat, group = group, fill = countydata[dex,inpval]),color = "grey", size = 0.1) 
    } 
    if(inptype == "difference from average"){
      print(meanv)
      val = (countydata[dex,inpval] - meanv)
      print(head(val))
      print(length(dex))
      mapmid = geom_polygon(aes(long, lat, group = group, fill = countydata[dex,inpval]-meanv),color = "grey", size = 0.1) 
    }
    if(inptype == "logged val"){
      mapmid = geom_polygon(aes(long, lat, group = group, fill = log(countydata[dex,inpval])),color = "grey", size = 0.1) 
    }
    p1 = mapbase + mapmid + mapend + mapfill + mapguide + ggtitle("2016 Election Results by County") 
    p1 
  })
}

shinyApp(ui = ui, server = server)
