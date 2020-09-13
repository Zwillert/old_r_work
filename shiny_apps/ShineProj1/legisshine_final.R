#####                                                        #####
##### Shiny app to view Senate roll call and legislator data #####
#####                                                        #####
##### Import Packages and Data #####
### Sourcing 

source('legisshine_support.R', echo=TRUE)
source('legisshine.R', echo=TRUE)
y13 = parse_year(113)
### Packages 
library(shiny)
library(dendextend)
library(gridExtra)
library(grid)
library(ggdendro)
library(plotly)
library(ggridges)
### Data

sen_unity <- read.csv("sen_unity.csv")
sen_peeps <- read.csv("sen_peeps.csv")
rollcall_senate_all <- read.csv("rollcall_senate_all.csv")
senate_RC_data <- read.csv("senate_RC_data.csv")
peeps = c()
dtr = c()
##### Import Packages and Data #####
#####         #####
##### UI Code #####
#####         #####
ui=shinyUI(fluidPage(
  tabsetPanel(id = "main",
  tabPanel("Welcome",
           h1("Visualizing the Senate"),
           p("Welcome to this interactive Senate data visualization tool. "),
           plotOutput("firstPlot", height = "800px")    
  ),  
  tabPanel("Congress Summary", 
    sidebarLayout(
    sidebarPanel(
    numericInput(inputId = "year", label = "Session of Congress", value = 113)), 
    mainPanel(
      verticalLayout(
        #plotlyOutput("plot2"),
        
        plotOutput("plot2"),
        selectInput("dtOut", "View Data Selection", choices = c("Raw Data", "Legislator Data", "Bills Data"), selected = "Raw Data"), 
        dataTableOutput("dataTable")
      ))
    )),
  
  tabPanel("Nominate Analysis", 
           verticalLayout(
            
           plotlyOutput("plot0"),
           plotlyOutput("plot02"),
           selectInput("dtOut2", "View Data Selection", choices = c("Raw Data", "Legislator Data", "Bills Data"), selected = "Legislator Data"), 
           dataTableOutput("dataTable2")
    )),
  
  tabPanel("Dendogram", 
    selectInput("type", "Unit of Analysis", c("Legislators", "Bills"), selected = "Legislators"),         
    selectInput("coord", "Coodinates", c("Polar", "Cartesian"), selected = "Polar"),     
    h2("Dendogram of single linkage clustering of US Senators"),
    plotOutput("plot3", width = "700px", height = "900px")
    ),
  
  tabPanel("Homology Barplot",
           h2("Persistent Homology Analysis of Roll Call Data"),
           plotOutput("plot1"), 
           a("Here is a link to further work",
        href = "https://drive.google.com/file/d/0B9NR5nUTTth0RE55ZXZScmkyb0k/view?usp=sharing"))
)))
#####         #####
##### UI Code #####
#####         #####
#####             #####
##### Server Code #####
#####             #####
server=shinyServer(function(input, output) {
##### Process Data #####
  sen_peeps$Year = sen_peeps$Cong * 2 + 1787
  sen_peeps$Year = as.factor(sen_peeps$Year)

  output$firstPlot = renderPlot({
    ggplot(sen_peeps, aes(x = Dim1, y = Year, fill = Year)) + geom_density_ridges() + guides(fill = F) + xlab("Left-Right Partisan Scale") + ggtitle("Density Plots of Left-Right Political Ideology in the Senate")
  })
  dooit = function(){
    yr = input$year
    chm = "sen"
    saved_yrs = 113
    rcd = senate_RC_data[which(senate_RC_data$congress == yr),-c(1, 2, 32, 33)]
    colnames(sen_peeps)[3] = "ISPCR"
    dtr <<- merge(
      x = sen_unity[which(sen_unity$congr_number == yr),], 
                    y = sen_peeps[which(sen_peeps$Cong == yr),-c(1, 4:5, 11:14)],
                    by.x = "ISPCR_code", by.y = "ISPCR")[,-c(1,2)]
    if(yr == 113){
      LyearT = y13
    } else {
      LyearT = parse_year(yr)
    }
      print("wait, what?")
  
    peeps <<- rownames(LyearT$votes)
    perT = persistence(ydta = LyearT)
    yearT = year_m(perT)
    pf = yearT[[1]]
    ##### Process Data #####
    ##### Data Table Render #####
    dt = LyearT$votes
    dt = cbind(rownames(dt), dt)
    colnames(dt)[1] = "Senator"
    dt[which(dt == 2)] = "Yea"
    dt[which(dt == 1)] = "Nay"
    dt[which(dt == -1)] = "Absent"
    
    output$dataTable = renderDataTable({
      if (input$dtOut == "Raw Data"){
        dt
      } else if (input$dtOut == "Legislator Data"){
        takeout = c(1:5)
        dtr[-takeout]
      } else if (input$dtOut == "Bills Data"){
        takeout = c(1,2)
        rcd[,-takeout]
      }
    })
    output$dataTable2 = renderDataTable({
      if (input$dtOut2 == "Raw Data"){
        dt
      } else if (input$dtOut2 == "Legislator Data"){
        takeout = c(1:5)
        dtr[-takeout]
      } else if (input$dtOut2 == "Bills Data"){
        takeout = c(1,2)
        rcd[,-takeout]
      }
    })
    ##### Data Table Render #####
    ##### Plot Making #####
    titl = paste("Persistence Diagram of the ", yr, "th Senate", sep = "")
    gold = ggplot(pf[which(pf$pid_pct < 1),], aes(pep, eps_vec))+ geom_point()+ geom_path(data = yearT[[2]], aes( 99 * weighted_comp_rats, rat_eps), col = "green")+ geom_point(data = pf[which(pf$pid == 100 & pf$pid_pct == 1),], aes(pep, eps_vec, alpha = .1), alpha = .1, col = "blue") + geom_point(data = pf[which(pf$pid == 200 & pf$pid_pct == 1),], aes(pep, eps_vec, alpha = .1, col = "red"), alpha = .1) + coord_flip() + facet_wrap(~(year * 2 + 1789)) + xlab("Number of Components") + ylab("Epsilon") + ggtitle(titl)
    pl = plot_polish(LyearT)
    pl2 = pl[[1]]
    # Tricky cluterfuck of 3 lines
    LyearT$votes = LyearT$votes[-1,] 
    pl2$Senator = rownames(LyearT$votes)[pl2$mem]
    #print(pl2$Senator)
    pl2$Senator[pl2$memord] = as.numeric(pl2$mem[pl2$mem])
    dex = as.numeric(pl2$Senator[c(1:nrow(LyearT$votes))])
    #print(dex)
    #print((pl2$Senator))
    #View(pl2[-1,])
    pl2$Senator = rownames(LyearT$votes)[dex[pl2$mem]]
    #print(head(pl2$Senator))
    #
    pl2$Party = "Indepedent"
    pl2$Party[which(pl2$party == -1)] = "Republican"
    pl2$Party[which(pl2$party == 1)] = "Democrat"
    pl2$Vote = "Absent"
    pl2$Vote[which(pl2$voteval == 2)] = "Yea"
    pl2$Vote[which(pl2$voteval == 1)] = "Nay"
    pl2$Roll_Call_Number = pl2$vote
    titl = paste("Plot of all votes taken in the ", as.character(yr), "th Session of the Senate", sep = "")
    tplt = ggplot(pl2) + geom_point(data = pl2[which(pl2$voteval == 2),], aes(billord, memord, col = party), size = .1) + geom_point(data = pl2[which(pl2$voteval == 1),], aes(billord, memord), size = .1) + geom_point(data = pl2[which(pl2$voteval == -1),], aes(billord, memord, col = party), size = .1, alpha = .1) + xlab("Bills, Sorted by Partisan Gap") + ylab("Senators, Sorted by Ideology") + ggtitle(titl) #+ guides(col = F) + theme(legend.position = "none")
    ##### Plot Making #####
    ##### Plot Render #####
    View(dtr)
    output$plot0 <- renderPlotly({
      titl = paste("Partisanship Analysis of Senators in the ", as.character(yr), "th Session of Congress", sep = "")
      dtr$party_unity = dtr$party_unity * ((dtr$party_id == 200) * 2 - 1)
      dtr$Percent_of_votes_in_line_with_party = abs(dtr$party_unity)
      colnames(dtr)[c(3, 6)] = c("Senator_name", "Total_votes")
      pl = ggplot() + geom_point(data = dtr, aes(Dim1, Dim2, col = party_unity, Percent_of_votes_in_line_with_party = Percent_of_votes_in_line_with_party, State = state_name, Name = person_name, Total_votes = Total_votes)) + scale_colour_gradient2(low = "blue", high = "red", guide = "colourbar")
      pl = pl + labs(main = "Partisanship Analysis of Senators", x = "Partisanship Dimension 1", y = "Partisanship Dimension 2", col = "Percentage of Votes taken in Party Line") + ggtitle(titl) + guides(col = FALSE)
      ggplotly(pl, tooltip = c("Name", "State", "Percent_of_votes_in_line_with_party", "Total_votes"))
    })
    output$plot02 <- renderPlotly({
      titl = paste( "Partisanship Analysis of Bills in the ", as.character(yr), "th Session of Congress", sep = "")
      rcd$Bill_type = rcd$Clausen1
      rcd$Yea_Nay_Spread = rcd$Yeas - rcd$Nays
      pl = ggplot() + geom_point(data = rcd, aes(mid.dim1, mid.dim2, shape = Bill_type, size = Yeas/100, alpha = .3, col = Yeas-Nays, Date = date, Description = Description, Type1 = Bill_type,  Yeas = Yeas, Nays = Nays)) + scale_colour_gradient2(low = "green", high = "red", mid = "black", guide = "colourbar")
      pl = pl + labs( x = "Partisanship Dimension 1", y = "Partisanship Dimension 2", col = "Number of Yes Votes", size = "", shape = "Bill Classification") + ggtitle(titl) + guides(col = F) + guides(alpha = FALSE)
      ggplotly(pl, tooltip = c("Date", "Type1",  "Description", "Yeas", "Nays"))
    })
    output$plot1 <- renderPlot({
      
      gold
    })
    #output$plot2 <- renderPlotly({
    output$plot2 <- renderPlot({  
      tplt
      #ggplotly(tplt, tooltip = c("Senator", "Roll_Call_Number", "Party", "Vote"))
    })
    output$plot3 <- renderPlot({
      if(input$type == "Legislators"){
        expv = expand_votes_v(LyearT$votes)
        hc = hclust(dist(expv))
        hc$labels = rownames(LyearT$votes)
        hc$labels_col = LyearT$legis.data$party[-1]
        dc = as.dendrogram(hc)
        labels_colors(dc) <- c("blue", "green", "red")[hc$labels_col][order.dendrogram(dc)]
        dc2 = highlight_branches_lwd(dc)
        dc3 = dc2 %>% set("branches_k_color", k = 3, value = c("orange", "red", "blue"))
        par(cex = .4)
      } else {
        expv = expand_votes_v(t(LyearT$votes))
        hc = hclust(dist(expv))
        for(x in 1:ncol(LyearT$votes)){
          v3 = x%%121
          v2 = (x - v3)/121
          v3 = v3%%11
          v1 = (x - v3)/11
          hc$labels[x] = paste(c(x, rep(".", v1), x), collapse = "")
          par(cex = .2)
        }
        hc$labels_col = colSums(LyearT$votes == 2)
        hc$labels_col[which(hc$labels_col > 75)] = 4
        hc$labels_col[which(hc$labels_col > 50)] = 3
        hc$labels_col[which(hc$labels_col > 25)] = 2
        hc$labels_col[which(hc$labels_col > 4)] = 1
        
        dc = as.dendrogram(hc)
        labels_colors(dc) <- c("red", "yellow", "green", "blue")[hc$labels_col][order.dendrogram(dc)]
        dc2 = highlight_branches_lwd(dc)
        dc3 = dc2 
      }
      if(input$coord == "Polar"){
        circlize_dendrogram(dc3)
      } else {
        ggdendrogram(dc3) #+ coord_flip()
      }
    })
    ##### Plot Render #####
  }
  observeEvent(input$year, { 
  dooit()
  })
})
#####             #####
##### Server Code #####
#####             #####
shinyApp(ui,server)
