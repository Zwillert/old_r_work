
pppd_munge = function(data = data2){
  for(y in 1:ncol(data)){
    if(is.factor(data[,y])){
    if(length(levels(data[,y])) > 10){
      data[,y] = as.numeric(data[,y])
     } else {
        data[,y] = as.factor(as.character(data[,y]))
     }
    }
  }
  return(data)
}

data4 = pppd_munge()
ggplot(subset(data4[intersect(which(data4$VOTEDECTIME_W23 != "Refused"), which(data4$REGRETVOTEA_W23 != "Refused")),], Voted_for == "Donald Trump, the Republican" | Voted_for == "Hillary Clinton, the Democrat"), aes(REGRETVOTEA_W23, VOTEDECTIME_W23, col = Voted_for != "Donald Trump, the Republican", alpha = .1 )) + geom_jitter() + facet_wrap(~Voted_for) + guides(col = F)
pppd_munge2 = function(data = data4){
  accum = 1
  mat = data[,1]
  cnames = colnames(data)
  for(y in 1:ncol(data)){
    if(is.factor(data[,y])){
      for(l in levels(data[,y])){
        name = paste(cnames[y], l, sep = "")
        data[name] = (data[,y] == l) * 1
      }
    }
  }
  return(data)
}

data5 = pppd_munge2()

