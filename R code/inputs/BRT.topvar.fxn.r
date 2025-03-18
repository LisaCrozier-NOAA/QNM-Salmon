BRT.topvar.fxn<-function(modelname,pct=50){
     model<-get(modelname)
     topvar<-summary(model)
     topvar$cumsum<-round(cumsum(topvar$rel.inf),2)
     topvar$index<-1:nrow(topvar)
     n<-min(which(topvar$cumsum>pct))
     
     #set up colors to match node colors
     name1<-substr(topvar$var,1,2);name1
     name2<-substr(topvar$var,3,4);name2
     clim<-grep("a",name1)
     pred1<-grep("d",name1)
     pred2<-which(topvar$var %in% c("g1.d2", "g1.d4", "g3.d2", "g3.d4"));pred2
     pred<-c(pred1,pred2)
     
     salmon1<-which(grepl("e|f" , name1));salmon1
     prey<-grep("b",name1) 
     salmon2<-prey[which(grep("b",name1) %in% grep("e",name2))];salmon2
     salmon3<-which(topvar$var %in% c("g1.g1", "g3.g3"));salmon3
     topvar$var[prey]
     topvar$var[salmon2]
     salmon<-c(salmon1,salmon2,salmon3)
     
     topvar$color<-"pink" 
     topvar$color[clim]<-"black" 
     topvar$color[salmon]<-"green" 
     topvar$color[pred]<-"red"
     
     #reverse order for plotting
     topvar2<-topvar[1:n,]
     topvar2<-topvar2[order(rev(topvar2$index)),]
     return(topvar2)
     
}
