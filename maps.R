# https://github.com/TZstatsADS/Fall2016-Proj2-grp4/blob/master/app/ui.R
# https://pfwang.shinyapps.io/app1/

# Mapbox register: get keys
# complete list of map providers


nyc<-reactive({
  nyc_map<-leaflet()%>%setView(lat=40.7589,lng=-73.9851,zoom=12)%>%
    addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/mapbox/light-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoia3dhbGtlcnRjdSIsImEiOiJjaW9jenN1OGwwNGZsdjRrcWZnazh2OXVxIn0.QJrmnV9lJzdXHkH95ERdjw",attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>')%>%
    addProviderTiles("Stamen.Toner")%>%
    addProviderTiles("Stamen.TonerLabels")
  return(nyc_map)
})


output$street<-renderGoogle_map({
  google_map(key="AIzaSyBEwCy_6d2PImTjhBUEl8gT8ChiFJfzF1c",location=c(40.803321,-73.936403),zoom=16,search_box=T)
})

output$ticket<-renderDataTable({
  number()
})

output$ggBarPlotA<-renderPlot({
  ggplot(parking1%>%filter(month==monthSwitch()),aes(f,col=e))+
    geom_histogram(binwidth=1,position="identity")+geom_freqpoly(binwidth=1)+
    labs(title="Timeline of Violation distribution")+
    theme(axis.text=element_text(size=14),legend.key=element_rect(fill="white"),
          legend.background=element_rect(fill="grey40"),legend.position=c(0.14,0.80),
          panel.grid.major=element_line(colour="grey40"),panel.grid.minor=element_blank(),
          panel.background=element_rect(fill="black"),
          plot.background=element_rect(fill="black",colour="black",size=2,linetype="longdash"))
})
output$ggPiePlot<-renderPlotly({
  plot_ly(parking2,labels=parking2$label,values=parking2$score,hole=0.5,type="pie")%>%
    layout(title="Donut Chart of Violation Type",xaxis=list(title=NULL,showgrid=F),yaxis=list(title=NULL,showgrid=F),plot_bgcolor='rgba(0,0,0,1)')
})