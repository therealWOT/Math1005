# finds total cars per day for year xxxx
getdata <- function(y){
df = read.csv("32029 - Burns Bay Road.csv")
frame = df[df$classification_seq == "All Vehicles",] #options for this = "All Vehicles", "Light Vehicles", "Heavy Vehicles"
year = frame[frame$year == y,]
direction = year[year$cardinal_direction_seq == "Northbound",] #options for this = "Northbound" or "Southbound"
dailytraffic = rowSums(direction[,c(5:28)])
filtereddaily = dailytraffic[complete.cases(dailytraffic)]
filteredframe = direction[complete.cases(dailytraffic),]
newframe = cbind(filteredframe,filtereddaily)
newframe$date = as.Date(newframe$date, format = "%d/%m/%Y")
return(newframe)
}

scatterplot <- function(y){
  a = getdata(y)
  plot(a$date,a$filtereddaily, main = paste("Car traffic in", as.character(y)),  ylab="Traffic Volume", xlab="Date")
  abline(lm(a$filtereddaily~a$date), col="blue")
}

residuals <- function(y){
  a = getdata(y)
  l=lm(a$filtereddaily~a$date)
  plot(a$date, l$residuals, main = paste("Residuals for car traffic in", as.character(y)), ylab="Residuals", xlab="Date")
  abline(h=0, col="red")
}
box <- function(y){
  a = getdata(y)
  boxplot(a$filtereddaily, main = paste("Daily car traffic for", as.character(y)), ylab = "Traffic Volume", xlab = as.character(y))
}



comparebox <- function(y,z){
a = getdata(y)
b = getdata(z)

boxplot(a$filtereddaily,b$filtereddaily, main = paste("Daily car traffic for", as.character(y), "and", as.character(z)), ylab = "Traffic volume", names = c(as.character(y),as.character(z)))
}


#use these functions to generate graphs: scatterplot = scatterplot(), residuals = residuals(), boxplot = box(), comparison boxplot = comparebox()
#put dates in brackets, compare box takes 2 dates, date range is from 2008 - 2023
#data uses northbound traffic, change parameter to southbound in first method for southbound

scatterplot(2021)
residuals(2021)
box(2021)

comparebox(2021,2020)






