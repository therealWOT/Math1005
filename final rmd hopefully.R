y=2021
df = read.csv("74229 - Beecroft Road2.csv") #change this to the file location on your computer
frame = df[df$classification_seq == "All Vehicles",] #options for this = "All Vehicles", "Light Vehicles", "Heavy Vehicles"
year = frame[frame$year == y,]
direction = year[year$cardinal_direction_seq == "Northbound",] #options for this = "Northbound" or "Southbound"
dailytraffic = rowSums(direction[,c(5:28)])
filtereddaily = dailytraffic[complete.cases(dailytraffic)]
filteredframe = direction[complete.cases(dailytraffic),]
newframe = cbind(filteredframe,filtereddaily)
newframe$date = as.Date(newframe$date, format = "%d/%m/%Y")
return(newframe)
