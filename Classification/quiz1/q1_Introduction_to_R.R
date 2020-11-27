# Lab 1 Quiz - Introduction to R

# Importing our dataset as dataframe. We will use airpassengers dataset from R for years (1949- 1960)
dn = list(paste("Y", as.character(1949:1960), sep = ""), month.abb)
airmat = matrix(AirPassengers, 12, byrow = TRUE, dimnames = dn)
air = as.data.frame(t(airmat))

# Average airpassengers for year 1951
avgPass1951 = mean(air$Y1951)

# Max number of airpassengers for months January and February
maxJan = max(air["Jan",])
maxFeb = max(air["Feb",])

#find cross correlation of years 1952, 1953 1954 with year 1951.
cor5251 = cor(air$Y1952,air$Y1951)
cor5351 = cor(air$Y1953,air$Y1951)#
cor5451 = cor(air$Y1954,air$Y1951)

#find cross correlation of years January, February with month November.
corJanNov = cor(unlist(air["Jan",]),unlist(air["Nov",]))
corFebNov = cor(unlist(air["Feb",]),unlist(air["Nov",]))

# Saving the sum of all airpassengers per year in a vector and ploting it 
sumPerYear = colSums(air)

plot(sumPerYear,type = "b",xaxt= 'n', xlab = "Year", ylab = "number of Passengers")
axis(1, at = 1:(1960 - 1949 + 1),labels = names(air), las=2 )

# Saving the sum of all airpassengers per month in a vector and ploting it 
sumPerMonth = rowSums(air)

plot(sumPerMonth,type = "b",xaxt= 'n', xlab = "Month", ylab = "number of Passengers" )
axis(1, at = 1:12,labels = rownames(air)[1:12], las=2 )
