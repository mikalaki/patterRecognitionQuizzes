# Decision Trees Quiz

#importing proper libraries
library(rpart)
library(rpart.plot)

# importing data from car_insurance.txt
car_insurance = read.csv("car_insurance.txt")

##1. Find GINI index for all training set

totaln = length(car_insurance$Insurance)
absfreq_Yes = length(which(car_insurance$Insurance == "Yes"))
absfreq_No = length(which(car_insurance$Insurance == "No"))

freq_Yes = absfreq_Yes/totaln
freq_No = absfreq_No/totaln

GINI_ALL =  1 - freq_Yes^2 - freq_No^2

##2. Find GINI index for customerID
#Gini for Cust id
absfreq_ID = table(car_insurance[, c(1, 5)])
freq_ID = prop.table(absfreq_ID, 1)
freqSum_ID = rowSums(prop.table(absfreq_ID))
GINI_ID_total =0;
for ( i in 1:length(car_insurance$CustomerID)){
  GINI_ID_this = 1 - freq_ID[i, "No"]^2 - freq_ID[i, "Yes"]^2
  GINI_ID_total = GINI_ID_total + freqSum_ID[i] * GINI_ID_this
  
}

##3. Find GINI index for Sex = male and Sex
absfreq_sex = table(car_insurance[ , c(2,5)])
freq_sex =  prop.table(absfreq_sex ,1 )
freqSum_sex = rowSums(prop.table(absfreq_sex))

GINI_Male = 1 - freq_sex[ "M", "No"]^2 -freq_sex[ "M", "Yes"]^2
GINI_Female = 1 - freq_sex[ "F", "No"]^2 -freq_sex[ "F", "Yes"]^2

GINI_sex = freqSum_sex["M"] * GINI_Male +
  freqSum_sex["F"] * GINI_Female 

##4. Find GINI index for characteristic CarType with multiway split
absfreq_car_type = table(car_insurance[ , c(3,5)])

freq_car_type =  prop.table(absfreq_car_type ,1 )

freqSum_car_type = rowSums(prop.table(absfreq_car_type))

# partial GINIs
GINI_Family = 1 - freq_car_type[ "Family", "No"]^2 -freq_car_type[ "Family", "Yes"]^2

GINI_Sedan = 1 - freq_car_type[ "Sedan", "No"]^2 -freq_car_type[ "Sedan", "Yes"]^2

GINI_Sport = 1 - freq_car_type[ "Sport", "No"]^2 -freq_car_type[ "Sport", "Yes"]^2

# Car Type general GINI
GINI_car_type = freqSum_car_type["Family"] * GINI_Family +
  freqSum_car_type["Sedan"] * GINI_Sedan + 
  freqSum_car_type["Sport"] * GINI_Sport


## 5. Find GINI index for characteristic Budget with multiway split
absfreq_Budget = table(car_insurance[ , c(4,5)])

freq_Budget =  prop.table(absfreq_Budget ,1 )

freqSum_Budget = rowSums(prop.table(absfreq_Budget))

# partial GINIs
GINI_Low = 1 - freq_Budget[ "Low", "No"]^2 -freq_Budget[ "Low", "Yes"]^2

GINI_Medium  = 1 - freq_Budget[ "Medium", "No"]^2 -freq_Budget[ "Medium", "Yes"]^2

GINI_High = 1 - freq_Budget[ "High", "No"]^2 -freq_Budget[ "High", "Yes"]^2

GINI_VeryHigh = 1 - freq_Budget[ "VeryHigh", "No"]^2 -freq_Budget[ "VeryHigh", "Yes"]^2

# Budget Type general GINI
GINI_Budget = freqSum_Budget["Low"] * GINI_Low + 
  freqSum_Budget["Medium"] * GINI_Medium + 
  freqSum_Budget["High"] * GINI_High + 
  freqSum_Budget["VeryHigh"] * GINI_VeryHigh

#Based on GINI index what characteristic must be used for the first split ?
# GINI_sex = 0.48               #
# GINI_car_type = 0.1625        #  
# GINI_Budget = 0.4914286       #  
# CustomerID has no usable info #   } ==> For the first split we use CarType   
# CustomerID must not be used for any model creation

# Desicion tree with class insurance and characteristics Car_type, Budget, Sex 
model <- rpart(Insurance ~ Sex  + CarType    + Budget , method = "class", data = car_insurance, minsplit = 1, minbucket = 1, cp = -1)

#print the decision tree properly.
rpart.plot(model,extra = 104, nn = TRUE)
