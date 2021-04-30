install.packages("readxl")
install.packages("dplyr")
installed.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(readxl)
#PART 1
awards_test <- read_excel("Awards_Test.xlsx")
regions <- read_excel("AMP_Regions_MY19.xlsx")
measures <- read_excel("Measure List.xlsx")

#Join Data
data <- merge(x=awards_test,y=regions,by="po_id",all.x=TRUE)
data<- merge(x=data,y=measures, by = "measure_spec_abbr", all.x= TRUE)


  #PART A/PART B

###Flag INVALID IF
###Clinical quality measures with denominators less than 30 
###OR  Patient experience measures with reliability less than 0.70


data$validity <- ifelse(data$denominator < 30 & data$domain == "Clinical Quality" | data$reliability <= 0.69 & data$domain == "Patient Experience", "INVALID", "VALID" )

#Get all Physician Organization names 
PO_name <- unique(data$po_name)

#PART 2
eligible <- list()
for (x in PO_name) {
  num_PO_measure = nrow(subset(data,data$po_name == x))
  half = nrow(subset(data,data$po_name == x))/2
  invalid = nrow(subset(data,data$po_name == x & data$validity == "INVALID"))
  if (invalid > half) {
    next
  } else (eligible[x] <- x)
}


data2 <- subset(data, data$po_name %in% eligible)
PO_name2 <- unique(data2$po_name)
length(PO_name2)

#Show the number of observations that are in each PO
table(data2$po_name)

#PART 1
global_AVG <- list()
global_MIN <- list()
global_MAX <- list()
for (x in PO_name2){
  global_avg = mean(subset(data2,data2$po_name == x & data2$validity == "VALID")$rate)
  global_min = min(subset(data2,data2$po_name == x & data2$validity == "VALID")$rate)
  global_max = max(subset(data2,data2$po_name == x & data2$validity == "VALID")$rate)
  global_AVG[[x]]<- global_avg
  global_MIN[[x]]<- global_min
  global_MAX[[x]]<- global_max
}


#PART 2
diff <- list()
for (x in PO_name2){
  rate = subset(data2,data2$po_name == x & data2$validity == "VALID")$rate
  diff_rate = abs(as.numeric(global_AVG[x]) - rate)
  diff[[x]] <- diff_rate
  
}

#PART 3

avg_cq <- list()
avg_pxe <- list()
for (x in PO_name2){
  x1 <- subset(data2,data2$po_name == x & data2$validity == "VALID")
  cq_rate <- subset(x1,x1$domain == "Clinical Quality")$rate
  pxe_rate <- subset(x1,x1$domain == "Patient Experience")$rate
  cq_diff_rate <- abs(as.numeric(global_AVG[x]) - cq_rate)
  pxe_diff_rate <- abs(as.numeric(global_AVG[x]) - pxe_rate)
  avg_cq[[x]] <- mean(cq_diff_rate)
  avg_pxe[[x]] <- mean(pxe_diff_rate)
  
}

#PART 4

#Compute invalid PO average measure rate

avg_cq_iv <- list()
avg_pxe_iv <- list()
for (x in PO_name2){
  x1 <- subset(data2,data2$po_name == x & data2$validity == "INVALID")
  cq_rate_iv <- subset(x1,x1$domain == "Clinical Quality")$rate
  pxe_rate_iv <- subset(x1,x1$domain == "Patient Experience")$rate
  cq_diff_rate <- abs(as.numeric(global_AVG[x]) - cq_rate_iv)
  pxe_diff_rate <- abs(as.numeric(global_AVG[x]) - pxe_rate_iv)
  avg_cq_iv[x] <- mean(cq_diff_rate)
  avg_pxe_iv[x] <- mean(pxe_diff_rate)
  
}

imputed_cq <- list()
imputed_pxe <- list()
for (x in PO_name2){
  IR_cq <- as.numeric(global_AVG[x]) + as.numeric(avg_cq_iv[x])
  IR_pxe <- as.numeric(global_AVG[x]) + as.numeric(avg_pxe_iv[x])
  imputed_cq[x] <- IR_cq
  imputed_pxe[x] <- IR_pxe
}

#Create new column "new_rate"
data2$new_rate = data2$rate


#Remove NA Imputed Values
imputed_pxe <- imputed_pxe %>% discard(is.na)
imputed_cq <- imputed_cq %>% discard(is.na)

#Extract PO_names that are INVALID from each domain
invalid_pxe <- unique(subset(data2,data2$domain == "Patient Experience" & data2$validity == "INVALID")$po_name)
invalid_cq <- unique(subset(data2,data2$domain == "Clinical Quality" & data2$validity == "INVALID")$po_name)

#Find original rate values that are INVALID and replace with IMPUTED rate values
for (i in invalid_pxe){
  a= data2$new_rate[data2$po_name == i & data2$domain == "Patient Experience" & data2$validity == "INVALID"]
  b= replace(a,c(1:length(a)),as.numeric(imputed_pxe[i]))
  data2$new_rate[data2$po_name == i & data2$domain == "Patient Experience" & data2$validity == "INVALID"] = b
}

for (i in invalid_cq){
  a= data2$new_rate[data2$po_name == i & data2$domain == "Clinical Quality" & data2$validity == "INVALID"]
  b= replace(a,c(1:length(a)),as.numeric(imputed_cq[i]))
  data2$new_rate[data2$po_name == i & data2$domain == "Clinical Quality" & data2$validity == "INVALID"] = b
}

data3 <- data2

#PART 1
PO_avg_pxe <- list()
PO_avg_cq <- list()
for (x in PO_name2){
  avg1 = mean(data3$new_rate[data2$po_name == x & data2$domain == "Patient Experience"])
  avg2 = mean(data3$new_rate[data2$po_name == x & data2$domain == "Clinical Quality"])
  PO_avg_pxe[x] = avg1
  PO_avg_cq[x] = avg2
}

#PART 2
PO_avg_pxe <- PO_avg_pxe %>% discard(is.na)
PO_avg_cq <- PO_avg_cq %>% discard(is.na)

cq_median <- median(as.numeric(PO_avg_cq))
pxe_median <- median(as.numeric(PO_avg_pxe))

#PART 3

cost_median <- median(subset(data3,data3$domain == "Cost")$new_rate)

#PART 4
winners <- list()
for (i in 1:length(PO_avg_cq)){
  if(as.numeric(unlist(PO_avg_cq[i])) >= cq_median & as.numeric(unlist(PO_avg_pxe[i])) >= pxe_median &
     as.numeric(unlist(PO_avg_cq[i])) <= cost_median & as.numeric(unlist(PO_avg_pxe[i])) <= cost_median){
    winners[i] <- names(PO_avg_cq)[i]
  }
}

winners <- winners[!sapply(winners, is.null)]
winners <- unlist(winners)

#Question 1
winners

#Question 2
print("They need to improve their Patient Experience in order to win the award. They need to improve their measure rate by 0.4455 units ")
data3$awards<- ifelse(data3$po_name %in% winners, "Winner", "Loser")

write.csv(data3,'/Users/khristionlambert/Library/Mobile Documents/com~apple~CloudDocs/Work/data3.csv')




