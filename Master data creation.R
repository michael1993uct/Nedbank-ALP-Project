rm(list = ls())
library("dplyr")
setwd("/Users/michaellevin/Nedbank ALP Directory")
listoffiles = c("ALP Gold.csv", "Copy of ALP template FMCGs.csv", "ALP Lungani.csv", "Construction - Materials.csv", "ALP Palesa.csv")
creatingmasterdata <- function(filelist) {
       for (i in 1:length(filelist))  {
           if (i == 1) {
             data = read.csv(filelist[i], sep = ";", stringsAsFactors=FALSE)
             data = data[1:13]
           }
          else {
            tempdata = read.csv(filelist[i], sep = ";", stringsAsFactors=FALSE)
            data = rbind(data, tempdata[1:13])
          }
       }
  return(data)
}



data <- creatingmasterdata(listoffiles)
data = data[1:13]
data$Industry.B[data$Industry.B == "Food producer"] <- "Food Producer" 
data$Company[data$Company == "Sibanye Gold"] <- "Sibanye"
colnames(data)[7] <- "Total.Current.Liabilities"
data$Industry.B <- sub("(.)", "\\U\\1", data$Industry.B, perl=TRUE)
data <- subset(data, is.na(data$Total.Non.Current.Assets) != T  )
data$Industry.B[data$Industry.B == "Food and beverages"] <-  "Food Producer"
data$Industry.B[data$Industry.B == "Materials"] <- "Construction and materials"
data$Company[data$Company == "Aveng Ltd"] <- "Aveng"
saveRDS(data, file = "masterdata.Rdata")
