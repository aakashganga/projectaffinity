library(readxl)
library(stats)
library(fastmatch)
library(ggplot2)
library(tidyr)
library(data.table)
library(dplyr)

#load files
df <- read_excel("oilsandpastaGBB.xlsx")

#Select data for test
sampledata <- select(df, Category, Subcategory, 'PRODUCT TYPE', FLAVOR)

#Factor the data frame from chr to factor
sampledata[] <- lapply(sampledata, factor) #the '[]' keeps the dataframe structure
colnames <- names(sampledata)

#One Hot Encode and remove original columns, leave only numeric columns

#Encoder Category
for(unique_value in unique(sampledata$Category)){
  sampledata[paste("Category", unique_value, sep = "_")] <- ifelse(sampledata$Category == unique_value, 1, 0)
}

#Encoder Subcategory
for(unique_value in unique(sampledata$Subcategory)){
  sampledata[paste("Subcategory", unique_value, sep = ".")] <- ifelse(sampledata$Subcategory == unique_value, 1, 0)
}

#Encoder PRODUCT TYPE
for(unique_value in unique(sampledata$'PRODUCT TYPE')){
  sampledata[paste("PRODUCT TYPE", unique_value, sep = ".")] <- ifelse(sampledata$'PRODUCT TYPE' == unique_value, 1, 0)
}
#Encoder FLAVOR
for(unique_value in unique(sampledata$FLAVOR)){
  sampledata[paste("Flavor", unique_value, sep = ".")] <- ifelse(sampledata$FLAVOR == unique_value, 1, 0)
}

sampledata$Category = NULL
sampledata$Subcategory = NULL
sampledata$'PRODUCT TYPE' = NULL
sampledata$FLAVOR = NULL


#TRY #1 - find distance as matrix.
sampledist<- dist(as.matrix(sampledata))
plot(hclust(sampledist))

#TRY #2 - using recommenderlab to find distance
library(recommenderlab)
samplematrix <- as.matrix(sampledata)
sampleratingmatrix <- as(samplematrix, "binaryRatingMatrix")
image(sampleratingmatrix)

samplesimilarity <-similarity(sampleratingmatrix, y=NULL, method="Jaccard")

#TRY #3 - maybe kmodes will work better
samplecategorical <- select(df, Category, Subcategory, 'PRODUCT TYPE', FLAVOR)
library(klaR)
samplecluster <-kmodes(samplecategorical, modes = 40, iter.max=100, weighted = TRUE)




#How to connect to an input? If I built a shinyapp and had a textbox to enter in the UPC, how can i return topN list?

#adding SKU to the sampledata?
#SKU TO BINARY
df$`UPC EAN13`<-setnames(df,"UPC EAN13", "UPC13")
masterdata <- cbind(df$UPC13, sampledata)
masterdata <- setnames(masterdata, "df$UPC13", 'UPC13')

#NOT WORKING?
SKU <- function(x){
  relay =fmatch(x, masterdata$UPC13)
  relay
}
input = SKU(700175364999)
input
inputx = masterdata[input,]