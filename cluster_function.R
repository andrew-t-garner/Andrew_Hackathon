#Clustering Function

#Packages Required: data.table,ffbase,bit64,gplots,cluster,sqldf, etc.
#install.packages(pacman)
#library(pacman)
#pacman::p_load(dplyr,data.table,ffbase,bit64,gplots,cluster,sqldf,reshape,reshape2,NbClust,lattice)
#pkgs<-c('dplyr','data.table','ffbase','bit64','gplots','cluster','sqldf','reshape','reshape2')
#lapply(pkgs, require, character.only = TRUE)
#Function Inputs: file path, may want to add # of clusters depending on how interactive 

#Comments with ### in front were things I was thinking about trying, but haven't figured out/finished yet.

#Edits/next steps: 
#                         

#                         - Check logic of distance matrix and clustering algorithm     
#                         - Display/review quality measures for clustering
#                         - Return original data frame with clusters applied for use elsewhere,


read_combine <- function (file_path)
{
  #set working directory to folder containing the .csv files
  setwd(file_path)  # Andrew's dir C:/Users/atg530/Desktop/The_Complete_Journey/dunnhumby _The Complete Journey_CSV
  
  ### I wanted to automatically pull all files from my directory in, but the custom data types tripped me up.
  ### I hard coded only the files I am using for now.
  ### file_list <- list.files(file_path,full.names = TRUE);
  ### fullset <-lapply( file_list, fread,header=T,verbose="TRUE")
  
  #Read in files
  
  hh_demographic <-fread("hh_demographic.csv",colClasses = list(character = 6, character = 8)
                         ,stringsAsFactors = TRUE)
  
  transaction <- fread("transaction_data.csv",colClasses = list(character = 1, character = 2, character =3
                                                                , character = 4, character = 7, cYharacter = 9, integer = 10), stringsAsFactors = FALSE )
  
  product   <- fread("product.csv",colClasses = list(character= 1), stringsAsFactors = FALSE)
  
  #Creating Tableau Ouput to append Cluster to
  
  Total<-sqldf('SELECT household_key
               ,COUNT(DISTINCT "BASKET_ID") `Total_Baskets`
               ,SUM("SALES_VALUE") `Total_Sales` 
               FROM "transaction" 
               GROUP BY household_key')
  
  Y1_Total <-sqldf('SELECT household_key
                   ,COUNT(DISTINCT "BASKET_ID") `Y1_Baskets`
                   ,SUM("SALES_VALUE") `Y1_Sales` 
                   FROM "transaction" 
                   WHERE "week_no" < 52 
                   GROUP BY household_key')
  
  Y2_Total <-sqldf('SELECT household_key
                   ,COUNT(DISTINCT "BASKET_ID") `Y2_Baskets`
                   ,SUM("SALES_VALUE") `Y2_Sales` 
                   FROM "transaction" 
                   WHERE "week_no" > 51 
                   GROUP BY household_key')
  
  
  tprod <-merge(x = transaction, y = product, by = "PRODUCT_ID", all.x = TRUE)
  
  Total_Dept <-dcast.data.table(as.data.table(tprod),household_key ~ DEPARTMENT, value.var = "SALES_VALUE", sum)
  
  colnames(Total_Dept) <- paste("Total", colnames(Total_Dept), sep = "_")
  colnames(Total_Dept)[1] <- "household_key"
  
  tprod1 <- sqldf('SELECT *
                  FROM "tprod" 
                  WHERE "week_no" < 52')
  
  Y1_Dept <-dcast.data.table(as.data.table(tprod1),household_key ~ DEPARTMENT, value.var = "SALES_VALUE", sum)
  
  colnames(Y1_Dept) <- paste("Y1", colnames(Y1_Dept), sep = "_")
  colnames(Y1_Dept)[1] <- "household_key"
  
  tprod2 <- sqldf('SELECT *
                  FROM "tprod" 
                  WHERE "week_no" > 51')
  
  Y2_Dept <-dcast.data.table(as.data.table(tprod2), household_key ~ DEPARTMENT, value.var = "SALES_VALUE", sum)
  
  colnames(Y2_Dept) <- paste("Y2", colnames(Y2_Dept), sep = "_")
  colnames(Y2_Dept)[1] <- "household_key"
  
  #Join in household demographics, combine total, year 1, and year 2 data for each household
  
  tmp <-merge(x = Total, y = hh_demographic, by = "household_key", all = TRUE)
  
  tmp <-merge(x = tmp, y = Y1_Total, by = "household_key", all = TRUE)
  
  tmp <-merge(x = tmp, y = Y2_Total, by = "household_key", all = TRUE)
  
  tmp <-merge(x = tmp, y = Total_Dept, by = "household_key", all = TRUE)
  
  tmp <-merge(x = tmp, y = Y1_Dept, by = "household_key", all = TRUE)
  
  tmp <-merge(x = tmp, y = Y2_Dept, by = "household_key", all = TRUE)
  
  #Pick out columns for clustering
  #Clustering on "Lifetime" Spend, # of Baskets, Spend by Department
  
  All_Total <-merge(x = Total, y = Total_Dept, by = "household_key", all = TRUE)  
  All_Total_nk <- subset( All_Total, select = -c(household_key))
  
  #cluster_data <- subset( tmp, select = -c(Y1_Baskets : Y2_VIDEO))
  #cluster_data <- subset( cluster_data, select = -c(Y1_Baskets : Total_V1))
  #cluster_data_no_key <- subset( cluster_data, select = -c(household_key))

  
  nbdata<-NbClust(data = All_Total_nk, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "ch", alphaBeale = 0.1)
  plot(x= names(nbdata$All.index), y=nbdata$All.index)
  
  with_cluster <- cbind(nbdata$Best.partition, All_Total)
  colnames(with_cluster)[1] <- "cluster"
  
  cl_key <- subset( with_cluster, select = c(cluster, household_key))
  
  sqldf('select cluster, count(*) from "cl_key" group by "cluster"')
  
  #splom(with_cluster[3:8], main="analysis", color= )
  
  final <- merge(x=cl_key, y=tmp,all=TRUE)
  
  write.csv(final,'clustered_by_spending.csv', row.names = FALSE)
  
  #Create distance matrix
  ###gower.dist is supposed to handle qualitative and quantitative varibles, but output wasn't working with
  ###clustering packages, so I used daisy instead
  ###gwr<-gower.dist(hh_demographic)
  
  d<-daisy(cluster_data_no_key)
  
  #Agglomerative Nesting (Hierarchical Clustering)
  a<-agnes(d)
  
  #dendrogram
  #pltree(a) #or 
  plot(a)
  
  #change to an hclust object (may not need this depending on the functions used)
  
  hclusta<-as.hclust(a)
  
  mycl<- cutree(hclusta,k=3)
  
  
  # add the cluster ID to the original data, it will be something other than hh_demographic
  #once data prep is improved
  with_cluster <- cbind(cluster_data, clusterID=mycl)
  
  return(with_cluster)
}
  
  
  
  
  


