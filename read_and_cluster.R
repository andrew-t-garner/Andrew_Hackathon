#Clustering Function

#Packages Required: data.table,ffbase,bit64,gplots,cluster,sqldf, etc.
#Function Inputs: file path, may want to add # of clusters depending on how interactive 

#Comments with ### in front were things I was thinking about trying, but haven't figured out/finished yet.

#Edits/next steps: 
#                         - Confirm all data types are correct for purpose
#                         - Add a sql pivot to get spend by commodity or department for each household record
#                         - Drop any unnecessary columns before clustering
#                         - Check logic of distance matrix and clustering algorithm     
#                         - Check logic of distance matrix and clustering algorithm
#                         - Display/review quality measures for clustering
#                         - Either just return original data frame with clusters applied for use elsewhere,
#                           or think of some interesting visualizations to output with clusters applied

read_combine <- function (file_path)
  {
    #set working directory to folder containing the .csv files
    setwd(file_path)
  
      ### I wanted to automatically pull all files from my directory in, but the custom data types tripped me up.
      ### I hard coded only the files I am using for now.
      ### file_list <- list.files(file_path,full.names = TRUE);
      ### fullset <-lapply( file_list, fread,header=T,verbose="TRUE")

    #Read in files
  
    hh_demographic <-fread("hh_demographic.csv",colClasses = list(character = 6, character = 8)
                           ,stringsAsFactors = TRUE)
    
    transaction <- fread("transaction_data.csv",colClasses = list(character = 1,character = 2, character =3
                           , character = 4, character = 7, character = 9, character = 10 ))
    
    product   <- fread("product.csv",colClasses = list(character= 1), stringsAsFactors = TRUE)
    
    #Join tables together
    
    tprod <-merge(x = transaction, y = product, by = "PRODUCT_ID", all.x = TRUE)
    
    tprodhh <-merge(x = tprod, y = hh_demographic, by = "household_key", all.x = TRUE)
    
      ###gower.dist is supposed to handle qualitative and quantitative varibles, but output wasn't working with
      ###clustering packages, so I used daisy instead
      ###gwr<-gower.dist(hh_demographic)
    
    #Create distance matrix, only using demographics for now, need to change that once data prep is better above
    d<-daisy(hh_demographic,metric = 'gower')
    
    #Agglomerative Nesting (Hierarchical Clustering)
    a<-agnes(d)
    
    #dendrogram
    pltree(a) #or plot(a)
    
    #change to an hclust object (may not need this depending on the functions used)
    hclusta<-as.hclust(a)
    mycl<- cutree(hclusta,k=7)

    
    # add the cluster ID to the original data, it will be something other than hh_demographic
    #once data prep is improved
    with_cluster <- cbind(hh_demographic, clusterID=mycl)

    return(with_cluster)
    
###------------- Everything below this line was experiment with plotting, nothing too useful -------------------------------------   
    
    ###final <-merge(x=transaction,y=with_cluster, by = "household_key", all.x = TRUE)
    ###final <- sqldf('select clusterID, count(distinct basket_id) as visit_count
    ###, sum(sales_value) as lt_sales from final group by clusterID, household_key')
   
    
    ###plot(final$visit_count, final$lt_sales, main="Lifetime Sales vs Visits to Store", 
    ###xlab="Visits", ylab="Sales", pch=19, col=final$clusterID)
    
    ### heatmap code found online below https://www.biostars.org/p/86563/, not sure it will be useful in this case

    ###examine the data with cluster ids attached, and ordered like the heat map
    ####with_cluster[hclusta$order,]
    
    ### get a color palette equal to the number of clusters
    ###clusterCols <- rainbow(length(unique(mycl)))
    
    ### create vector of colors for side bar
    ###myClusterSideBar <- clusterCols[mycl]
    
    #### choose a color palette for the heat map
    ###myheatcol <- rev(redgreen(75))
    
    ### draw the heat map
    ###heatmap.2(hh_demographic, main="Hierarchical Cluster", Rowv=as.dendrogram(hclusta), Colv=NA, dendrogram="row", scale="row", col=myheatcol, density.info="none", trace="none", RowSideColors= myClusterSideBar)
    
  }

