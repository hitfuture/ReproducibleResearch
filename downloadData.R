library(data.table)
site<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
file.name<-"StormData.csv.bz2"
data.dir <-"./data"
path.name<-paste(data.dir,file.name,sep="/")
# downloadData.R
#Step 0. Download data
## Create the ./data directory if it does not exist.
if(!file.exists(data.dir)) {
        dir.create(data.dir) 
        message(paste("Creating directory",data.dir,sep=": "))
}
## Download the data from the Internet if it does not exist.
if(!file.exists(path.name)) {
        data.source.url<-site
        download.file(url=data.source.url,destfile=path.name,method="curl")
        download.date<-date()
        unzip(path.name,exdir = data.dir)
        unzip(zipfile = path.name,list = TRUE)
        
} 


