# sqlite DB to save and read logger files
require("DBI")
require("RSQLite")
require("leaflet")
require("rgdal")

# WGS to UTM and vice versa

wgstoutm<-function(x,y){
  zone<-(floor((x + 180)/6) %% 60) + 1
  #xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  xy <- data.frame(cbind("X"=x,"Y"=y))
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(cbind.data.frame(X=res$X,Y=res$Y,zone))
}

utmtowgs<-function(x,y,zone){
  #xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  xy <- data.frame(cbind("X"=x,"Y"=y))
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS(paste0("+proj=utm +zone=",zone," +datum=WGS84"))  ## for example
  res <- spTransform(xy, CRS("+proj=longlat +datum=WGS84"))
  return(as.data.frame(res))
}

# create wgs coordinates for kegel presentation

kegelcreation<-function(x,y,dir,length,deg){
  # first convert to utm
  utm<-wgstoutm(x,y)
  
  # then calc new coords
  
  kr <- (dir-deg/2)/180*pi
  kl <- (dir+deg/2)/180*pi
  
  utm_x1 <- utm$X + sin(kr) * length
  utm_y1 <- utm$Y + cos(kr) * length
  utm_zone1 <- utm$zone # if the zones switch, this setting needs to be changed 
  
  utm_x2 <- utm$X + sin(kl) * length
  utm_y2 <- utm$Y + cos(kl) * length
  utm_zone2 <- utm$zone # if the zones switch, this setting needs to be changed 
  
  # convert back to wgs
  wgs<-utmtowgs(c(utm_x1,utm_x2),c(utm_y1,utm_y2),c(utm_zone1,utm_zone2))
  return(wgs)
}

# read csv logger file to db

add_logger_file_to_db <- function(file,filepath_db,receiver){
  #check how many lines to skip
  
  
  #data<-read.csv2(filepath,dec = ".",skip = nlines, stringsAsFactors = FALSE, row.names = NULL)
  data<-skip_n_lines_auto(file)
  data$X<-NULL
  con = dbConnect(RSQLite::SQLite(), dbname=filepath_db)
  if(any(dbListTables(con)==receiver)){
    tmp_df<-dbReadTable(con,receiver)
    df<-unique(rbind(data,tmp_df))
    dbWriteTable(con, paste0("receiver_",receiver), df, overwrite=TRUE)
  }
  else{
    dbWriteTable(con, paste0("receiver_",receiver), data)
  }
  dbDisconnect(con)
}






# read project meta file: locations

read_metafile <- function(filepath_meta){
  meta<-read.csv2(filepath_meta, dec=".", stringsAsFactors = FALSE, row.names = NULL)
}

# add polygon to map
create_antenna_map<-function(meta){
  
  m<- leaflet() %>% addTiles() %>% addCircles(lng=meta$Long,lat = meta$Lat)
  for(p in 1:dim(meta)[1]){
    print(p)
    x<-meta$Long[p]
    y<-meta$Lat[p]
    direction<-meta$Direction[p]
    wgs<-kegelcreation(x,y,direction,1000,60)
    m<- m %>% addPolygons(lng=c(wgs$X,x),lat=c(wgs$Y,y),fillOpacity=0.4,stroke=FALSE )
  }
  return(m)
}

# read sqlite to data frame

# find right header -> header = columns

skip_n_lines_auto <- function(file){
  tmp<-readLines(file, n=30)
  n<-grep("time;duration;freq;bw;strength;", tmp)-1
  print(n)
  data<-read.csv2(file,dec = ".",skip = n, row.names = NULL, stringsAsFactors = FALSE)
  return(data)
}

# read and add all data to a specific receiver in dir
# choose.dir() to choose dir

get_all_logger_data_in_fodler <-function(folder, file_db ,receiver){
  #get all file names first
  
  files <- list.files(folder)
  
  for(f in files){
    print(paste0(folder,f))
    add_logger_file_to_db(paste0(folder,f),file_db,receiver)
  }
  
}

# add location properties

get_logger_data_in_subfodlers <-function(folder, file_db){
  #get all file names first
  
  folders <- list.dirs(folder, full.names = FALSE, recursive = FALSE)
  
  for(sf in folders){
    get_all_logger_data_in_fodler(paste0(folder,sf,"/"),file_db,sf)
    }
}
