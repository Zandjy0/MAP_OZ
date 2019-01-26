#' Map Opportunity Zones
#' 
#' A function to find census key, chart addresses, and evaluates whether the address is within the CDFI opportunity zones (OZ).
#' To run this function you must obtain a google API key by setting up an account here (https://developers.google.com/maps/documentation/embed/get-api-key).
#'
#' Created by Steven J. Barbeaux January 25, 2019
#' @param ADDRESS is the vector of address/es which can be input singularly following the format:/n address="1600 Pennsylvania Avenue NW, Washington, DC, 20500"/n or it can be input as a list in a .csv or .xlsx file with a column with the addresses named "addresses" and each row seperate as in :/n addresses:/n "5339 ESSERVILLE RD, NORTON, VA, 24273"/n "1305 11th Ave SE, Hickory, NC, 28602"/n "1836 SUNDANCE CIR, Yadkinville, NC, 27055"/n "514 Peach Street, Yadkinville, NC, 27055"/n
#' @param ZOOM allows one to change the scale of the map from 1 (lowest resolution) to 20 (highest resolution).
#' @param OZ_CHECK set to TRUE causes the program to evaluate whether the property is within a CDFI OZ, FALSE skips this check.
#' @param MAP set to TRUE outputs map to either the screen or to PDF as described below.
#' @param PDF set to TRUE outputs a PDF file with the sattelite maps, one page per address. FALSE plots the maps in an R window.
#' @param PDF_NAME is the name of the PDF file to be exported.
#' @param XL_OUT set to TRUE exports an Excel file to the working directory of the addresses with Census tract id and OZ determination.
#' @param XL_NAME set to the name of the Excel file you wish to write, this program will over write any files of the same name.
#' @param API_KEY is the google API key for google maps.
#' @keywords OZ
#' @export
#' @examples
#' MAP_OZ()

MAP_OZ<-function(DIR=".",ADDRESS=address,ZOOM=17, OZ_CHECK=TRUE, MAP=TRUE,PDF=TRUE, PDF_NAME="MAPS.pdf", XL_OUT=FALSE ,XL_NAME="OZ_MAP.xlsx",API_KEY=api_key)
{
  setwd(DIR)
 
 ## add required libraries
  if(!requireNamespace("grid")) install.packages("grid")
  if(!requireNamespace("data.table")) install.packages("data.table")
  if(!requireNamespace("devtools")) install.packages("devtools")
  if(!requireNamespace("ggmap")) devtools::install_github("dkahle/ggmap", ref = "tidyup")
  if(!requireNamespace("ggplot2")) install.packages("ggplot2")
  if(!requireNamespace("xlsx")) install.packages("xlsx")
  if(!requireNamespace("tigris")) install.packages("tigris")
  library(grid)
  library(data.table)
  library(devtools)
  library(ggmap)
  library(ggplot2)
  library(xlsx)
  library(tigris)
  
 
# add API key for google 
  if(!has_google_key()) register_google(key = API_KEY)

## check to see if the address is a csv file, if so read it in
  if(grepl(".CSV",toupper(ADDRESS))){ 
    address <- data.table(read.csv(paste0(DIR,"/",ADDRESS),stringsAsFactors=FALSE))
    } else if (grepl(".XLSX", toupper(ADDRESS))){
      address <- data.table(read.xlsx(paste0(DIR,"/",ADDRESS), sheetIndex = 1))
    } else {address<-data.table(addresses=ADDRESS)}

## if there is more than one address check for duplicates and remove them
  if (nrow(address)>1){
    address$addresses=toupper(address$addresses)
    nd<-duplicated(address$addresses) ## check for duplicate addresses
    nd<-length(nd[nd==T])
    print(paste0("Removed ",nd," duplicate addresses from file"))
    address=unique(address)
  }
 
  if(length(address$addresses)>20){print("This may take some time")}
 
 ## finding the address on google and picking up latitude and longitude
  x2=geocode(address$addresses, source="google",output='all', messaging=FALSE)            
 
 ## Placing address and other attributes in data.table
  Places<-data.table(Address=address$addresses,lat=0,lon=0,status="good",location_type=NA)
  
  if(nrow(Places)==1){
    Places$status[1]=x2$status
    Places$location_type[1]<-x2$results[[1]]$geometry$location_type
    
    if(x2$status =="ZERO_RESULTS"){print("No results for that address!")}

    if(x2$status !="ZERO_RESULTS"){
     Places$lat[1]<-x2$results[[1]]$geometry$location$lat
     Places$lon[1]<-x2$results[[1]]$geometry$location$lng
    }
    Places3<-Places
  }
  
  if(nrow(Places)>1){
    cz <- 0
    for(i in 1:nrow(Places)){
     if(x2[[i]]$status =="ZERO_RESULTS"){cz<-cz+1}

      Places$status[i]=x2[[i]]$status
      Places$location_type[i]<-x2[[i]]$results[[1]]$geometry$location_type
  
      if(Places$status[i] !="ZERO_RESULTS"){
        Places$lat[i]<-x2[[i]]$results[[1]]$geometry$location$lat
        Places$lon[i]<-x2[[i]]$results[[1]]$geometry$location$lng
      }
    }
    Places2<-Places[duplicated(Places[,c("lat","lon")])==F]
    dr<-nrow(Places)-nrow(Places2)
    print(paste0(cz," locations were not found and ",dr," additional duplicates were removed"))
    Places3<-Places2[!is.na(lon)]
  }

## finding the census code for the address
  Places3$census_code <- apply(Places3, 1, function(row) call_geolocator_latlon(row['lat'], row['lon']))

## if OZ_CHECK is true then pull OZ data from online and check it against geoid from addresses
  if(OZ_CHECK){
    if(!file.exists("CDFI_OZ.xlsx")){download.file(url="https://www.cdfifund.gov/Documents/Designated%20QOZs.12.14.18.xlsx",destfile="CDFI_OZ.xlsx", mode='wb',quiet = TRUE)}
    OZ <- read.xlsx("CDFI_OZ.xlsx",sheetIndex=1)
    names(OZ)=c("STATE","COUNTY","GEOID","DESIGNATION","YEARS")
    OZ<-data.table(OZ)
    OZ$GEOID<-as.character(OZ$GEOID)
    Places3$GEOID<-as.character(substr(Places3$census_code,1,11))
    Places3$OZ_DESIGNATION=OZ$DESIGNATION[match(Places3$GEOID,OZ$GEOID)]
    Places3$OZ="In OZ"
    Places3[is.na(OZ_DESIGNATION)]$OZ="Not OZ"
  } else {Places3$OZ="No OZ Check"}

## parse address for plotting and sorting
  NAMES<-strsplit(as.character(Places3$Address),split=", ") 
  NAMES2<-data.table(do.call(rbind,NAMES))

  Places3$ADDRESS <-NAMES2$V1
  Places3$CITY    <-NAMES2$V2
  Places3$STATE   <-NAMES2$V3
  Places3$ZIPCODE <-NAMES2$V4
## if the house address line has two parts, they need to be combined
  Places3[NAMES2$V1 != NAMES2$V5]$ADDRESS<-paste(NAMES2[NAMES2$V1 != NAMES2$V5]$V1,NAMES2[NAMES2$V1 != NAMES2$V5]$V2,sep=" ")
  Places3[NAMES2$V1 != NAMES2$V5]$CITY<-NAMES2[NAMES2$V1!=NAMES2$V5]$V3
  Places3[NAMES2$V1 != NAMES2$V5]$STATE<-NAMES2[NAMES2$V1!=NAMES2$V5]$V4
  Places3[NAMES2$V1 != NAMES2$V5]$ZIPCODE<-NAMES2[NAMES2$V1!=NAMES2$V5]$V5
## sort addresses by zipcode and address
  Places3<-Places3[order(ZIPCODE,ADDRESS)]
  ## Create Map?  
  if(MAP){
## plotting or PDF?
    if(PDF){ pdf(file=PDF_NAME,width=8,height=8,pointsize=4)}
  
    for(i in 1:nrow(Places3)){
      if(!PDF){windows(width=12,height=12,pointsize=12)}
        vp <- viewport(width = 0.25, height = 0.25, x = 1, y = unit(0.7, "lines"), just = c("right","bottom"))
        suppressMessages(usa1<-get_map(location = c(lon = Places3$lon[i], lat =Places3$lat[i]), zoom = 5,maptype="toner-lite"))
        suppressMessages(USA <- get_map(location = c(lon = Places3$lon[i], lat =Places3$lat[i]), zoom = ZOOM, maptype = "satellite"))
        d<-ggmap(USA)+geom_point(data=Places3,aes(x=lon[i],y= lat[i]),color="red")+ggtitle(paste0(Places3$Address[i]," - ",Places3$location_type," - ",Places3$OZ))+
            theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),
           axis.text.y=element_blank(),axis.ticks.y=element_blank())
        e<-ggmap(usa1,extent = "device")+ylab("")+xlab("")+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
          geom_point(data=Places3,aes(x=lon[i],y= lat[i]),size=0.35,color="red")
        print(d)
        print(e,vp=vp)
        remove(USA)
        remove(usa1)
      }
    if(PDF){dev.off()}}
  if(XL_OUT){write.xlsx(Places3,XL_NAME,row.names=FALSE)}
  return(Places3)
}