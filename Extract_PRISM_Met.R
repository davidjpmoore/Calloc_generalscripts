# ------------------------------------
# Extracting Monthly Met from PRISM for all of Ross's Sites
# C. Rollinson 10 Feb 2016
# ------------------------------------

# ------------------------------------
# Load libraries
# ------------------------------------
library(raster); library(maps)
# ------------------------------------

# ------------------------------------
# Load file with site names & locations & turn it into a spatial file
# ------------------------------------
plot.dat <- read.csv("~/PhD/Carbon Research/Calloc_TreeRingNPP/raw_input_files/DOE_plus_valles.csv")
plot.dat$Site.Code <- as.factor(substr(plot.dat$PlotID, 1,2))
summary(plot.dat)

# Aggregating the location of plots up to the site level so that:
#  1) We have lat/lon for missing plots
#  2) We're saving ourselves a lot of time in the extraction
#  -- Note: this makes the assumption that there are no microclimatic differences among sites
site.dat <- aggregate(plot.dat[,c("latitude", "longitude", "elevation")],
                      by=plot.dat[,c("Site..Tower.", "Site.Code")], 
                      FUN=mean, na.rm=T)

# Make sure Longitude is negative
site.dat$longitude <- ifelse(site.dat$longitude>0, site.dat$longitude*-1, site.dat$longitude)
summary(site.dat)

# Get rid of sites without any lat/lon info
site.dat <- site.dat[!is.na(site.dat$longitude),]
summary(site.dat)

# Making site a spatial file & graphing to make sure everything looks okay
site.loc <- SpatialPointsDataFrame(coords=site.dat[,c("longitude", "latitude")], site.dat, proj4string=CRS("+proj=longlat"))

# A quick & dirty plot to double check things 
#  (ggplot can make it a lot prettier but requires extra steps)
map("state", plot=T,lty="solid", col="gray30", lwd=1.5)
plot(site.loc, pch=19, add=T, cex=2, col="blue")
# ------------------------------------


# ------------------------------------
# Set up & extract the PRISM data
# ------------------------------------
# Directory containing PRISM data & what variables we have
dir.prism <- "~/Desktop/PRISM Data"
var.prism <- c("ppt", "tmax", "tmin", "tmean")


# Simplifying our lives by figuring out which directories we have for one variable
dir.tmean <- dir(dir.prism, "tmean")
dir.tmax  <- dir(dir.prism, "tmax" )
dir.tmin  <- dir(dir.prism, "tmin" )
dir.ppt   <- dir(dir.prism, "ppt"  )


# ------------------------
# Doing the Extractions, looping through the two different sets of PRISM data 
#   1) 1895-1980
#   2) 1980-2015
# Note: just fo simplicity, doing everything at once and throwing into 1 met 
#       file in "long" format
# ------------------------
for(j in 1:length(dir.tmean)){

	# Get list of .bil files in one of those folder
	files.tmean <- dir(file.path(dir.prism, dir.tmean[j]), "_bil.bil")
	files.tmax  <- dir(file.path(dir.prism, dir.tmax [j]), "_bil.bil")
	files.tmin  <- dir(file.path(dir.prism, dir.tmin [j]), "_bil.bil")
	files.ppt   <- dir(file.path(dir.prism, dir.ppt  [j]), "_bil.bil")

	# remove the .xml files from these lists
	files.tmean <- files.tmean[!substr(files.tmean, nchar(files.tmean)-3, nchar(files.tmean))==".xml"]
	files.tmax  <- files.tmax[!substr(files.tmax, nchar(files.tmax)-3, nchar(files.tmax))==".xml"]
	files.tmin  <- files.tmin[!substr(files.tmin, nchar(files.tmin)-3, nchar(files.tmin))==".xml"]
	files.ppt   <- files.ppt[!substr(files.ppt, nchar(files.ppt)-3, nchar(files.ppt))==".xml"]



	for(i in 1:length(files.tmean)){
		# Looping through the files one by one to save memory & time
		tmean <- raster(file.path(dir.prism, dir.tmean[j], files.tmean[i]))
		tmax  <- raster(file.path(dir.prism, dir.tmax [j], files.tmax [i]))
		tmin  <- raster(file.path(dir.prism, dir.tmin [j], files.tmin [i]))
		ppt   <- raster(file.path(dir.prism, dir.ppt  [j], files.ppt  [i]))
		
		# Finding the year & month we're currently extracting
		time.now <- strsplit(files.tmean[i], "_")[[1]][5] # splits the name apart & then pulls the timestamp
		year.now <- substr(time.now, 1, 4)
		mo.now   <- substr(time.now, 5,6)
		print(paste0("Extracting: ", year.now, " - ", mo.now)) # printing some info on the status of the extractions
	
		# Stacking the met together to make the extraction a little smoother
		met.now <- stack(tmean, tmax, tmin, ppt)
		names(met.now) <- c("tmean", "tmax", "tmin", "ppt")
		# plot(met.now)		
		
		# Extract the met data
		met.extract <- data.frame(extract(met.now, site.loc, method = "bilinear"))
		met.extract$Year      <- as.numeric(year.now) # Lets let year be numeric
		met.extract$Month     <- as.ordered(mo.now) # Keep month as a factor so we keep the 2-digit system
		met.extract$Site.Code <- site.loc$Site.Code
		met.extract$Site.Name <- site.loc$Site..Tower.
		
		# Binding stuff together!!!
		#  If this is the first time through, copy everything into a new object, otherwise rbind it
		if(j==1 & i==1) sites.met <- met.extract else sites.met <- rbind(sites.met, met.extract)
	}
}
summary(sites.met)
write.csv(sites.met, "prism_met_sites.csv", row.names=F)
# ------------------------
# ------------------------------------

sites.met <- read.csv("prism_met_sites.csv")

# ------------------------------------
# Add in the lags & re-save everything
# ------------------------------------
# Making sure month is 2-digits
sites.met$Month <- as.factor(ifelse(nchar(sites.met$Month)==1, paste0(0, sites.met$Month), sites.met$Month))
summary(sites.met)

for(y in (min(sites.met[sites.met$Site.Name==s, "Year"])+1):max(sites.met[sites.met$Site.Name==s, "Year"])){
	print(paste0("Processing Year: ", y))
	# Making a temporary dataframe with the lages
	df.temp <- data.frame(tmean = sites.met[sites.met$Year==(y-1) & 
	                                        !substr(sites.met$Month,1,1)=="p", "tmean"],
	                      tmax  = sites.met[sites.met$Year==(y-1) & 
	                                        !substr(sites.met$Month,1,1)=="p", "tmax"],
	                      tmin  = sites.met[sites.met$Year==(y-1) & 
	                                        !substr(sites.met$Month,1,1)=="p", "tmin"],
	                      ppt   = sites.met[sites.met$Year==(y-1) & 
	                                        !substr(sites.met$Month,1,1)=="p", "ppt"],
	                      Year  = y,
	                      Month = paste0("p", sites.met[sites.met$Year==(y-1) & 
	                                        !substr(sites.met$Month,1,1)=="p", "Month"]),
	                      Site.Code = sites.met[sites.met$Year==(y-1) & 
	                                        !substr(sites.met$Month,1,1)=="p", "Site.Code"],
	                      Site.Name = sites.met[sites.met$Year==(y-1) & 
	                                        !substr(sites.met$Month,1,1)=="p", "Site.Name"]
	                      )

	# adding the lag months to the dataframe
	sites.met <- rbind(sites.met, df.temp)
}
summary(sites.met)

unique(sites.met$Month)

# -------------
# Adding some names and orders to make life easier
library(car)
# First setting up the order of months; previous year now negative months from Jan
#   So December = -1, Nov = -2, etc
sites.met$Month.Order <- as.ordered(ifelse(substr(sites.met$Month,1,1)=="p", as.numeric(substr(sites.met$Month,2,3))-13, sites.met$Month))
summary(sites.met)
unique(sites.met$Month.Order)

sites.met$Month.Name <- recode(sites.met$Month.Order, "'-12'='pJan'; '-11'='pFeb'; '-10'='pMar'; '-9'='pApr'; '-8'='pMay'; '-7'='pJun'; '-6'='pJul'; '-5'='pAug'; '-4'='pSep'; '-3'='pOct'; '-2'='pNov'; '-1'='pDec'; '1'='Jan'; '2'='Feb'; '3'='Mar'; '4'='Apr'; '5'='May'; '6'='Jun'; '7'='Jul'; '8'='Aug'; '9'='Sep'; '10'='Oct'; '11'='Nov'; '12'='Dec' ")
summary(sites.met)
unique(sites.met$Month)
unique(sites.met$Month.Order)
unique(sites.met$Month.Name)
# -------------

write.csv(sites.met, "prism_met_sites_lags.csv", row.names=F)
# ------------------------------------

sites.met <- read.csv("prism_met_sites_lags.csv")
sites.met$Month.Order <- as.ordered(sites.met$Month.Order)
# ------------------------------------
# Convert to wide format
# ------------------------------------
library(reshape2)

# Need to make Year a factor for this to work
sites.met$Year <- as.ordered(sites.met$Year)
summary(sites.met)

sites.met$Month.Name <- factor(sites.met$Month.Name, levels=c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul", "pAug", "pSep", "pOct", "pNov", "pDec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# ---------------------
# Saving Tmean
# ---------------------
tmean.wide <- recast(sites.met[,c("Site.Name", "Year", "Month.Name","tmean")], Site.Name+Year~Month.Name)
summary(tmean.wide)

write.csv(tmean.wide, "prism_met_sites_wide_tmean.csv", row.names=F)
# ---------------------

# ---------------------
# Saving Tmax
# ---------------------
tmax.wide <- recast(sites.met[,c("Site.Name", "Year", "Month.Name","tmax")], Site.Name+Year~Month.Name)
summary(tmax.wide)

write.csv(tmax.wide, "prism_met_sites_wide_tmax.csv", row.names=F)
# ---------------------

# ---------------------
# Saving Tmin
# ---------------------
tmin.wide <- recast(sites.met[,c("Site.Name", "Year", "Month.Name","tmin")], Site.Name+Year~Month.Name)
summary(tmin.wide)

write.csv(tmin.wide, "prism_met_sites_wide_tmin.csv", row.names=F)
# ---------------------

# ---------------------
# Saving Precip
# ---------------------
ppt.wide <- recast(sites.met[,c("Site.Name", "Year", "Month.Name","ppt")], Site.Name+Year~Month.Name)
summary(ppt.wide)

write.csv(ppt.wide, "prism_met_sites_wide_ppt.csv", row.names=F)
# ---------------------

# ------------------------------------
