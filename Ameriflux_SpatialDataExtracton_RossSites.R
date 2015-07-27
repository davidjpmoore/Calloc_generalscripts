###############################################################################
# Making a map of Ross's Ameriflux study sites with Ecosystem Types Underneath
# -----------------------------------------------------------------------------
#
# --------------------------
# General Workflow
# --------------------------
# 1. Read, Plot, & Extract USGS Ecosystem Layer
#    -- More detail available at: http://rmgsc.cr.usgs.gov/ecosystems/index.shtml
#    -- Data downloadable at: http://rmgsc.cr.usgs.gov/outgoing/ecosystems/USdata/
#           select labeled_ecosystems_30m.zip for the files used here
#           other files may be of interest for analyses as well
# 2. Read, Format, & Plot NLDC land cover layer
#    -- More detail available at: http://www.mrlc.gov/nlcd2011.php
#    -- Data downloadable at: http://www.mrlc.gov/nlcd11_data.php
#           select "NLCD 2011 Land Cover"
#           other files may be of interest for analyses as well
# 
# --------------------------
#
###############################################################################

# --------------------------
# Load Libraries
# --------------------------
library(raster)
library(rgdal)
library(maptools)
library(proj4)
library(sp)
library(maps)
library(rasterVis)
library(rgeos)
library(car)
# --------------------------


# --------------------------
# Read in the sites data table
# --------------------------
sites <- read.csv("DOE_sites_ross_sampled.csv")
sites <- SpatialPointsDataFrame(coords=sites[,c("long", "lat")], sites, proj4string=CRS("+proj=longlat +datum=WGS84"))
summary(sites)
# --------------------------

# --------------------------
# Read, Plot & Extract USGS Ecosystems
# --------------------------
ecosystems <- raster("labeled_ecosystems_30m/final_labeled_prod4_mmu9pxl_30m_dd83_w_subsec.img")
ecosystems
# plot(ecosystems)

# reading in the csv I manually converted from a .dbf file. 
# (R can probably handle .dbf, but I didn't try)
ecosys.meta <- read.csv("labeled_ecosystems_30m/final_labeled_prod4_mmu9pxl_30m_dd83_w_subsec.img.vat.csv")
summary(ecosys.meta)

# Subsetting just the columns we're interested in 
ecosys.meta2 <- ecosys.meta[,c(7,8,10)]
names(ecosys.meta2) <- c("Ecosys.ID", "Ecosys.Name", "Ecosys.General")
summary(ecosys.meta2)

# A quick-and-dirty plotting of the ecosystems -- 
# note: there should be more customizeable ways to do this with GGPLOT, but I haven't tried it yet
pdf("DOE_Ecosystems_USGS.pdf")
plot(ecosystems)
map("state", plot=T, add=T, lty="solid", col="gray30", lwd=1.5)
plot(sites[,], pch=01, add=T, col="indianred1", cex=2, lwd=4,)
plot(sites[sites$site.name=="Valles Caldera Mixed Conifer",], pch=08, add=T, col="firebrick3", cex=3, lwd=6)
dev.off()

# Extracting the USGS Ecosystem identifier & merging in the description
# Note: here I'm just doing a simple extraction here which just takes the value at the actual
#       lat/long provided.  Other options include extracting over a buffer (usually in meters)
#       or getting a catchment shapefile (use readOGR function) and extracting from that
sites$Ecosys.ID <- as.factor(extract(ecosystems, sites))
sites <- merge(data.frame(sites), ecosys.meta2, all.x=T, all.y=F)
summary(sites)
summary(data.frame(sites))
data.frame(sites)
# --------------------------


# --------------------------
# NLDC land use data set
# --------------------------
nldc <- raster("nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img")

# sites <- read.csv("DOE_sites_ross_sampled.csv")
sites <- SpatialPointsDataFrame(coords=sites[,c("long", "lat")], sites, proj4string=CRS("+proj=longlat +datum=WGS84"))
sites <- spTransform(sites, projection(nldc))
summary(sites)

# Making a
usa <- map("state", plot=F, fill=T)
ids <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa.sp <- map2SpatialLines(usa, IDs=usa$names, proj4string=CRS("+proj=longlat +datum=WGS84"))
usa.sp <- spTransform(usa.sp, projection(nldc))

# A quick-and-dirty plotting of the ecosystems -- 
# note: there should be more customizeable ways to do this with GGPLOT, but I haven't tried it yet
pdf("DOE_LandCover_NLDC.pdf")
plot(nldc)
plot(usa.sp, add=T, lwd=1, col="gray30")
plot(sites[,], pch=01, add=T, col="black", cex=2, lwd=3,)
plot(sites[sites$site.name=="Valles Caldera Mixed Conifer",], pch=08, add=T, col="firebrick3", cex=3, lwd=6)
dev.off()


# Extracting the USGS Ecosystem identifier & merging in the description
# Note: here I'm just doing a simple extraction here which just takes the value at the actual
#       lat/long provided.  Other options include extracting over a buffer (usually in meters)
#       or getting a catchment shapefile (use readOGR function) and extracting from that
#
# Note: I'm using the info in "nlcd_2011_landcover_2011_edition_2014_10_10.txt" to do the land
#       cover recoding... there's almost certainly a simpler & more sophisticated way, but this
#       works for now
sites$NLCD.ID <- as.factor(extract(nldc, sites))

unique(sites$NLCD.ID)
sites$NLDC.Name <- recode(sites$NLCD.ID, "'41'='Deciduous Forest'; '42'='Evergreen Forest'; '43'='Mixed Forest'; '90'='Woody Wetlands'")
# sites <- merge(data.frame(sites), ecosys.meta2, all.x=T, all.y=F)
summary(sites)
summary(data.frame(sites))
data.frame(sites)
sites <- sites[,!(names(sites) %in% c("long.1", "lat.1", "long.2", "lat.2", "optional"))]
summary(sites)

write.csv(sites, "DOE_sites_ross_sampled_LandUse.csv", row.names=F)
# --------------------------



