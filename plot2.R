plot2 <- function()
{
  library(png)
  
  URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  rootpath <- file.path(".","Plots")
  filename <- "exdata_data_NEI_data.zip"
  
  if(!dir.exists(rootpath)) dir.create(rootpath)
  
  filepath <- file.path(rootpath,"summarySCC_PM25.rds")
  
  if(!file.exists(filepath))
  {
    filepath <- file.path(rootpath, filename)
    download.file(URL, filepath, 'curl')
    unzip(filepath, exdir = rootpath)
    unlink(filepath)
  }
  
  filepath <- file.path(rootpath, "summarySCC_PM25.rds")
  NEI <- readRDS(filepath)
  NEI <- subset(NEI, fips == "24510")
  total_emissions <- tapply(NEI$Emissions, NEI$year, sum) / 1000
  
  filepath <- file.path(rootpath, "plot2.png")
  if(exists(filepath)) unlink(filepath)
  png(filepath)
  
  bluescale <- 255 - total_emissions / max(total_emissions) * 255
  bluescale2 <- 255 - total_emissions / max(total_emissions) * 192
  bluescale <- as.character(as.hexmode(as.integer(bluescale)))
  bluescale2 <- as.character(as.hexmode(as.integer(bluescale2)))
  bluescale <- paste0("#", bluescale, bluescale2, "ff")
  
  barplot(total_emissions, 
          col = bluescale,
          main = "Total PM2.5 emissions from Baltimore City",
          ylab = "Thousands of tons")
  dev.off()
  img <- readPNG(filepath)
  grid::grid.raster(img)
}