plot1 <- function()
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
  total_emissions <- tapply(NEI$Emissions, NEI$year, sum) / 1000000
  
  filepath <- file.path(rootpath, "plot1.png")
  if(exists(filepath)) unlink(filepath)
  png(filepath)
  
  greyscale <- 255 - total_emissions / max(total_emissions) * 255
  greyscale <- as.character(as.hexmode(as.integer(greyscale)))
  greyscale <- paste0("#", greyscale, greyscale, greyscale)
  
  barplot(total_emissions, 
          col = greyscale,
          main = "Total PM2.5 emissions from all sources",
          ylab = "Millions of tons")
  dev.off()
  img <- readPNG(filepath)
  grid::grid.raster(img)
}