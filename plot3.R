plot3 <- function()
{
  library(png)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  
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
  NEI$type_by_year <- paste(NEI$type,NEI$year)
  NEI <- tapply(NEI$Emissions, NEI$type_by_year, sum)
  NEI <- data.frame("type_year" = names(NEI),"emissions" = NEI, row.names = NULL)
  NEI <- NEI %>% separate(type_year,c("type","year"),sep = " ",remove = T)
  NEI$year <- as.numeric(NEI$year)
  type_order <- c("POINT","NONPOINT","ON-ROAD","NON-ROAD")
  NEI <- NEI %>% mutate(type = factor(type, levels = type_order))
  NEI_blank <- NEI
  NEI_blank$emissions[NEI_blank$type == "POINT"] <- NEI$emissions[NEI$type == "NONPOINT"]
  NEI_blank$emissions[NEI_blank$type == "NONPOINT"] <- NEI$emissions[NEI$type == "POINT"]
  NEI_blank$emissions[NEI_blank$type == "ON-ROAD"] <- NEI$emissions[NEI$type == "NON-ROAD"]
  NEI_blank$emissions[NEI_blank$type == "NON-ROAD"] <- NEI$emissions[NEI$type == "ON-ROAD"]
  
  filepath <- file.path(rootpath, "plot3.png")
  if(exists(filepath)) unlink(filepath)
  
  ggplot(NEI, aes(year, emissions, col = "type")) + 
    geom_point() + 
    geom_line(stat = "smooth", method = "lm", formula = y ~ x, se = F, linetype = "dashed", alpha = 0.5) +
    geom_point(data = NEI_blank, aes(x = year, y = emissions), alpha = 0) +
    geom_path() + 
    facet_wrap(~type, nrow = 2, scales = "free_y") + 
    theme(legend.position = "none", 
          axis.title.x = element_blank(), 
          plot.title = element_text(hjust = 0.5)) +
    labs(title = "Baltimore City PM2.5 total emissions by type", y = "Emissions in tons")
  ggsave(filepath)
  
  img <- readPNG(filepath)
  grid::grid.raster(img)
}