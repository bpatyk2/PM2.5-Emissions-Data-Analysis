plot3rev <- function()
{
  library(png)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(ggpubr)
  
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
  NEI_top <- subset(NEI, type == "POINT" | type == "NONPOINT")
  NEI_bottom <- subset(NEI, type == "ON-ROAD" | type == "NON-ROAD")
  type_order_top <- c("POINT","NONPOINT")
  type_order_bottom <- c("ON-ROAD","NON-ROAD")
  NEI_top <- NEI_top %>% mutate(type = factor(type, levels = type_order_top))
  NEI_bottom <- NEI_bottom %>% mutate(type = factor(type, levels = type_order_bottom))
  
  filepath <- file.path(rootpath, "plot3.png")
  if(exists(filepath)) unlink(filepath)
  
  ap <- ggplot(NEI_top, aes(year, emissions)) + 
    geom_point(shape = 19, col = "red") + 
    geom_line(stat = "smooth", 
              method = "lm", 
              formula = y ~ x, 
              se = F, 
              linetype = "dashed",
              col = "red",
              alpha = 0.5) +
    facet_wrap(~type) + 
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = "Baltimore City PM2.5 total emissions by type", y = "Emissions in tons")
  
  bp <- ggplot(NEI_bottom, aes(year, emissions)) + 
    geom_point(shape = 19, col = "blue") + 
    geom_line(stat = "smooth", 
              method = "lm", 
              formula = y ~ x, 
              se = F, 
              linetype = "dashed",
              col = "blue",
              alpha = 0.5) +
    facet_wrap(~type) + 
    theme(legend.position = "none", 
          axis.title.x = element_blank()) +
    labs(y = "Emissions in tons")
  
  ggarrange(ap, bp, nrow = 2)
  
  ggsave(filepath)
  
  img <- readPNG(filepath)
  grid::grid.raster(img)
}