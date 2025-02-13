library(tidyverse)
library(tidylog)
library(sf)
library(raster)
library(gtsummary)

setwd("C:/Users/ledar/Dropbox/Forest Composition/composition/Maps/UrbanDrought")

df <- st_read('NLCDPolyLCV2.shp') |>  
  rename('Canopy' = HISTO_1,
         'Vegetation'= HISTO_2,
         'BareSoil' = HISTO_3,
         'Water' = HISTO_4,
         'Buildings' = HISTO_5,
         'Roads' = HISTO_6,
         'OtherPaved' = HISTO_7) |> 
  mutate(NLCD = as.character(RASTERVALU),
         group = if_else(NLCD %in% c('41', '42', '43'), 'Forest',
                         if_else(NLCD %in% c('51', '52', '53', '71', '72'), 'Grassland',
                                 if_else(NLCD %in% c('81', '82'), 'Crop',
                                         if_else(NLCD == '21', 'UrbanOpen',
                                                 if_else(NLCD == '22', 'UrbanLow',
                                                         if_else(NLCD == '23', 'UrbanMid', 
                                                                 if_else(NLCD == '24', 'UrbanHigh', NA)))))))) |> 
  filter(Shape_Area > 899) |> 
  st_drop_geometry() |> 
  dplyr::select(-c(OBJECTID_1, OBJECTID, OBJECTID_2, Shape_Leng, Shape_Area, 
                   HISTO_NODA, RASTERVALU, RASTERVA_1)) |> 
  mutate(Canopy = Canopy/9687.52,
         Vegetation = Vegetation/9687.52,
         BareSoil = BareSoil/9687.52,
         Water = Water/9687.52,
         Buildings = Buildings/9687.52,
         Roads = Roads/9687.52,
         OtherPaved = OtherPaved/9687.52) 

df3 <- df2 |> 
  select(-NLCD) |> 
  pivot_longer(!group,
               names_to = 'LandCover',
               values_to = 'Percent')

df4 <-  df3[sample(1:nrow(df2), 1000000,
                      replace=FALSE),]

plot <- ggplot(aes(x = group, y = Percent, fill = LandCover), data = df3) +
  geom_boxplot() +
  scale_fill_manual(values = c('#B08239', '#912427', '#598128', '#b2b2b2', '#000002', '#b6d886', '#405f59')) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = 'top',
        legend.title = element_blank())

plot

ggsave(plot = plot, 'plot.png',  width=7.5, height=6, units = "in", dpi = 300)

#Make a table

tabDat <- df |> 
  select(-c(NLCD)) 

style_number_percent <- function(x) paste0(style_number(x*100),'%')

tbl <- tbl_summary(tabDat, by = group,
            statistic = list(
              all_continuous() ~ "{mean} ± {sd}"),
            digits = list(Canopy = style_number_percent,
                          Vegetation = style_number_percent,
                          BareSoil = style_number_percent,
                          Water = style_number_percent,
                          Buildings = style_number_percent,
                          Roads = style_number_percent,
                          OtherPaved = style_number_percent,
                          `NA` = style_number_percent))

tbl

tbl %>%  
  gtsummary::as_tibble() %>% 
  write_csv(., "HighResLCByNLCD.csv")

#NLCD table

dfNLCD <- df |> 
  group_by(group) |> 
  summarize(count = n()) |> 
  mutate(area = count*900)

dfNLCD2 <- dfNLCD |> 
  mutate(percent = area/(sum(dfNLCD$area)))

lc <- list(c())
  
