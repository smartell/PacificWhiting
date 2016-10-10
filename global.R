library(dplyr)
library(rCharts)
library(rMaps)

# load("../../database/AnalysisHaulTable.Rdata")
load("./data/Whiting.rda")
hake.df <- 	Whiting %>% 
						# dplyr::filter(FmpArea == "PNW") %>%
						transform(Longitude = -Longitude) %>%
						dplyr::mutate(latGroup = cut(Latitude,seq(42,49,by=1))) %>%
						dplyr::tbl_df()


bb.lon <-  range(hake.df$Longitude)
bb.lat <-  range(hake.df$Latitude)


.COOPS <- c("PWCC - American","PWCC - Glacier","PWCC - Trident",
            "Whiting Mothership Coop Pool 1",
            "Whiting Mothership Coop Pool 2")