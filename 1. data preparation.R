#Preparation of the dataset for the assessment

########################### PART 1 ##################################

Sys.setlocale("LC_ALL", "en_US.UTF-8")

#----Libraries-----
#install.packages(c("tidyverse","sf","data.table","ggplot2","purrr","extrafont","patchwork"))

library(tidyverse)
library(sf)
library(data.table)
library(ggplot2)
library(purrr)
library(extrafont)
library(patchwork)

source("functions/EQR_functions.R")
source("functions/functions.R")

#if you don't want to include beat:
  BEAT<- "no"
#if you want to include it:
  BEAT<- "yes"

  
################################################################################

#----Utilities-----
  palette <- c("High"="#619cff",
               "Good"="#00ba38",
               "Moderate"="#f0fb61" ,
               "Poor"="#ffc461",
               "Bad"="#f8766d",
               "No Data"="white",
               "Not Assessed"="#EEEEEE")
  
  reg<- c("Baltic Sea", "Black Sea", "Mediterranean Sea","North-east Atlantic Ocean",
          "Greater North Sea, incl. the Kattegat and the English Channel")
  
  correct_order <- c("High", "Good", "Moderate", "Poor", "Bad","No Data")
  
  categories<-c("Direct effects","Indirect effects","Nutrients","Integrated")


  df_factors <- data.frame(HEATClass=factor(correct_order, levels=correct_order))
  df_regions <- data.frame(Region = factor(c(reg,"Europe seas"),
                                           levels = c(reg,"Europe seas")))
  df_factors <- df_factors %>% merge(df_regions, all=T)
  rm(df_regions)

  grid<-st_read("grid/assessment_grid_100_20_minus_land.shp")%>%
    select(GRIDCODE,Region,Subregion=Subregn,Include)%>%
    st_zm()%>%
    mutate(Region=ifelse(grepl("North",Region) & Subregion %in% c("Greater North Sea, incl. the Kattegat and the English Channel"),
                         "Greater North Sea, incl. the Kattegat and the English Channel",Region))
  
  europe_outline <- st_read("grid/Europe_coastline_poly.shp",crs=3035)
  
  
################################################################################
  
#----Data-----
  folder<-"data/EQRs"
  filelist<-list.files(folder)
  filelist  #in the last run we had 21 files
  
  #we are using the hybrid version for OSPAR, which includes OSPAR results for offshore and indicators result for coastal
  #BEAT data is added afterwards
  filelist<-filelist[!grepl("_BEAT",filelist)]
  
  filepaths <- file.path(folder, filelist)
  
  #read in the data
  file_list<-list()
  for (.fp in filepaths){
    df <- read.table(.fp, header = TRUE, sep = ";", stringsAsFactors = FALSE)
    
    if (ncol(df) == 1) {
      df <- read.csv(.fp, header = TRUE, stringsAsFactors = FALSE)
      df$X<-NULL
    }
    file_list[[.fp]]<-df
  }
  
  all_data<- bind_rows(file_list)
  rm(filepaths,filelist,df)
  
  #Check parameters naming
  param_names<-unique(is.na(all_data$Parameter))
  if (length(param_names)>1) {
    print("Some parameters are unnamed")
  }else{
    print("All parameters have names")
  }
  rm(param_names)
  
  all_data<-all_data%>%
    mutate(ER=EQR)%>%
    filter(!is.na(ER))%>%
    mutate(Period=as.character(Period))%>% #small fixes
    select(-EQR)
  
#add BEAT data
  if (BEAT=="yes"){
    #join BEAT data
    beat_coastal<-read.table(paste0(folder,"/EQR_coastal_BEAT.csv"),sep=";",header=T)%>%
      rename("Parameter"="parameter")%>%
      filter(!(Parameter %in% c("Phytobenthos")))%>%
      mutate(Level=1,
             ER=EQR,
             Period="2022",
             QEtype= "Bio",
             HEATCat= "Sec",
             Category="Indirect effects",
             DESCRIPTOR=case_when(
               Parameter=="Angiosperms"~ "QE1-2-2",
               Parameter=="Benthic invertebrates"~ "QE1-3",
               Parameter=="Macroalge"~ "QE1-2-1",
               Parameter=="Macrophytes"~ "QE1-2-3",
               Parameter=="OA flora"~ "QE1-2",
               TRUE ~ NA_character_
             ),
             DESCRIPTOR_NAME= Parameter
      )%>%
      filter(!is.na(ER))%>%
      select(-EQR)
    
    all_data<-bind_rows(all_data,beat_coastal)
    rm(beat_coastal)
  }else{
    all_data<-all_data
  }
  
  
#harmonize categories
  all_data<-all_data%>%
    mutate(HEATCat= case_when(
      HEATCat %in% c("Sec","secondary","Secondary") ~ "Sec",
      HEATCat %in% c("Pri","primary","Primary") ~ "Pri",
      HEATCat %in% c("Nut","nutrients","Nutrients") ~"Nut",
      TRUE ~ NA_character_
    ))
  
  
#check repeated gridcell data from different levels
  check<-all_data%>%select(GRIDCODE,Level)%>%
    distinct()%>% 
    group_by(GRIDCODE)%>%
    mutate(n= ifelse(n()>1,as.character(n()),"not repeated"))%>%
    filter(!(n=="not repeated"))
  
  check_grid<-unique(check$GRIDCODE)
  
  double<-all_data%>%
    filter(GRIDCODE %in% check_grid)%>%
    arrange(GRIDCODE,Level)%>%
    group_by(GRIDCODE)%>%
    filter(Level== min(Level))
  
  all_data<-all_data%>%
    filter(!(GRIDCODE %in% check_grid))%>%
    bind_rows(double)
  rm(double,check,check_grid,file_list)
  
#check parameter nomenclature
  all_data<-all_data%>%
    mutate(Parameter=case_when(
      Parameter %in% c("Ammonium-nitrogen (µg N/l), sommer") ~"NH4, summer",
      Parameter %in% c("Ammonium-nitrogen (µg N/l), vinter") ~"NH4, winter",
      Parameter %in% c("Ammonium","NH4") ~"NH4",
      Parameter %in% c("Benthic invertebrates","Softbottom fauna") ~"Macroinvertebrates",
      Parameter %in% c("Chlorophyll","summer klorofyll a","Chla a","CHL-A","Chlorophyll a (in-situ & satellite)") ~"Chlorophyll",
      Parameter %in% c("Dissolved Inorganic Nitrogen","DIN") ~"DIN",
      Parameter %in% c("Dissolved Inorganic Phosphorus","DIP","Phosphate","PO4") ~"DIP",
      Parameter %in% c("Dissolved oxygen","DO","Oxygen","Oksygen (ml O2/l)") ~"Oxygen",
      Parameter %in% c("Fosfat-fosfor (µg P/l), sommer") ~"PO4, summer",
      Parameter %in% c("Fosfat-fosfor (µg P/l), vinter") ~"PO4, winter",
      Parameter %in% c("MSMDI (Nedre voksegrense) - EQR") ~"MSMDI",
      Parameter %in% c("Nitrate","NO3") ~"NO3",
      Parameter %in% c("Nitrat-nitrogen (µg N/l), sommer") ~"NOx, summer",
      Parameter %in% c("Nitrat-nitrogen (µg N/l), vinter") ~"NOx, winter",
      Parameter %in% c("Nitrite","NO2") ~"NO2",
      Parameter %in% c("Siktdyp (m), sommer","SDD","Secchi Depth","secchi") ~"Secchi depth",
      Parameter %in% c("Total fosfor (µg P/l), sommer","TotalPhosphorus(Summer)") ~"TP, summer",
      Parameter %in% c("Total fosfor (µg P/l), vinter","TotalPhosphorus(Winter)") ~"TP, winter",
      Parameter %in% c("TotalNitrogen","Total Nitrogen") ~"TN",
      Parameter %in% c("Total nitrogen (µg N/l), sommer","TotalNitrogen(Summer)") ~"TN, summer",
      Parameter %in% c("Total nitrogen (µg N/l), vinter","TotalNitrogen(Winter)") ~"TN, winter",
      Parameter %in% c("TotalPhosphorus","Total Phosphorus","TP") ~"TP",
      Parameter=="not available" ~ NA,
      TRUE ~ Parameter
    ))
  

  param_order<-c("Chlorophyll","MSMDI","Secchi depth","Macroinvertebrates","Macroalge","Macrophytes","OA flora","Angiosperms","Phytobenthos","DIN","NO2","NO3",
                 "NOx, summer","NOx, winter","NH4","NH4, summer","NH4, winter","TN","TN, summer","TN, winter",
                 "DIP","PO4, summer","PO4, winter","TP","TP, summer","TP, winter",
                 "Oxygen","Oksygen metning (%)","Oxygen debt","Oxygen Deficiency")
  
  all_data<-all_data%>% #DATA used for the assessment
    mutate(Parameter=factor(Parameter,levels= param_order))
  
  saveRDS(all_data,"data/assessment_data.rds")

################################################################################

#----Parameter visualization-----
  
  bg <- data.frame(x=c(560000, 560000, 6960000, 6960000, 560000),
                   y=c(774000, 6850000, 6850000, 774000, 774000)) %>%
    mutate(id=1) %>%
    sf::st_as_sf(coords=c("x","y"), crs=sf::st_crs(grid)) %>%
    group_by(id) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  data_1<-all_data%>%filter(Level==1)
  
  dfHEATmean <- aggregateHEAT(data_1, var_cat="HEATCat",group_vars=c("GRIDCODE","Parameter"))
  
  data_1<-left_join(grid,dfHEATmean, by="GRIDCODE") %>%filter(!is.na(HEATClass)) %>%
    mutate(HEATClass=ifelse(HEATClass=="Mod","Moderate",HEATClass))
  
  p<-ggplot() +
    geom_sf(data=bg, fill="#DDEEEE") +
    geom_sf(data=europe_outline,fill="white",color=NA) +
    geom_sf(data=data_1, aes(fill = HEATClass), colour = alpha("grey", 0.3),show.legend=F) +
    scale_fill_manual(values = palette, breaks = correct_order,limits=correct_order,na.value = "white",drop=F)  +
    facet_wrap(~Parameter,ncol=6)+
    labs(fill = "Status")+
    theme_minimal()+
    theme(
      text = element_text(size=20,family="Calibri"),
      plot.title=element_text(hjust=0.5),
      axis.text = element_blank(),
      plot.background = element_rect(fill="white",color="white"),
      panel.grid = element_blank(),
      panel.ontop = T,
      panel.background= element_rect(fill=NA,color="black")
    )+
    coord_sf(xlim = c(570000, 6950000), ylim = c(773000, 6840000),expand = FALSE)
  ggsave("visualization/HEAT parameters.png",p,height = 14,width=17)
  rm(dfHEATmean,data_1,p)