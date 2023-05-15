
# ______________________________________________________________________________

# Script used to produce maps and piecharts of ReefSYN policy brief


# load packages
source ("R/packages.R")


# ====================== 

# magris data (reef area)

shapefiles<-list.files(here ("data","magris_reef_map"),pattern=".shp")

# rm cumulative impacts
shapefiles<-shapefiles[-grep("pu", tolower (shapefiles))]

# load all at once
shapes <- lapply (shapefiles, function (shp) 
  
  readOGR (here ("data","magris_reef_map"),
           gsub(".shp","",shp)) 
)



# subset of habitats
list_habitats <- list ("amazon" = "AO11",# amazon
                       "eastern"= c("EC11",# eastern
                                    "EC12",
                                    "EC13",
                                    "EC14",
                                    "EC15",
                                    "EC16"),
                       "noronha" = c("FS12", "FS13"),#noronha
                       "northeastern" = c("NC11",# northeastern
                                          "NC12",
                                          "NC13"),
                       "riogrande"="RC11", # rio grande
                       "southeastern"="SC11", #southeastern
                       "trindade" = "TS12"
)

# reef location
BR_reefs <- lapply (seq (1,length(shapes)), function (shp)
  # extract codes  
  shapes[[shp]][which(shapes[[shp]]$habitat %in% list_habitats[[shp]]),]
  
)


# bind 
BR_reefs <- bind(BR_reefs[[1]],
                 BR_reefs[[2]],
                 BR_reefs[[3]],
                 BR_reefs[[4]],
                 BR_reefs[[5]],
                 BR_reefs[[6]],
                 BR_reefs[[7]])


# penedos

spsp<-readOGR (here ("data","magris_reef_map","SPSP"),"PS11_2") 


# shape of SA
South_America <-  readOGR (here ("data","South_America","South_America.shp")) 
Brazil_latlong <- South_America[which(South_America$COUNTRY == "Brazil"),]

# save pdf 
pdf(file= here ("output", "BR_map.pdf"))
plot(Brazil_latlong)
dev.off()

# lambert projection
Brazil <- spTransform(Brazil_latlong, 
                            CRS("+proj=laea +lat_0=0 +lon_0=0"))



# ======================================================
#  mpa & EEZ data

mpas <- readOGR (here ("data","MPAs"),
                 layer = "cnuc_2021_02")
mpas_lambert <- spTransform(mpas, 
                            CRS("+proj=laea +lat_0=0 +lon_0=0"))

# remove non-marine PAs
globeEEZ <- readOGR(here("data",
                         "Intersect_EEZ_IHO_v4_2020",
                         "Intersect_EEZ_IHO_v4_2020"), 
                    layer = "Intersect_EEZ_IHO_v4_2020")

# BR eez
br_eez <-globeEEZ[grep("Brazilian",globeEEZ$EEZ),] 
br_eez <- gUnaryUnion(br_eez)

pdf (here ("output", "br_eez.pdf"))
plot(br_eez)
dev.off()

# project
br_eez_lambert <- spTransform(br_eez, 
                              CRS("+proj=laea +lat_0=0 +lon_0=0"))

# over mpas
over_mpas <- over (mpas_lambert, br_eez_lambert)

# subset of marine PAs
marine_pas_lambert<- mpas_lambert [which(is.na(over_mpas)!= T),]
marine_pas<- mpas [which(is.na(over_mpas)!= T),]

# equal crs
crs(br_eez) <-  crs (mpas)

# here we filter with 0.8 (or 80%) threshold
sf_use_s2(FALSE)

# intersection
intersection_ZEE_mpas<- st_intersection(st_as_sf(br_eez), st_as_sf(marine_pas)) 

# filter by area
intersection_ZEE_mpas <- intersection_ZEE_mpas %>% 
  filter(st_area(.) >= 0.005*st_area(st_as_sf(marine_pas)))
intersection_ZEE_mpas <- as_Spatial(intersection_ZEE_mpas)

# group
f.mpa<-fortify(intersection_ZEE_mpas, region="fid")
f.mpa2<- cbind (f.mpa, 
                colour_mpa = intersection_ZEE_mpas@data [match (f.mpa$id,
                                                           intersection_ZEE_mpas$fid),]$grupo)
# plot 
plot5 <- ggplot() +
  
  theme_classic()+
  
  geom_polygon (data = BR_reefs, aes(long, lat,group = group), 
              fill= "#bf3720",alpha=1,colour="#bf3720",size=1.25)  +
  
  geom_polygon (data = spsp, aes(long, lat,group = group), 
                fill= "#bf3720",alpha=1,colour="#bf3720",size=1.25)   +


  geom_polygon (data = f.mpa2 , aes(x=long, 
                                    y=lat,
                                    group = group,
                                    fill=colour_mpa),
              
              alpha=0.5,
              size=0)   +
  scale_fill_manual("MPA",
                    values = c("PI" = "#ffc222",
                               "US" = "#ffd870"))+
  
  geom_polygon (data = br_eez, aes(long, lat,group = group), 
                fill="gray",alpha=0.01,colour='gray70',linetype=2) + 
  
  geom_polygon (data = Brazil_latlong, aes(long, lat,group = group), 
                fill="#7da8ba",alpha=0.5,colour=NA) + 
  
   coord_sf(xlim = c(-52, -20), ylim = c(-32, 10), expand = F) + 
  theme(legend.position = c(0.65,0.1),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        #axis.text = element_blank(),
        legend.text = element_text(size=8))+
  xlab("Longitude") +
  ylab ("Latitude")

# save
pdf (file = here ("output", "maps_with_reefs.pdf"))
plot5
dev.off()



#  ================================


# biodviersity data
fish_data <- read.csv(here ("data", 
                            "Pinheiro_et_al_2018",
                            "brazilian-reef-fish-table-04-mar-18-website.xlsx - Database.csv"),
                      sep = ",")

#fish_data
fish_data_df <- (melt (fish_data, id.vars = c("Relation" ,"X10.familias" ,
                                               "X40.familias","Type",
                                               "Order","Family","Genus",
                                               "Species","authority",
                                               "Popular_name_PT","English_name")))
fish_data_df$variable <- as.character(fish_data_df$variable)

# interesting variables
variables <-c("BR","RR", "RB","TP","SG","SB", "EM", "OC",
              "Brazilian.Province",
              "Argentine",                 
              "North.Islands","Central.Islands","PML.PAMA","Northeast.with.PAMA.PML",   
              "Northeast","Southeast","Noronha.rocas","Atol",                      
              "FN","SpSp","PML","PA.MA","CE","RN","PE","PB",
              "AL","CVT","Tri","BA","Abr","ES","ArC","IGr","RJ","SP","PR","SC",                        
              "URU","ARG","BR.Endêmico","SWA.Endemic","Expatriated","Exotic") 

# filtering  
fish_data_df<-fish_data_df[which(fish_data_df$variable %in% variables),]

# remove colums we don't want
fish_data_df <- fish_data_df[,-which(colnames(fish_data_df) %in% c("authority","Popular_name_PT","English_name"))]

# find BR endemics
BR_endemics <- fish_data_df[which(fish_data_df$variable == "BR.Endêmico" & 
                                    fish_data_df$value == "1" ), "Species"]


# statistics for a few sites
sites<-c("PML.PAMA","Atol","FN","RN","Abr", "Tri","ArC",  
         "SC")


# fish per site
sites_fish <- lapply (sites , function (i){

  # subset based on site & incidence (value)
  sites_fish <- fish_data_df[which(fish_data_df$variable == i &
                                      fish_data_df$value == "1"),]

  # all species, non-endemics and endemics
  df_pie <- data.frame (site = i ,
                        total.SR = sum(table(sites_fish$Species %in% BR_endemics)),
                        endemics.SR = table(sites_fish$Species %in% BR_endemics)[2]#,
                        #non.endemics.SR = table(sites_fish$Species %in% BR_endemics)[1]
                        )
  
  # discounting 
  # df_pie$total.SR <- df_pie$total.SR - df_pie$endemics.SR
  # melting
  df_pie<-melt(df_pie)
  df_pie$pie_size <- sum(table(sites_fish$Species %in% BR_endemics))
  ; # return
  
  df_pie
  })


# pie chart applied to each site

pie_charts_fish <- lapply (sites_fish, function (i) 
  
  
    ggplot(i, aes(x = "",
                  y = value, 
                  fill = variable)) +
      #facet_wrap(~site)+
      geom_col() +
      #geom_bar(position="fill", stat="identity")+
      coord_polar(theta = "y") + 
      geom_text(aes(label = value),
                position = position_stack(vjust = 0.5),
                size=5)+
      theme_classic() + 
      theme (legend.position = "none",
             axis.text = element_blank(),
             axis.title = element_blank(),
             axis.line = element_blank(),
             axis.ticks = element_blank(),
             panel.border = element_rect(colour = "black", fill=NA, 
                                         size=2)) + 
      scale_fill_manual (values = c ("#3d5a80", "#7da8ba"))
)


df_SR_fish <- do.call(rbind ,sites_fish) # 

df_SR_fish$site <- factor(df_SR_fish$site,
                          levels = sites)


# produce charts
fish_charts <- df_SR_fish%>%

  ggplot(aes(x=0, y = value, 
             fill = variable, 
             width = pie_size/max(pie_size))) +
  
        geom_bar(position="fill", stat="identity") + 
        coord_polar("y") +
  facet_wrap(~ site,ncol=2) + 
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=0,y = value/max(value), label=value))+
  theme_classic() +
  theme(legend.position = "top")+ 
  scale_fill_manual (values = c ("#3d5a80", "#7da8ba"))



# -------------------------------


## benthic event core data
aued_benthos_event_core <- read.csv ("../../../ReefSYN_data/DwC_output/AAued_spatialData/event_core.csv")
      
## benthic occurrence data
aued_benthos_emof <- read.csv ("../../../ReefSYN_data/DwC_output/AAued_spatialData/DF_eMOF.csv")

## benthic occurrence data
aued_benthos_occ <- read.csv ("../../../ReefSYN_data/DwC_output/AAued_spatialData/DF_occ.csv")

# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("site","locality","year")
benthos_SN_data_aued <- aued_benthos_event_core [match (aued_benthos_emof$eventID,
                                                        aued_benthos_event_core$eventID),
                                                 variables_we_want]
# bind the occurrence data
benthos_SN_data_aued<- cbind (benthos_SN_data_aued,
                              aued_benthos_emof)

# bind taxa id
benthos_SN_data_aued<- cbind (benthos_SN_data_aued,
                                  spp = aued_benthos_occ$scientificNameAccepted [match (benthos_SN_data_aued$occurrenceID,
                                                                                        aued_benthos_occ$occurrenceID)])

# -----------------------------------------------------
# francini
## benthic event core data
francini_benthos_event_core <- read.csv ("../../../ReefSYN_data/DwC_output/RFrancini_spatialData/event_core.csv")

## benthic occurrence data
francini_benthos_emof <- read.csv ("../../../ReefSYN_data/DwC_output/RFrancini_spatialData/DF_eMOF.csv")

## benthic occurrence data
francini_benthos_occ <- read.csv ("../../../ReefSYN_data/DwC_output/RFrancini_spatialData/DF_occ.csv")

# matching event IDs to find site and locality (variables_we_want)
benthos_SN_data_francini <-francini_benthos_event_core [match (francini_benthos_emof$eventID,
                                                               francini_benthos_event_core$eventID),
                                                 variables_we_want]
# bind the occurrence data
benthos_SN_data_francini<- cbind (benthos_SN_data_francini,
                                  francini_benthos_emof)


# bind taxa id
benthos_SN_data_francini<- cbind (benthos_SN_data_francini,
                                  spp = francini_benthos_occ$scientificNameAccepted [match (benthos_SN_data_francini$occurrenceID,
                                                                                            francini_benthos_occ$occurrenceID)])


# ---------------------------------
# rio grande do norte (ross et al. )

## benthic event core data
ross_benthos_event_core <- read.csv ("../../../ReefSYN_data/DwC_output/GLongo_NRoss_spatialData/event_core_benthos.csv")

## benthic occurrence data
ross_benthos_emof <- read.csv ("../../../ReefSYN_data/DwC_output/GLongo_NRoss_spatialData/DF_eMOF_benthos.csv")

## benthic occurrence data
ross_benthos_occ <- read.csv ("../../../ReefSYN_data/DwC_output/GLongo_NRoss_spatialData/DF_occ_benthos.csv")

# matching event IDs to find site and locality (variables_we_want)
benthos_SN_data_ross <-ross_benthos_event_core [match (ross_benthos_emof$eventID,
                                                       ross_benthos_event_core$eventID),
                                                  variables_we_want]
# bind the occurrence data
benthos_SN_data_ross<- cbind (benthos_SN_data_ross,
                                  ross_benthos_emof)

# bind taxa id
benthos_SN_data_ross<- cbind (benthos_SN_data_ross,
                               spp = ross_benthos_occ$scientificNameAccepted [match (benthos_SN_data_ross$occurrenceID,
                                                                                     ross_benthos_occ$occurrenceID)])

# all rio grande do norte
benthos_SN_data_ross$site <- "rgnor"


# -------------------------------
# abrolhos bank

## benthic event core data
abrolhos_benthos_event_core <- read.csv ("../../../ReefSYN_data/DwC_output/RFrancini_timeSeries_abrolhos/event_core_benthos.csv")

## benthic occurrence data
abrolhos_benthos_emof <- read.csv ("../../../ReefSYN_data/DwC_output/RFrancini_timeSeries_abrolhos/DF_eMOF_benthos.csv")

## benthic occurrence data
abrolhos_benthos_occ <- read.csv ("../../../ReefSYN_data/DwC_output/RFrancini_timeSeries_abrolhos/DF_occ_benthos.csv")

# matching event IDs to find site and locality (variables_we_want)
benthos_SN_data_abrolhos <- abrolhos_benthos_event_core [match (abrolhos_benthos_emof$eventID,
                                                            abrolhos_benthos_event_core$eventID),
                                                 variables_we_want]
# bind the occurrence data
benthos_SN_data_abrolhos<- cbind (benthos_SN_data_abrolhos,
                                  abrolhos_benthos_emof)

# bind taxa id
benthos_SN_data_abrolhos<- cbind (benthos_SN_data_abrolhos,
                              spp = abrolhos_benthos_occ$scientificNameAccepted [match (benthos_SN_data_abrolhos$occurrenceID,
                                                                                        abrolhos_benthos_occ$occurrenceID)])

# -------------------------------
# PELD ILOC bank

## benthic event core data
PELD_benthos_event_core <- read.csv ("../../../ReefSYN_data/DwC_output/PELD_iloc_benthos/event_core.csv")
colnames(PELD_benthos_event_core)[which(colnames(PELD_benthos_event_core) == "island")] <- "site"

## benthic occurrence data
PELD_benthos_emof <- read.csv ("../../../ReefSYN_data/DwC_output/PELD_iloc_benthos/DF_eMOF.csv")

## benthic occurrence data
PELD_benthos_occ <- read.csv ("../../../ReefSYN_data/DwC_output/PELD_iloc_benthos/DF_occ.csv")

# matching event IDs to find site and locality (variables_we_want)
benthos_SN_data_PELD <- PELD_benthos_event_core [match (PELD_benthos_emof$eventID,
                                                         PELD_benthos_event_core$eventID),
                                                     variables_we_want]
# bind the occurrence data
benthos_SN_data_PELD <- cbind (benthos_SN_data_PELD,
                               PELD_benthos_emof)

# bind taxa id
benthos_SN_data_PELD <- cbind (benthos_SN_data_PELD,
                                  spp = PELD_benthos_occ$scientificNameAccepted [match (benthos_SN_data_PELD$occurrenceID,
                                                                                        PELD_benthos_occ$occurrenceID)])

# removing and altering colnames to allow matching across datasets
benthos_SN_data_PELD<-benthos_SN_data_PELD[,-which(colnames(benthos_SN_data_PELD) == "analyzedBy")]
colnames(benthos_SN_data_PELD) [which(colnames(benthos_SN_data_PELD) == "measurementeUnity")] <- "measurementUnit"


# ----------------------------------------------


# bind benthic data of these five datasets
compiled_benthic_data<- rbind (benthos_SN_data_aued,
                                benthos_SN_data_ross,
                                benthos_SN_data_francini,
                               benthos_SN_data_abrolhos,
                               benthos_SN_data_PELD)


#intersect_dataB <- (compiled_benthic_data[which(compiled_benthic_data$spp == "Montastraea cavernosa" ),])
#intersect_dataB [which(intersect_dataB$measurementValue > 0 & intersect_dataB$site == "ilhasc_sul"),]


## subset of sites we want
sites_benthos <- c ("manuel_luis", "rocas","noronha", "rgnor","abrolhos","trindade","arraial",#"rio_de_janeiro",
                    "ilhasc_sul")


# subset
compiled_benthic_data<- compiled_benthic_data[which(compiled_benthic_data$site %in% sites_benthos),]



# remove corals (we will do one for them)
corals <- c("Siderastrea", "Millepora","Favia","Agaricia",
            "Porites","Montastraea","Madracis","Mussismilia",
            "Scolymia","Meandrina", "Stephanocoenia")



# endemic corals
endemic_corals <- c("Siderastrea stellata", 
                    "Favia gravida",
                    "Mussismilia braziliensis",
                    "Mussismilia harttii",
                    "Mussismilia hispida",
                    "Mussismilia leptophylla",
                    "Millepora laboreli",
                    "Millepora nitida",
                    "Meandrina brasiliensis")


# find the lines with corals
records_of_corals <- lapply (corals, function (i) 
  
  grep (i,compiled_benthic_data$spp)
  
  )


# melt
records_of_corals <- unlist(records_of_corals)



# complete spp composition with all species found across datasets
# all spp
complete_benthic_composition <- (cast(compiled_benthic_data, formula = site~spp,
                             value = "measurementValue",
                             fun = mean,
                             na.rm=T,fill=0))

# df for pie chart
pie_charts_benthos_complete <- data.frame (site = complete_benthic_composition[,which(colnames(complete_benthic_composition) == "site")],
                                  SR_benthos=rowSums(complete_benthic_composition[,-which(colnames(complete_benthic_composition) %in% c("site", "NA"))]>0,na.rm=T))

# match order 
pie_charts_benthos_complete<-pie_charts_benthos_complete[match (sites_benthos,
                                                                pie_charts_benthos_complete$site),]



# --------------------------

# non corals
compiled_benthic_data_non_corals <- compiled_benthic_data [-records_of_corals,]
 


#  corals
compiled_benthic_data_corals <- compiled_benthic_data [records_of_corals,]



# benthos except corals per location


benthic_composition <- (cast(compiled_benthic_data_non_corals, formula = site~spp,
                               value = "measurementValue",
                               fun = mean,
                             na.rm=T,fill=0))





# df for pie chart
pie_charts_benthos <- data.frame (site = benthic_composition[,which(colnames(benthic_composition) %in% c("site"))],
                                  SR_benthos=rowSums(benthic_composition[,-which(colnames(benthic_composition) %in% c("site", "NA"))]>0,na.rm=T))



# match order 
pie_charts_benthos<-pie_charts_benthos[match (sites_benthos,
                                              pie_charts_benthos$site),]




# corals

coral_composition <- (cast(compiled_benthic_data_corals, formula = site~spp,
                           value = "measurementValue",
                           fun = mean,
                           na.rm=T))

# df for pie chart
pie_charts_corals <- data.frame (site = coral_composition[,which(colnames(coral_composition) %in% c("site"))],
                                  SR_corals=rowSums(coral_composition[,-which(colnames(coral_composition) %in% c("site", "NA"))]>0,na.rm=T))

# match order 
pie_charts_corals<-pie_charts_corals[match (sites_benthos,
                                            pie_charts_corals$site),]



# -----------------------------
# endemics


coral_endemics <- coral_composition[,c(1,which(colnames(coral_composition) %in% endemic_corals))]


# df for pie chart
pie_charts_coral_endemics <- data.frame (site = coral_endemics[,1],
                                  SR_coral_endemics=rowSums(coral_endemics[,-1]>0,na.rm=T))

# match order 
pie_charts_coral_endemics<-pie_charts_coral_endemics[match (sites_benthos,
                                                            pie_charts_coral_endemics$site),]

## cbind
pie_charts_benthos <- cbind (pie_charts_benthos,
                             SR_corals=pie_charts_corals$SR_corals,
                             SR_corals_endemics = pie_charts_coral_endemics$SR_coral_endemics,
                             SR_total = pie_charts_benthos_complete$SR_benthos)

# discounting endemics and corals
pie_charts_benthos$SR_benthos <- pie_charts_benthos$SR_total - (pie_charts_benthos$SR_corals - pie_charts_benthos$SR_corals_endemics)

# melt
pie_charts_benthos<- melt(pie_charts_benthos,id.vars = c("site", "SR_total"))

# draw pie chart
# one chart per site
pie_benthos <- lapply (sites_benthos, function (i)
      
    ggplot(pie_charts_benthos[which(pie_charts_benthos$site == i),], aes(x = "", y = value, fill = variable)) +
        #facet_wrap(~site)+
        geom_col() +
        coord_polar(theta = "y") + 
        geom_text(aes(label = value),
                  position = position_stack(vjust = 0.5),
                  size=5)+
        theme_classic() + 
        theme (legend.position = "none",
               axis.text = element_blank(),
               axis.title = element_blank(),
               axis.line = element_blank(),
               axis.ticks = element_blank(),
               panel.border = element_rect(colour = "black", fill=NA, 
                                           size=2)) +
      
      scale_fill_manual (values = c ("#ee6c4d", "#df3720","#7e2110"))
      
)



# save fig 1
pdf (here ("output", "fig1_raw_opt1.pdf"),width = 8,height=7)

## arrange
grid.arrange(# map
             plot5,
             # fish
             pie_charts_fish[[1]],
             pie_charts_fish[[2]],
             pie_charts_fish[[3]],
             pie_charts_fish[[4]],
             pie_charts_fish[[5]],
             pie_charts_fish[[6]],
             pie_charts_fish[[7]], 
             pie_charts_fish[[8]], 
             #pie_charts_fish[[9]], 
             # benthos
             pie_benthos[[1]],
             pie_benthos[[2]],
             pie_benthos[[3]],
             pie_benthos[[4]],
             pie_benthos[[5]],
             pie_benthos[[6]],
             pie_benthos[[7]], 
             pie_benthos[[8]],
             # pie_benthos[[9]], 
             #top=textGrob("Contribution of reefal tourism do GDP"),
             nrow=9,ncol =5,
             layout_matrix = rbind (c(NA,NA,11,NA,NA), 
                                    c(NA,1,2,3,12),
                                     c(1,1,1,4,13),
                                     c(1,1,1,5,14),
                                     c(1,1,1,6,15),
                                     c(1,1,1,7,16),
                                     c(1,1,1,8,17),
                                    c(1,1,1,9,18),
                                    c(1,1,1,10,19))
)


dev.off()


# other option
pie_charts_benthos$site <- factor(pie_charts_benthos$site,
                          levels = sites_benthos)

# benthos piechart
# size == richness
benthic_charts <- pie_charts_benthos %>%
  
  ggplot(aes(x=0, y = value, 
             fill = variable, 
             width = SR_total/max(SR_total))) +
  
  geom_bar(position="fill", stat="identity") + 
  coord_polar("y") +
  facet_wrap(~ site,ncol=2) + 
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=0,y = value/max(value), label=value))+
  theme_classic() +
  theme(legend.position = "top") +
  
  scale_fill_manual (values = c ("#ee6c4d", "#df3720","#7e2110"))

benthic_charts
ggsave (file = here ("output", "pizza_benthos.pdf"))


# benthos piechart
# equal size
benthic_charts_equal_size <- pie_charts_benthos %>%
  
  ggplot(aes(x=0, y = value, 
             fill = variable, 
             width = 1)) +
  
  geom_bar(position="fill", stat="identity") + 
  coord_polar("y") +
  facet_wrap(~ site,ncol=2) + 
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=0,y = value/max(value), label=value))+
  theme_classic() +
  theme(legend.position = "top") +
  
  scale_fill_manual (values = c ("#ee6c4d", "#df3720","#7e2110"))

benthic_charts_equal_size
ggsave (file = here ("output", "pizza_benthos_equal_size.pdf"))


# plot composition
pdf (here ("output", "fig1_raw_opt2.pdf"),width = 10,height=7)

## arrange
grid.arrange(# map
  plot5,
  # fish
  fish_charts,
  # benthos
  benthic_charts, 
  
  #top=textGrob("Contribution of reefal tourism do GDP"),
  nrow=7,ncol =7,
  layout_matrix = rbind (c(1,1,1,2,2,3,3), 
                         c(1,1,1,2,2,3,3),
                         c(1,1,1,2,2,3,3),
                         c(1,1,1,2,2,3,3),
                         c(1,1,1,2,2,3,3),
                         c(1,1,1,2,2,3,3),
                         c(1,1,1,2,2,3,3))
)

dev.off()


# plot
pdf(file= here ("output", "BR_EEZ.pdf"))
plot(br_eez,border = "gray")
plot(Brazil_latlong,add=T,col="gray")
plot(BR_reefs,add=T,col="orange")
dev.off()


# ========================================================
# MPA statistics
# data From RQMA

data.frame (esfera = c("estadual","federal", "municipal"),
            Freq = c(84,70,36)) %>% 
 ggplot(aes(x=reorder (esfera,Freq), y = Freq, 
            fill = esfera)) +
   
   geom_bar(stat="identity",position="stack",width=0.5) + 
   coord_polar("y") +
   scale_y_continuous(expand = c(0,20))+
   geom_text(aes(x=0,y = Freq, label=Freq))+
   theme_void()+
   theme(legend.position = "top")

ggsave (file = here ("output", "MPA_category.pdf"))

# % area
melt(data.frame (totZEE = 1,
            MPA=0.2649)) %>% #gArea (marine_pas_lambert)/gArea(br_eez_lambert))) %>%
  ggplot(aes(x=0, y = value, 
             fill = variable     )) +
  
  geom_bar(stat="identity",width=1) + 
  coord_polar("y") +
  #facet_wrap(~ site,ncol=1) + 
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=0,y = value, label=round(value,2)*100))+
  theme_void()+
  theme(legend.position = "top")


ggsave (file = here ("output", "MPA_area_EEZ.pdf"))


# reefs within MPAs
BR_reefs_lambert <- spTransform(BR_reefs, 
                            CRS("+proj=laea +lat_0=0 +lon_0=0"))
spsp_lambert <- spTransform(spsp, 
                            CRS("+proj=laea +lat_0=0 +lon_0=0"))


# maps into lambert
intersection_ZEE_mpas_lambert <- spTransform(intersection_ZEE_mpas,CRS("+proj=laea +lat_0=0 +lon_0=0"))

# intersect_mpas_reefs 
intersect_data <- terra::intersect (intersection_ZEE_mpas_lambert[2,],BR_reefs_lambert)
plot(intersect_data,border="red")
plot(intersection_ZEE_mpas_lambert[2,],add=T,lty=2)
plot(BR_reefs_lambert,add=T,lty=2,col="green3")
plot(intersect_data,border="red",col="red",add=T)

# intersect the area
intersect_reefs_mpa <- lapply (seq (1,length(intersection_ZEE_mpas_lambert$fid)), function (i) 
  tryCatch (
    terra::intersect (intersection_ZEE_mpas_lambert[i,],BR_reefs_lambert),
    error = function (e) print(NA))
)

# penedos
# intersect the area
intersect_reefs_penedos <- lapply (seq (1,length(intersection_ZEE_mpas_lambert$fid)), function (i) 
  tryCatch (
    terra::intersect (intersection_ZEE_mpas_lambert[i,],spsp_lambert),
    error = function (e) print(NA))
)

# save
save(intersect_reefs_mpa,
     intersect_reefs_penedos,
     file= here ("output", "intersect_reefs_mpa.RData")
     )
#load(here ("output", "intersect_reefs_mpa.RData"))


plot(intersect_reefs_mpa[[194]])
plot(intersection_ZEE_mpas_lambert[194,],add=T,lty=2,border="red")
plot(BR_reefs_lambert,add=T,lty=2,col="green3")
plot(intersect_reefs_mpa[[194]],border="red",add=T)


# map
pdf (file= here ("output", "maps.pdf"),onefile=T)

# eez
plot(br_eez_lambert,border="gray35")

# brazil
plot(Brazil,border="gray35")

# mpas
plot(marine_pas_lambert,border="red",col="red",lwd=3)

# reefs
plot(BR_reefs_lambert,border="black",lwd=4)
plot(spsp_lambert,add=T,border="black",lwd=4)

# only reefs within mpas
plot(br_eez_lambert,border="gray35")
plot(BR_reefs_lambert,add=T,border="yellow4",lwd=4)
plot(spsp_lambert,add=T,border="yellow4",lwd=4)
lapply (intersect_reefs_mpa[is.na(intersect_reefs_mpa)!=T], function (i) 
  
    plot(i,add=T,border="red",col = "red",lwd=4)
    
)
lapply (intersect_reefs_penedos[is.na(intersect_reefs_penedos)!=T], function (i) 
  
  plot(i,add=T,border="red",col = "red",lwd=4)
  
)


# only reefs within mpas
plot(br_eez_lambert,border="gray35")
plot(BR_reefs_lambert,add=T,border="yellow4",lwd=4)
plot(spsp_lambert,add=T,border="yellow4",lwd=4)
lapply (intersect_reefs_mpa[is.na(intersect_reefs_mpa)!=T], function (i) 
  
  plot(i,add=T,border="red",col = "red",lwd=4)
  
)

dev.off()


## do the calculations
reef_area_within_mpas <- lapply (intersect_reefs_mpa[is.na(intersect_reefs_mpa)!=T], gArea)
reef_area_within_mpas_penedos <- lapply (intersect_reefs_penedos[is.na(intersect_reefs_penedos)!=T], gArea)
reef_area_within_mpas<- c(reef_area_within_mpas,reef_area_within_mpas_penedos)


# total reef area
total_reef_area <- gArea(BR_reefs_lambert)+gArea(spsp_lambert)
total_reef_area/gArea (br_eez_lambert)

# proportion
melt(data.frame (unprotected=1-0.374,
            allMPAs=0.374,
            only_no_take=0.16)) %>%
  ggplot(aes(x=0, y = value, 
             fill = variable     )) +
  
  geom_bar(stat="identity") + 
  coord_polar("y") +
  #facet_wrap(~ site,ncol=1) + 
  #scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=0,y = value, 
                label=round(value,2)))+
  theme_void()+
  theme(legend.position = "top")+
  ggtitle ("area recifal dentro de MPAs")

ggsave (file = here ("output", "reef_within_MPA.pdf"))

# reefs are in
melt (data.frame (all_areas = 190-61,
                  reefs_within = 61)) %>% 
  ggplot(aes(x=0, y = value, 
             fill = variable     )) +
  
  geom_bar(stat="identity",width=1) + 
  #coord_polar("y") +
  #facet_wrap(~ site,ncol=1) + 
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(x=0,y = value, label=round(value,2)))+
  theme_void()+
  theme(legend.position = "top")

ggsave (file = here ("output", "Number_MPA_with_reefs.pdf"))

# mpa class
esfera <- c(sapply (lapply (intersect_reefs_mpa[is.na(intersect_reefs_mpa)!=T], function (i) 
  i$esfera
), "[[",1),
sapply (lapply (intersect_reefs_penedos[is.na(intersect_reefs_penedos)!=T], function (i) 
  i$esfera
), "[[",1))

# group
grupo <- c(sapply (lapply (intersect_reefs_mpa[is.na(intersect_reefs_mpa)!=T], function (i) 
  i$grupo
), "[[",1),
sapply (lapply (intersect_reefs_penedos[is.na(intersect_reefs_penedos)!=T], function (i) 
  i$grupo
), "[[",1))


data.frame (reef_area = unlist(reef_area_within_mpas), 
            level=esfera,
            group=grupo) %>%
  group_by (level, group) %>%
  summarise (cover = sum (reef_area)/total_reef_area) %>% 
  ggplot(aes (x=group,y=cover,fill=level,col=level))+
    geom_bar(position="fill", stat="identity")+
#  coord_polar("y")
  geom_text(aes(x=group,y = cover+0.05, label=round(cover,2)),col="black")


# of Mpas with reefs, which ones host more

ggsave (file = here ("output", "contribution_MPA_with_reefs.pdf"))


# ranking of mpas
data.frame (reef_area = unlist (reef_area_within_mpas),
            nome_uc=c(marine_pas_lambert[is.na(intersect_reefs_mpa)!=T,]$nome_uc,
                      marine_pas_lambert[is.na(intersect_reefs_penedos)!=T,]$nome_uc),
            mpa_area = c(gArea (marine_pas_lambert[is.na(intersect_reefs_mpa)!=T,],byid=T),
                         gArea (marine_pas_lambert[is.na(intersect_reefs_penedos)!=T,],byid=T)))%>%
  mutate (prop_area = reef_area/mpa_area) %>%
  arrange (prop_area) %>%
  ggplot (aes (x=reorder(nome_uc,prop_area), y=prop_area, fill=prop_area)) +
  geom_bar(stat="identity")+
  scale_fill_viridis_c()+
  theme(axis.text.x = element_text(angle=90,size=5))


ggsave (file = here ("output", "contribution_MPA_relative_to_area.pdf"))

# not standardized by area
# ranking of mpas
data.frame (reef_area = unlist (reef_area_within_mpas)/1000,
            nome_uc=c(marine_pas_lambert[is.na(intersect_reefs_mpa)!=T,]$nome_uc,
                      marine_pas_lambert[is.na(intersect_reefs_penedos)!=T,]$nome_uc),
            mpa_area = c(gArea (marine_pas_lambert[is.na(intersect_reefs_mpa)!=T,],byid=T)/1000,
                         gArea (marine_pas_lambert[is.na(intersect_reefs_penedos)!=T,],byid=T)/1000)) %>%
  mutate (prop_area = reef_area) %>%
  arrange (prop_area) %>%
  ggplot (aes (x=reorder(nome_uc,prop_area), y=prop_area, fill=prop_area)) +
  geom_bar(stat="identity")+
  scale_fill_viridis_c()+
  ylab("Reef Area within MPAs (km2)")+
  theme(axis.text.x = element_text(angle=90,size=5))

ggsave (file = here ("output", "contribution_MPA.pdf"))

save.image()
