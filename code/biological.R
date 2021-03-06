# load ----
library(tidyverse)
library(extrafont)
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()))
library (xtable)
library (knitr)
options (scipen = 999)

# data ----
#clean up event data- change names and data types etc.
events <- read.csv('./data/events_2017D06_170522.csv')
events %>% 
  mutate(ID = gsub(" ", "", STATION_ID),date = as.Date(DATE_SET, format='%m-%d-%Y'),
                  year = as.numeric(format(date, "%Y"))) %>% 
  select(year=year, date=date, Event = EVENT_ID, District = DISTRICT, 
         Dredge = DREDGE_ID, Bed = BED_SW, Type = STATION_TYPE, ID = ID, 
         slat = START_LATITUDE, slon=START_LONGITUDE, sdepth = DEPTH_START_F, 
         stime = START_TIME, speed=TOW_SPEED, maxdepth=Max_Dpth_fa, mindepth=Min_Dpth_fa, 
         elat=END_LATITUDE, elon=END_LONGITUDE, edepth=DEPTH_END_F, etime=END_TIME, 
         calc_length=TOW_LENGTH_CALC, field_length=TOW_LENGTH_FIELD,length=TOW_LENGTH_DESIGNATED,
         performance=GEAR_PERFORMANCE_CODE_SW, Vessel=VESSEL_NAME) %>% 
  filter (performance == 1, Type != "Ancillary") -> event

catch <- read.csv('./data/catchComp_2017D06_170522.csv')
catch %>% 
  select(Event = EVENT_ID, species=RACE_CODE, 
                 size_class=SCAL_SIZE_CLASS, count=COUNT,
                 sample_wt=SAMPLE_WT_KG, cond = CONDITION_CODE_RII, 
                 sample_type = SAMPLE_TYPE)  %>%
  filter (Event %in% event$Event) -> catch

awl <- read_csv('./data/awl_2017D06_170526.csv')
awl %>% 
  select(Event = EVENT_ID, species=RACE_CODE, weight=WHOLE_WT_GRAMS, 
               worm=SHELL_WORM_SW, height=SHELL_HEIGHT_MM, sex=SEX_SW, 
               gonad_cond=SCAL_GONAD_COND, blister=MUD_BLISTER_SW, 
               meat_cond=MEAT_CONDITION_SW, clapper = CLAPPER, 
               sample_type = SAMPLE_TYPE, meat_wt = MEAT_WEIGHT_GRAMS) %>%
  filter (Event %in% event$Event) -> awl


# height ----
#height/weight relationship
awl %>% 
  left_join(event) %>% 
  filter(Bed=='KSH1') %>% 
  ggplot(aes(height, fill=Vessel))+geom_density(alpha=.2)
   


#ggsave("./figs/Fig1.tiff", dpi=300, height=6, width=6, units="in")

#height densities - this way includes <100mm shells
awl %>% 
  left_join(event) %>% 
  ggplot(aes(height))+geom_density(alpha=.2,fill=4)+facet_wrap(~Bed)

#height densities - 
awl %>% 
  left_join(event) %>% 
  filter(is.na (clapper), sex != 'NA') %>% 
  ggplot(aes(height, fill=factor(sex),color=factor(sex)))+geom_density(alpha=.2)+facet_wrap(~Bed)

# OTHER SCAL BIOLOGICAL DATA TABLES ----

awl %>% 
  filter(species == 74120, is.na(clapper)) %>% 
  left_join (event) %>% 
   mutate(Month=months(date)) -> bio # join event for Bed and Month

bio %>% 
  mutate(# Month used by ratio plot
    Month=factor(months(date),levels = c("January","February","March", "April","May","June","July", 
                                             "August","September", "October","November","December")),
    BedMonth = paste(Bed,Month)) -> bio # add BedMonth var for gonad table

# freq table for each variable by bed. 
worm <- table(bio$worm, bio$Bed) 
gonad <- table(bio[bio$gonad_cond %in% c(1,2,3,4), c("gonad_cond","BedMonth")]) # exclude 0's (immature) and 5's (cannot determine) 
blist <- table(bio$blister, bio$Bed) 
weak <- table(bio$meat_cond, bio$Bed) 
clap <- table(bio$clapper, bio$Bed) 

# prop tables 
f.prop_tbl <- function(x){
  round(prop.table(x,2) * 100, 2)
}
worm.p <- f.prop_tbl(worm) 
gonad.p <- f.prop_tbl(gonad)
blist.p <- f.prop_tbl(blist) 
weak.p <- f.prop_tbl(weak) 

# bind n row to prop table 
worm.pn <-  rbind(worm.p, margin.table(worm, 2))
gonad.pn <- rbind(gonad.p, margin.table(gonad, 2))
blist.pn <- rbind(blist.p, margin.table(blist, 2))
weak.pn <-  rbind(weak.p, margin.table(weak, 2))

# change row names.  Should use LUTs or some other type of substitution, in case classes observed in future changes. 
worm.pn <- as.data.frame(worm.pn) %>% 
  mutate(Percent=c("0%", "1 - 24%", "25 - 49%", "50 - 74%",  "n")) #no 4's in 2017D06 "75 - 100%",
gonad.pn <- as.data.frame(gonad.pn) %>% 
  mutate(Stage=c("Empty", "Filling", "Full", "n")) # no 2's in 2017D06 "Initial Recovery"
blist.pn <- as.data.frame(blist.pn) %>% 
  mutate(Percent = c("0%", "1 - 24%", "n"))   # no > 1's 2017D06
weak.pn <- as.data.frame(weak.pn) %>% 
  mutate(Meats = c("Good", "Weak", "n"))

# reorder kayak beds together #Not needed for 2017D06
# worm.pn <- worm.pn[,c("Percent","EK1","WK1","KSH1", "KSH2", "KSH3")]
# gonad.pn <- gonad.pn[,c("Stage","EK1 April","WK1 April","KSH1 May","KSH1 July", "KSH2 July", "KSH3 July")] # In word, move Month to column spanners. 
# blist.pn <- blist.pn[,c("Percent","EK1","WK1","KSH1", "KSH2", "KSH3")]
# weak.pn <- weak.pn[,c('Meats',"EK1","WK1","KSH1", "KSH2", "KSH3")]


write_csv(worm.pn,'./output/worm.csv')
write_csv(gonad.pn,'./output/gonad.csv')
write_csv(blist.pn,'./output/blist.csv')
write_csv(weak.pn,'./output/weak.csv')

# Density plot of meat ratio by month ----

bio %>% 
  mutate(mr = meat_wt/weight) %>% 
  filter(Bed=='KSH1'|Bed=='KSH2'|Bed=='KSH3') %>% 
   ggplot(aes(mr, fill=Month))+geom_density(alpha=.5) + 
   scale_fill_manual(values=c( "#f0f0f0",  "#bdbdbd", "#636363")) + 
   theme(legend.position=c(.75, .75)) + 
   xlab("Meat Weight/ Round Weight") + ylab('Density')

ggsave("./figs/RatioByMonth.png", dpi=300, height=4.5, width=6.5, units="in")



awl %>% 
  filter(species == 74120, !is.na(clapper)) %>% 
   left_join(event) %>% 
   ggplot(aes(height))+geom_histogram(fill=4, alpha=.2, color=1,bins=50)+
   facet_wrap(~Bed, ncol=1,scale='free_y')+
   xlab('Shell height (mm)')+ylab('Number') + 
   theme(strip.background = element_blank())

ggsave("./figs/Clappers.png", dpi=300, height=8.5, width=6.5, units="in")


bio %>% 
  filter(Bed=='KSH1') %>% 
   ggplot(aes(log(height), log(weight), color=Month))+
   geom_point(aes(shape=Month)) + xlim(4.5,5.4) + 
   stat_smooth(method='lm') + scale_color_manual(values=c('black', 'gray'))+ 
   guides(color=guide_legend(override.aes=list(fill=NA))) + ylab('log Round weight') + 
   xlab('log Shell height')+ 
   theme(legend.position=c(.75, .25))

ggsave("./figs/rndwt-shellht.png", dpi=300, height=4.5, width=6.5, units="in")
