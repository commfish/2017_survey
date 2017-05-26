###############################
## EXPAND SCAL LENGTHS       ##
###############################
# josh mumm 
# accounts for unequal sampling density of small vs large scals
# by expanding sampled size distribution of each to the total number caught by event  

## PREP ----
set.seed(15343437) 
library (reshape2)
library (FSA)
library(tidyverse)
library(extrafont)
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()))

awl <- read.csv('./data/awl_2017D06_170526.csv')
event <- read.csv('./data/events_2017D06_170522.csv')
cc <- read.csv('./data/catchComp_2017D06_170522.csv')

awl %>% filter (RACE_CODE==74120 & is.na(CLAPPER)) %>%
   select(Event = EVENT_ID, sc=SCAL_SIZE_CLASS, sh=SHELL_HEIGHT_MM) -> awl
event %>%
   filter (GEAR_PERFORMANCE_CODE_SW == 1) %>%
   select(Event = EVENT_ID, Bed = BED_SW) -> event
cc %>%
   filter (RACE_CODE==74120 & CONDITION_CODE_RII == 1) %>%
   select(Event = EVENT_ID, sc = SCAL_SIZE_CLASS, cnt = COUNT) -> cc

#join bed from events to awl and cc
awl <- merge(awl, event, by = "Event", all.x = T) 
cc  <- merge(cc, event, by = "Event", all.x = T)

# total caught by size class for each event 
cc %>% 
   group_by(Event,Bed,sc) %>% 
   summarise (tot = sum(cnt)) -> tot 

tot_w <- dcast(tot, Event + Bed ~ sc, value.var = "tot", fill = 0)     # cast tot from long to wide
names(tot_w)[3:4] <- c('L', 'S')

## EXPAND SIZE DISTRIBUTIONS ---- 
# for each event there will 2 seperate dists to expand - smls + lrgs

#create empty df to hold final expanded awl 
dat <-data.frame(Event=factor(), Bed = factor(), sh = double())
events <- unique(tot_w$Event)  

#i <- '2016D02004' # start w one event - here 242 lrgs and 168 smls

for(i in events){    
  Sys.sleep(0.1)
  print ( c("####",paste (i, "   START") , "####"))
  
  #select one event from awl and break by sizeClass
  awl_L <- awl[awl$Event == i & awl$sc == '1',]
  awl_S <- awl[awl$Event == i & awl$sc == '2',]
  
  # num measured
  mL <-  nrow(awl_L)
  mS <-  nrow(awl_S)

  # tot caught 
  totL <- tot_w[tot_w$Event == i, "L"]
  totS <- tot_w[tot_w$Event == i, "S"]
  
  #create empty vectors for expanded size dists so that object exists even if no scals in it, prevents latter errors when combining
  eawl_L <- numeric(0) 
  eawl_S <- numeric(0)  
  
  #expand LARGES 
  if(totL > 0 & mL > 1 & totL- mL > 1)   # conditional to prevent terminal errors 
  {eawl_L <- expandLenFreq(awl_L$sh, w=.1, total = totL, decimals = 1) } # these are the additional, not measured scals. 
  eawl_L # note these are only the additional, not measured scals.
  length(eawl_L) - (totL - mL) #compare num of expanded to tot caught minus measured.  Should = 0
  
  #expand SMALLS
  if(totS > 0 & mS > 1 & totS - mS > 1)   
  {eawl_S <- expandLenFreq(awl_S$sh, w=.1, total = totS, decimals = 1) }
  eawl_S # note this is only the additional ones, not measured.
  length(eawl_S) - (totS - mS) #compare num of expanded to tot caught minus measured.  Should = 0

  #compare hists.  change for S or L 
  par(mfrow= c(2,1))
  hist (awl_L$sh, breaks = seq(0,250,1), main  = c("measured", i), freq = T, col = 'red')
  hist (eawl_L, breaks = seq(0,250,1), main  = c("additional", i), freq = T, col = 'blue')
  
  #combine measured with additional lengths
  L <- c(awl_L$sh, eawl_L)
  S <- c(awl_S$sh, eawl_S)
  
  #compare total measurements in expanded awls to tot cnt from CC. Difs should equal 0. 
  length(L) - totL
  length(S) - totS
  
  #assign event and bed to vector of lenghts for each sc 
  r <- length(L)
  L.df <- data.frame(Event = as.factor(rep(i, r)),
                     Bed = as.factor(rep(tot_w[tot_w$Event == i, "Bed"], r)),
                     sc = as.factor(rep("1", r)),
                     sh = L)
  r <- length(S)
  S.df <- data.frame(Event = as.factor(rep(i, r)),
                     Bed = as.factor(rep(tot_w[tot_w$Event == i, "Bed"], r)),
                     sc = as.factor(rep("2", r)),
                     sh = S)

  #and bind these dfs together as total expanded lengths (measured + additional) for event i 
  all.df <- rbind(L.df,S.df)
  nrow(all.df) - totL - totS # compare num recs in expanded df to total from CC
  
  dat <- rbind(dat,all.df) # append to main df for all events 

  Sys.sleep(0.2) 
  print ( c("####",paste (i, "   COMPLETE") , "####"))
}

write.csv (dat,"expandedScalLengths.csv")

## ERROR CHECKING ----                                                                                                   

#compare totals
nrow(dat)
sum(tot_w[,c("L","S")])

#### compare histograms of awl to dat (combined measured + expanded)
library (lattice)
par(mfcol = c(2,1))
# all
  hist (~ sh , data = awl, breaks = seq(0,200,1), main  = c("measured", "all beds"), freq = T, col = 'red' )
  hist (~ sh , data = dat, breaks = seq(0,200,1), main  = c("expanded", "all beds"), freq = T, col = 'blue')

#by Bed
beds <- (sort(unique(dat$Bed)))
for (i in beds) {
  hist (~ sh , data = subset(awl, Bed == i), breaks = seq(0,200,1), main  = c("measured", i), freq = T, col = 'red' )
  hist (~ sh , data = subset(dat, Bed == i), breaks = seq(0,200,1), main  = c("expanded", i), freq = T, col = 'blue')
}


#set the levels for plotting
dat <- within(dat, Bed <- factor(Bed, levels = c('KSH1','KSH2')))
dat %>% group_by(Bed) %>% summarise(n=n()) %>% mutate(x=c(0,0), y=c(50,1)) -> dat.n

ggplot(dat,aes(sh))+geom_histogram(fill=4, alpha=.2, color=1, bins=75)+
   facet_wrap(~Bed, ncol=1,scale='free_y')+
   xlab('Shell height (mm)')+ylab('Number') + 
   geom_text(data=dat.n, aes(x, y, label=paste0('n=',n)), size=3) + 
   theme(strip.background = element_blank())

ggsave("./figs/Heights.png", dpi=300, height=8.5, width=6.5, units="in")
