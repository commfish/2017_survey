####### 2016 Scallop Statewide Survey
####### Ben Williams / Katie Palof 
####### ben.williams@alaska.gov / katie.palof@alaska.gov

# Functions to bootstrap scallop survey by numbers, weight, and meat weight ratio


# Function to summarize - does NOT include bootstrap
f.sum <- function(x){
  # first turn the list to a dataframe
  # use dplyr to summarise each list
  # output is one row all stats.
  
  x = as.data.frame(x)
  x %>%
    group_by(year, District, Bed, variable)%>%
    summarise(n=mean(n),
              area = mean(area_nm2) ,
              dbar = (1/n*sum(di)),
              var_dbar=1/((n)-1)*sum((di-dbar)^2) ,
              cv=sqrt(var_dbar)/dbar*100,
              ss=sum((di-dbar)^2),
              N=area*dbar,
              varN=(area^2)*1/n*1/(n-1)*ss,
              cvN=sqrt(varN)/N*100) -> out
  out
}

# bootstrap ----
# used for numbers and weights-------------
f.it <- function(x){
  # first turn the list to a dataframe
  # extract the identifiers to append to the results
  # function to be run each time for calculating dbar & N
  # function to sample by rows
  # replicate the data 1000 times
  
  x = as.data.frame(x)
  y = x[1,c(2:4,8)]
  boot.it <- function(x){
    d_bar = sum(x$di)/mean(x$n)
    N=mean(x$area_nm2)*d_bar
    c(d_bar, N)
  }
  
  f.do <- function(x){
    x %>% sample_n(nrow(.), replace=TRUE) -> x 
    boot.it(x)
  }
  
  as.data.frame(t(replicate(1000,f.do(x)))) -> out
  names(out) <- c('dbar','N')
  cbind(out,y)
}

# bootstrap II----
# used for meat weight ratio
f.wt <- function(x){
  # function bootstraps meat weight ratio by bed, not by individual event
  # first turn the list to a dataframe
  # small function to group and calculate mean for each bootstrap sample
  # replicate each sample 1000 x by year bed, district etc
  # calculate ratio with function
  
  x = as.data.frame(x)
  
  f.do <- function(y){
    y %>% 
      group_by(year,District,Bed) %>% 
      summarise(ratio = mean(ratio))
  }
  
  replicate(1000,sample_n(x, nrow(x), replace=T), simplify=FALSE) %>% 
    lapply(., f.do) %>% 
    bind_rows %>% 
    mutate(replicate=1:n())
}


#Clapper density summerization function --------------------
f.clap <- function(x){
  # first turn the list to a dataframe
  # use dplyr to summarise each list
  # output is one row all stats.
  
  x = as.data.frame(x)
  x %>%
    group_by(year, District, Bed)%>%
    summarise(n=mean(n),
              area = mean(area_nm2) ,
              dbar_c = (1/n*sum(di)),
              var_dbar_c=1/((n)-1)*sum((di-dbar_c)^2) ,
              cv=sqrt(var_dbar_c)/dbar_c*100,
              ss=sum((di-dbar_c)^2),
              N_c=area*dbar_c,
              varN_c = (area^2)*1/n*1/(n-1)*ss,
              cvN_c = sqrt(varN_c)/N_c*100, 
              dbar_wt = (1/n*sum(di_wt)) ,
              var_dbar_wt=1/((n)-1)*sum((di_wt-dbar_wt)^2) ,
              cv_wt=sqrt(var_dbar_wt)/dbar_wt*100 ,
              ss_wt=sum((di_wt-dbar_wt)^2) ,
              Wt_c=area*dbar_wt ,
              varWt_c=(area^2)*1/n*1/(n-1)*ss_wt ,
              cvWt_c=sqrt(varWt_c)/Wt_c*100) -> out
  out
}


# K-S Function ----
ks_func <- function(x){
   #first turn the list into a dataframe
   #use dplyr to seperate into two groups y and z to compare
   # output is event id and p-value
   x = as.data.frame(x)
   x %>% spread(m_weight, height, fill=NA) ->inter
   inter %>% summarise(n=n()) ->n
   ks <-ks.test(inter$ht, inter$mw)
   p.value <- ks$p.value
   out <- cbind(n, p.value)
   out
}
