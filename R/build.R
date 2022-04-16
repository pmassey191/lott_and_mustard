library(plm)
library(bacondecomp)
library(did)
crime_data <- read_dta(here("Data/UpdatedStateLevelData-2010.dta")) %>% 
  filter(year>=1977 & year <= 1992)

state_treated <- crime_data%>% 
  group_by(state) %>% 
  summarise(cnt = sum(shalll))

crime_data <- inner_join(crime_data,state_treated)  %>% 
  mutate(treat_date = ifelse(cnt<16 & cnt >0,1992-cnt,0))

table_1 <- 

table_2 <- crime_data %>% 
  select()

castle  <- bacondecomp::castle

twfe <- plm(lmur ~ shalll + rpcpi + rpcim + rpcui + density + aomur  + 
              popstate + ppwm1019 + ppbm1019 + ppnm1019 + ppwf1019 + 
              ppbf1019 + ppnf1019 + ppwm2029 + ppbm2029 + ppnm2029 + 
              ppwf2029 + ppbf2029 + ppnf2029 + ppwm3039 + ppbm3039 + 
              ppnm3039 + ppwf3039 + ppbf3039 + ppnf3039 + ppwm4049 + 
              ppbm4049 + ppnm4049 + ppwf4049 + ppbf4049 + ppnf4049 + 
              ppwm5064 + ppbm5064 + ppnm5064 + ppwf5064 + ppbf5064 + 
              ppnf5064 + ppwm65o + ppbm65o + ppnm65o + ppwf65o + 
              ppbf65o + ppnf65o, data = crime_data,index = c("state","year"),model = "within",
            effect = "twoways")
print(twfe)

summary(twfe)


bacon_decomp <- bacon(lmur~shalll, data = crime_data, id_var = "fipsstat", time_var = "year")


atts <- att_gt(yname = "lmur", # LHS variable
                tname = "year", # time variable
                idname = "fipsstat", # id variable
                gname = "treat_date", # first treatment period variable
                data = crime_data, # data
                xformla = ~aomur, # no covariates
                #xformla = ~ l_police, # with covariates
                est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                bstrap = TRUE, # if TRUE compute bootstrapped SE
                biters = 1000, # number of bootstrap iterations
                print_details = FALSE, # if TRUE, print detailed results
                clustervars = "fipsstat", # cluster level
                panel = TRUE) 

summary(atts)

robust_se <- list(sqrt(diag(vcovHC(twfe, type = "HC1"))))

stargazer()
regr <- crime_data %>% select(lmur, shalll, rpcpi, rpcim, rpcui, density, aomur , 
                              popstate, ppwm1019, ppbm1019, ppnm1019, ppwf1019, 
                              ppbf1019, ppnf1019, ppwm2029, ppbm2029, ppnm2029, 
                              ppwf2029, ppbf2029, ppnf2029, ppwm3039, ppbm3039, 
                              ppnm3039, ppwf3039, ppbf3039, ppnf3039, ppwm4049, 
                              ppbm4049, ppnm4049, ppwf4049, ppbf4049, ppnf4049, 
                              ppwm5064, ppbm5064, ppnm5064, ppwf5064, ppbf5064, 
                              ppnf5064, ppwm65o, ppbm65o, ppnm65o, ppwf65o, 
                              ppbf65o, ppnf65o)
x <- cor(regr)
x
