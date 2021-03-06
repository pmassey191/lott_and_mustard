library(vtable)

crime_data <- read_dta(here("Data/UpdatedStateLevelData-2010.dta")) %>% 
  filter(year>=1977 & year <= 1992)

state_treated <- crime_data%>% 
  group_by(state) %>% 
  summarise(cnt = sum(shalll))

crime_data <- inner_join(crime_data,state_treated)  %>% 
  mutate(treat_date = ifelse(cnt >0,1993-cnt,0))


states_1977 = c("Alabama, Connecticut, New Hampshire, North Dakota, South Dakota, Vermont, Washington")
states_1981 = c("Indiana")
states_1986 = c("Maine")
states_1988 = c("Florida")
states_1989 = c("Virgina")
states_1990 = c("Georgia, Pennsylvania, West Virginia")
states_1991 = c("Idaho, Mississippi, Oregon")
states_1992 = c("Montana")

Years <- c("1977","1981","1986","1988","1989","1990","1991","1992")
States = c(states_1977,states_1981,states_1986,states_1988,states_1989,states_1990,states_1991,states_1992)

kbl(data.frame(Years,States),format ="latex",label = "rollout",caption = "State Rollout of Concealed Carry",booktabs = TRUE,
    position = "h",centering = TRUE)




table_2 <- crime_data %>% 
  select(shalll, aovio ,aopro, aomur, aorap, aorob, aoaga, aobur, aolar, aoaut, 
         ratvio ,ratpro, ratmur, ratrap, ratrob, rataga, ratbur, ratlar, rataut,
         rpcpi, rpcui, rpcim, rpcrpo, popstate, density) %>% as.data.frame()

stargazer(table_2, type = "latex", digits = 2,min.max = FALSE)

table_2_summary %>% 
  kbl(caption = "Test",
      format = "latex") %>% 
  kable_styling(latex_options = c("striped","hold_position"))

#two way fixed effect models

twfe_lvio <- feols(lvio ~ shalll + rpcrpo + rpcpi + rpcim + rpcui + density + aovio + popstate + 
                      ppwm1019 + ppbm1019 + ppnm1019 + ppwf1019 + ppbf1019 + ppnf1019 +
                      ppwm2029 + ppbm2029 + ppnm2029 + ppwf2029 + ppbf2029 + ppnf2029 +
                      ppwm3039 + ppbm3039 + ppnm3039 + ppwf3039 + ppbf3039 + ppnf3039 +
                      ppwm4049 + ppbm4049 + ppnm4049 + ppwf4049 + ppbf4049 + ppnf4049 + 
                      ppwm5064 + ppbm5064 + ppnm5064 + ppwf5064 + ppbf5064 + ppnf5064 | state + year,
                   data = crime_data, vcov = ~state+year)


twfe_lmur <- feols(lmur ~ shalll + rpcpi + rpcim + rpcui + rpcrpo + density + aomur + popstate + 
                 ppwm1019 + ppbm1019 + ppnm1019 + ppwf1019 + ppbf1019 + ppnf1019 +
                 ppwm2029 + ppbm2029 + ppnm2029 + ppwf2029 + ppbf2029 + ppnf2029 +
                 ppwm3039 + ppbm3039 + ppnm3039 + ppwf3039 + ppbf3039 + ppnf3039 +
                 ppwm4049 + ppbm4049 + ppnm4049 + ppwf4049 + ppbf4049 + ppnf4049 + 
                 ppwm5064 + ppbm5064 + ppnm5064 + ppwf5064 + ppbf5064 + ppnf5064 | state + year, 
                 data = crime_data, vcov = ~state+year)

twfe_lrap <- feols(lrap ~ shalll + rpcpi + rpcim + rpcui + rpcrpo + density + aorap + popstate + 
                      ppwm1019 + ppbm1019 + ppnm1019 + ppwf1019 + ppbf1019 + ppnf1019 +
                      ppwm2029 + ppbm2029 + ppnm2029 + ppwf2029 + ppbf2029 + ppnf2029 +
                      ppwm3039 + ppbm3039 + ppnm3039 + ppwf3039 + ppbf3039 + ppnf3039 +
                      ppwm4049 + ppbm4049 + ppnm4049 + ppwf4049 + ppbf4049 + ppnf4049 + 
                      ppwm5064 + ppbm5064 + ppnm5064 + ppwf5064 + ppbf5064 + ppnf5064 | state + year,
                   data = crime_data, vcov = ~state+year)

twfe_laga <- feols(laga ~ shalll + rpcpi + rpcim + rpcui + rpcrpo + density + aoaga + popstate + 
                      ppwm1019 + ppbm1019 + ppnm1019 + ppwf1019 + ppbf1019 + ppnf1019 +
                      ppwm2029 + ppbm2029 + ppnm2029 + ppwf2029 + ppbf2029 + ppnf2029 +
                      ppwm3039 + ppbm3039 + ppnm3039 + ppwf3039 + ppbf3039 + ppnf3039 +
                      ppwm4049 + ppbm4049 + ppnm4049 + ppwf4049 + ppbf4049 + ppnf4049 + 
                      ppwm5064 + ppbm5064 + ppnm5064 + ppwf5064 + ppbf5064 + ppnf5064 | state + year,
                   data = crime_data, vcov = ~state+year)

twfe_lrob <- feols(lrob ~ shalll + rpcpi + rpcim + rpcui + rpcrpo + density + aorob + popstate + 
                     ppwm1019 + ppbm1019 + ppnm1019 + ppwf1019 + ppbf1019 + ppnf1019 +
                     ppwm2029 + ppbm2029 + ppnm2029 + ppwf2029 + ppbf2029 + ppnf2029 +
                     ppwm3039 + ppbm3039 + ppnm3039 + ppwf3039 + ppbf3039 + ppnf3039 +
                     ppwm4049 + ppbm4049 + ppnm4049 + ppwf4049 + ppbf4049 + ppnf4049 + 
                     ppwm5064 + ppbm5064 + ppnm5064 + ppwf5064 + ppbf5064 + ppnf5064 | state + year,
                   data = crime_data, vcov = ~state+year)

twfe_lpro <- feols(lpro ~ shalll + rpcpi + rpcim + rpcui + rpcrpo + density + aopro + popstate + 
                     ppwm1019 + ppbm1019 + ppnm1019 + ppwf1019 + ppbf1019 + ppnf1019 +
                     ppwm2029 + ppbm2029 + ppnm2029 + ppwf2029 + ppbf2029 + ppnf2029 +
                     ppwm3039 + ppbm3039 + ppnm3039 + ppwf3039 + ppbf3039 + ppnf3039 +
                     ppwm4049 + ppbm4049 + ppnm4049 + ppwf4049 + ppbf4049 + ppnf4049 + 
                     ppwm5064 + ppbm5064 + ppnm5064 + ppwf5064 + ppbf5064 + ppnf5064 | state + year,
                   data = crime_data, vcov = ~state+year)

twfe_lbur <- feols(lbur ~ shalll + rpcpi + rpcim + rpcui + rpcrpo + density + aobur + popstate + 
                     ppwm1019 + ppbm1019 + ppnm1019 + ppwf1019 + ppbf1019 + ppnf1019 +
                     ppwm2029 + ppbm2029 + ppnm2029 + ppwf2029 + ppbf2029 + ppnf2029 +
                     ppwm3039 + ppbm3039 + ppnm3039 + ppwf3039 + ppbf3039 + ppnf3039 +
                     ppwm4049 + ppbm4049 + ppnm4049 + ppwf4049 + ppbf4049 + ppnf4049 + 
                     ppwm5064 + ppbm5064 + ppnm5064 + ppwf5064 + ppbf5064 + ppnf5064 | state + year, 
                   data = crime_data, vcov = ~state+year)

twfe_llar <- feols(llar ~ shalll + rpcpi + rpcim + rpcui + rpcrpo + density + aolar + popstate + 
                     ppwm1019 + ppbm1019 + ppnm1019 + ppwf1019 + ppbf1019 + ppnf1019 +
                     ppwm2029 + ppbm2029 + ppnm2029 + ppwf2029 + ppbf2029 + ppnf2029 +
                     ppwm3039 + ppbm3039 + ppnm3039 + ppwf3039 + ppbf3039 + ppnf3039 +
                     ppwm4049 + ppbm4049 + ppnm4049 + ppwf4049 + ppbf4049 + ppnf4049 + 
                     ppwm5064 + ppbm5064 + ppnm5064 + ppwf5064 + ppbf5064 + ppnf5064 | state + year, 
                   data = crime_data, vcov = ~state+year)

twfe_laut <- feols(laut ~ shalll + rpcpi + rpcim + rpcui + rpcrpo + density + aoaut + popstate + 
                     ppwm1019 + ppbm1019 + ppnm1019 + ppwf1019 + ppbf1019 + ppnf1019 +
                     ppwm2029 + ppbm2029 + ppnm2029 + ppwf2029 + ppbf2029 + ppnf2029 +
                     ppwm3039 + ppbm3039 + ppnm3039 + ppwf3039 + ppbf3039 + ppnf3039 +
                     ppwm4049 + ppbm4049 + ppnm4049 + ppwf4049 + ppbf4049 + ppnf4049 + 
                     ppwm5064 + ppbm5064 + ppnm5064 + ppwf5064 + ppbf5064 + ppnf5064 | state + year, 
                   data = crime_data, vcov = ~state+year)


summary(twfe_lmur)
#bacon decomp

bacon_decomp_lvio <- bacon(lvio~shalll, data = crime_data, id_var = "fipsstat", time_var = "year") %>% 
  filter(type == "Earlier vs Later Treated"| type == "Later vs Earlier Treated")
bacon_decomp_lmur <- bacon(lmur~shalll, data = crime_data, id_var = "fipsstat", time_var = "year") %>% 
  filter(type == "Earlier vs Later Treated"| type == "Later vs Earlier Treated")
bacon_decomp_lrap <- bacon(lrap~shalll, data = crime_data, id_var = "fipsstat", time_var = "year") %>% 
  filter(type == "Earlier vs Later Treated"| type == "Later vs Earlier Treated")
bacon_decomp_laga <- bacon(laga~shalll, data = crime_data, id_var = "fipsstat", time_var = "year") %>% 
  filter(type == "Earlier vs Later Treated"| type == "Later vs Earlier Treated")
bacon_decomp_lrob <- bacon(lrob~shalll, data = crime_data, id_var = "fipsstat", time_var = "year") %>% 
  filter(type == "Earlier vs Later Treated"| type == "Later vs Earlier Treated")
bacon_decomp_lpro <- bacon(lpro~shalll, data = crime_data, id_var = "fipsstat", time_var = "year") %>% 
  filter(type == "Earlier vs Later Treated"| type == "Later vs Earlier Treated")
bacon_decomp_lbur <- bacon(lbur~shalll, data = crime_data, id_var = "fipsstat", time_var = "year") %>% 
  filter(type == "Earlier vs Later Treated"| type == "Later vs Earlier Treated")
bacon_decomp_llar <- bacon(llar~shalll, data = crime_data, id_var = "fipsstat", time_var = "year") %>% 
  filter(type == "Earlier vs Later Treated"| type == "Later vs Earlier Treated")
bacon_decomp_laut <- bacon(laut~shalll, data = crime_data, id_var = "fipsstat", time_var = "year") %>% 
  filter(type == "Earlier vs Later Treated"| type == "Later vs Earlier Treated")



sum_vio <- aggregate(weight ~ type,
                    data = bacon_decomp_lvio,
                    FUN = sum
)
sum_vio$avg_est <- c(by(
  bacon_decomp_lvio, bacon_decomp_lvio$type,
  function(x) round(weighted.mean(x$estimate, x$weight), 5)
))

sum_mur <- aggregate(weight ~ type,
                     data = bacon_decomp_lmur,
                     FUN = sum
)
sum_mur$avg_est <- c(by(
  bacon_decomp_lmur, bacon_decomp_lmur$type,
  function(x) round(weighted.mean(x$estimate, x$weight), 5)
))

sum_rap <- aggregate(weight ~ type,
                     data = bacon_decomp_lrap,
                     FUN = sum
)
sum_rap$avg_est <- c(by(
  bacon_decomp_lrap, bacon_decomp_lrap$type,
  function(x) round(weighted.mean(x$estimate, x$weight), 5)
))
sum_aga <- aggregate(weight ~ type,
                     data = bacon_decomp_laga,
                     FUN = sum
)
sum_aga$avg_est <- c(by(
  bacon_decomp_laga, bacon_decomp_laga$type,
  function(x) round(weighted.mean(x$estimate, x$weight), 5)
))
sum_rob <- aggregate(weight ~ type,
                     data = bacon_decomp_lrob,
                     FUN = sum
)
sum_rob$avg_est <- c(by(
  bacon_decomp_lrob, bacon_decomp_lrob$type,
  function(x) round(weighted.mean(x$estimate, x$weight), 5)
))
sum_pro <- aggregate(weight ~ type,
                     data = bacon_decomp_lpro,
                     FUN = sum
)
sum_pro$avg_est <- c(by(
  bacon_decomp_lpro, bacon_decomp_lpro$type,
  function(x) round(weighted.mean(x$estimate, x$weight), 5)
))
sum_bur <- aggregate(weight ~ type,
                     data = bacon_decomp_lbur,
                     FUN = sum
)
sum_bur$avg_est <- c(by(
  bacon_decomp_lbur, bacon_decomp_lbur$type,
  function(x) round(weighted.mean(x$estimate, x$weight), 5)
))
sum_lar <- aggregate(weight ~ type,
                     data = bacon_decomp_llar,
                     FUN = sum
)
sum_lar$avg_est <- c(by(
  bacon_decomp_llar, bacon_decomp_llar$type,
  function(x) round(weighted.mean(x$estimate, x$weight), 5)
))
sum_aut <- aggregate(weight ~ type,
                     data = bacon_decomp_laut,
                     FUN = sum
)
sum_aut$avg_est <- c(by(
  bacon_decomp_laut, bacon_decomp_laut$type,
  function(x) round(weighted.mean(x$estimate, x$weight), 5)
))


df <- rbind(sum_vio,sum_mur,sum_rap,sum_aga,sum_rob,sum_pro,sum_bur,sum_lar,sum_aut)
model <- rep(c("Violent Crime","Murder","Rape","Aggrevated Assualt","Robbery","Property Crime","Burglary","Larceny","Auto Theft"),each = 2)
df$model <- model

kbl(df, caption = "test",booktabs = TRUE) %>% 
  kable_styling() %>% 
  pack_rows(group_label = ,1,2)



kbl(df,booktabs = TRUE,format= "latex",caption = "Bacon Decomposition")





plot_violent_cs <- ggplot(bacon_decomp_lvio, aes(x = weight, y = estimate, color = factor(type)))+
                  labs(x = "Weight", y = "Estimate", color = "Type",title = "Goodman-Bacon Decomposition: Violent Crimes")+
                   geom_point()+
                  theme_minimal()
plot_mur_cs <- ggplot(bacon_decomp_lmur, aes(x = weight, y = estimate, color = factor(type)))+
  labs(x = "Weight", y = "Estimate", color = "Type",title = "Goodman-Bacon Decomposition: Murder ")+
  geom_point()+
  theme_minimal()
plot_rape_cs<- ggplot(bacon_decomp_lrap, aes(x = weight, y = estimate, color = factor(type)))+
  labs(x = "Weight", y = "Estimate", color = "Type",title = "Goodman-Bacon Decomposition: Rape ")+
  geom_point()+
  theme_minimal()
plot_aga_cs <- ggplot(bacon_decomp_laga, aes(x = weight, y = estimate, color = factor(type)))+
  labs(x = "Weight", y = "Estimate", color = "Type",title = "Goodman-Bacon Decomposition: Aggravated Assault")+
  geom_point()+
  theme_minimal()
plot_rob_cs <- ggplot(bacon_decomp_lrob, aes(x = weight, y = estimate, color = factor(type)))+
  labs(x = "Weight", y = "Estimate", color = "Type",title = "Goodman-Bacon Decomposition: Robbery")+
  geom_point()+
  theme_minimal()
plot_pro_cs <- ggplot(bacon_decomp_lpro, aes(x = weight, y = estimate, color = factor(type)))+
  labs(x = "Weight", y = "Estimate", color = "Type",title = "Goodman-Bacon Decomposition: Property Crime")+
  geom_point()+
  theme_minimal()
plot_bur_cs <- ggplot(bacon_decomp_lbur, aes(x = weight, y = estimate, color = factor(type)))+
  labs(x = "Weight", y = "Estimate", color = "Type", title = "Goodman-Bacon Decomposition: Burglary")+
  geom_point()+
  theme_minimal()
plot_lar_cs <- ggplot(bacon_decomp_llar, aes(x = weight, y = estimate, color = factor(type)))+
  labs(x = "Weight", y = "Estimate", color = "Type",title = "Goodman-Bacon Decomposition: Larceny")+
  geom_point()+
  theme_minimal()
plot_aut_cs <- ggplot(bacon_decomp_laut, aes(x = weight, y = estimate, color = factor(type)))+
  labs(x = "Weight", y = "Estimate", color = "Type",title = "Goodman-Bacon Decomposition: Auto Theft")+
  geom_point()+
  theme_minimal()

ggsave(here("figures/vio_cs.jpg"),plot_violent_cs)
ggsave(here("figures/mur_cs.jpg"),plot_mur_cs)
ggsave(here("figures/aga_cs.jpg"),plot_aga_cs)
ggsave(here("figures/aut_cs.jpg"),plot_aut_cs)
ggsave(here("figures/bur_cs.jpg"),plot_bur_cs)
ggsave(here("figures/lar_cs.jpg"),plot_lar_cs)
ggsave(here("figures/pro_cs.jpg"),plot_pro_cs)
ggsave(here("figures/rape_cs.jpg"),plot_rape_cs)
ggsave(here("figures/rob_cs.jpg"),plot_rob_cs)



#carlos santana
atts_lvio <- att_gt(yname = "lvio", # LHS variable
                    tname = "year", # time variable
                    idname = "fipsstat", # id variable
                    gname = "treat_date", # first treatment period variable
                    data = crime_data, # data
                    xformla = ~NULL, #no covariates
                    #xformla = ~ l_police, # with covariates
                    est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                    control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                    bstrap = TRUE, # if TRUE compute bootstrapped SE
                    biters = 1000, # number of bootstrap iterations
                    print_details = FALSE, # if TRUE, print detailed results
                    clustervars = "fipsstat", # cluster level
                    panel = TRUE) 


atts_lmur <- att_gt(yname = "lmur", # LHS variable
                    tname = "year", # time variable
                    idname = "fipsstat", # id variable
                    gname = "treat_date", # first treatment period variable
                    data = crime_data, # data
                    xformla = ~NULL, # no covariates
                    #xformla = ~ l_police, # with covariates
                    est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                    control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                    bstrap = TRUE, # if TRUE compute bootstrapped SE
                    biters = 1000, # number of bootstrap iterations
                    print_details = FALSE, # if TRUE, print detailed results
                    clustervars = "fipsstat", # cluster level
                    panel = TRUE) 

atts_lrap <- att_gt(yname = "lrap", # LHS variable
                    tname = "year", # time variable
                    idname = "fipsstat", # id variable
                    gname = "treat_date", # first treatment period variable
                    data = crime_data, # data
                    xformla = ~NULL, # no covariates
                    #xformla = ~ l_police, # with covariates
                    est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                    control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                    bstrap = TRUE, # if TRUE compute bootstrapped SE
                    biters = 1000, # number of bootstrap iterations
                    print_details = FALSE, # if TRUE, print detailed results
                    clustervars = "fipsstat", # cluster level
                    panel = TRUE) 

atts_laga <- att_gt(yname = "laga", # LHS variable
                    tname = "year", # time variable
                    idname = "fipsstat", # id variable
                    gname = "treat_date", # first treatment period variable
                    data = crime_data, # data
                    xformla = ~NULL, # no covariates
                    #xformla = ~ l_police, # with covariates
                    est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                    control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                    bstrap = TRUE, # if TRUE compute bootstrapped SE
                    biters = 1000, # number of bootstrap iterations
                    print_details = FALSE, # if TRUE, print detailed results
                    clustervars = "fipsstat", # cluster level
                    panel = TRUE) 

atts_lrob <- att_gt(yname = "lrob", # LHS variable
                    tname = "year", # time variable
                    idname = "fipsstat", # id variable
                    gname = "treat_date", # first treatment period variable
                    data = crime_data, # data
                    xformla = ~NULL, # no covariates
                    #xformla = ~ l_police, # with covariates
                    est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                    control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                    bstrap = TRUE, # if TRUE compute bootstrapped SE
                    biters = 1000, # number of bootstrap iterations
                    print_details = FALSE, # if TRUE, print detailed results
                    clustervars = "fipsstat", # cluster level
                    panel = TRUE) 

atts_lpro <- att_gt(yname = "lpro", # LHS variable
                    tname = "year", # time variable
                    idname = "fipsstat", # id variable
                    gname = "treat_date", # first treatment period variable
                    data = crime_data, # data
                    xformla = ~NULL, # no covariates
                    #xformla = ~ l_police, # with covariates
                    est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                    control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                    bstrap = TRUE, # if TRUE compute bootstrapped SE
                    biters = 1000, # number of bootstrap iterations
                    print_details = FALSE, # if TRUE, print detailed results
                    clustervars = "fipsstat", # cluster level
                    panel = TRUE) 

atts_lbur <- att_gt(yname = "lbur", # LHS variable
                    tname = "year", # time variable
                    idname = "fipsstat", # id variable
                    gname = "treat_date", # first treatment period variable
                    data = crime_data, # data
                    xformla = ~NULL, # no covariates
                    #xformla = ~ l_police, # with covariates
                    est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                    control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                    bstrap = TRUE, # if TRUE compute bootstrapped SE
                    biters = 1000, # number of bootstrap iterations
                    print_details = FALSE, # if TRUE, print detailed results
                    clustervars = "fipsstat", # cluster level
                    panel = TRUE) 

atts_llar <- att_gt(yname = "llar", # LHS variable
                tname = "year", # time variable
                idname = "fipsstat", # id variable
                gname = "treat_date", # first treatment period variable
                data = crime_data, # data
                xformla = ~NULL, # no covariates
                #xformla = ~ l_police, # with covariates
                est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                bstrap = TRUE, # if TRUE compute bootstrapped SE
                biters = 1000, # number of bootstrap iterations
                print_details = FALSE, # if TRUE, print detailed results
                clustervars = "fipsstat", # cluster level
                panel = TRUE) 

atts_laut <- att_gt(yname = "laut", # LHS variable
                    tname = "year", # time variable
                    idname = "fipsstat", # id variable
                    gname = "treat_date", # first treatment period variable
                    data = crime_data, # data
                    xformla = ~NULL, # no covariates
                    #xformla = ~ l_police, # with covariates
                    est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                    control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                    bstrap = TRUE, # if TRUE compute bootstrapped SE
                    biters = 1000, # number of bootstrap iterations
                    print_details = FALSE, # if TRUE, print detailed results
                    clustervars = "fipsstat", # cluster level
                    panel = TRUE)


attlvio <- aggte(atts_lvio, na.rm = TRUE)
attlmur <- aggte(atts_lmur, na.rm = TRUE)
attlrap <- aggte(atts_lrap, na.rm = TRUE)
attlaga <- aggte(atts_laga, na.rm = TRUE)
attlrob <- aggte(atts_lrob, na.rm = TRUE)
attlpro <- aggte(atts_lpro, na.rm = TRUE)
attlbur <- aggte(atts_lbur, na.rm = TRUE)
attllar <- aggte(atts_llar, na.rm = TRUE)
attlaut <- aggte(atts_laut, na.rm = TRUE)

attlvio$overall.att
attlmur$overall.att
attlrap$overall.att
attlaga$overall.att
attlrob$overall.att
attlpro$overall.att
attlbur$overall.att
attlaut$overall.att




#Sun and Abraham

lvio_sa <- feols(lvio ~ sunab(treat_date, year)|fipsstat + year, data = crime_data, 
                 vcov = ~fipsstat+year)
iplot(lvio_sa,ref.line = -1)

lmur_sa <- feols(lmur ~ sunab(treat_date, year)|fipsstat + year, 
                 data = crime_data %>% filter(treat_date!=1977), 
                 vcov = ~fipsstat+year)
iplot(lmur_sa,ref.line = -1)

lrap_sa <- feols(lrap ~ sunab(treat_date, year)|fipsstat + year, 
                 data = crime_data %>% filter(treat_date!=1977), 
                 vcov = ~fipsstat+year)
iplot(lrap_sa,ref.line = -1)

laga_sa <- feols(laga ~ sunab(treat_date, year)|fipsstat + year, data = crime_data %>% 
                   filter(treat_date!=1977), 
                 vcov = ~fipsstat+year)
iplot(laga_sa,ref.line = -1)

test <- crime_data %>% filter(treat_date!=1977)

lrob_sa <- feols(lrob ~ sunab(treat_date, year)|fipsstat + year, data = 
                   crime_data %>% filter(treat_date!=1977),
                 vcov = ~fipsstat+year)
iplot(lrob_sa,ref.line = -1)

lpro_sa <- feols(lpro ~ sunab(treat_date, year)|fipsstat + year, data = crime_data, 
                 vcov = ~fipsstat+year)
iplot(lpro_sa,ref.line = -1)

lbur_sa <- feols(lbur ~ sunab(treat_date, year)|fipsstat + year, data = crime_data, 
                 vcov = ~fipsstat+year)
iplot(lbur_sa,ref.line = -1)

laut_sa <- feols(laut ~ sunab(treat_date, year)|fipsstat + year, data = crime_data, 
                 vcov = ~fipsstat+year)
iplot(laut_sa,ref.line = -1)



#twfe table
esttex(twfe_lvio,twfe_lmur,twfe_lrap,twfe_laga,twfe_lrob,twfe_lpro,twfe_lbur,twfe_llar,twfe_laut)

etable(twfe_lvio,twfe_lmur,twfe_lrap,twfe_laga,twfe_lrob,twfe_lpro,twfe_lbur,twfe_llar,twfe_laut,tex = TRUE)
