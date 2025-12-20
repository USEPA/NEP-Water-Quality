#### Written by Andrew Mandovi (EPA ORISE)

# This script is written to perform Carbonate System calculations on National 
# Estuary Program (NEP) data. The script is custom specified for each NEP's data, 
# due to differences in the 2/4 required parameters.

library(seacarb)
library(dplyr)
library(cmocean)
library(ggExtra)
library(ggplot2)


Odrive_data_path = 'O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/'
local_data_path = 'C:/Users/amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/NEP_Monitoring_Project/Data/'
docs_folder = 'C:/Users/amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/'
figure_path = 'C:/Users/Amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/NEP_Monitoring_Project_AWM/Figures/seacarb/'

######################################################################################

#                             LOADING DATA

load(paste0(Odrive_data_path,'nep_unfiltered_data.Rdata'))
load(paste0(Odrive_data_path,'nep_filtered_data.Rdata'))

# load specific NEPs into their own simpler (shortened names) data frames:
df_til = nep_filtered_data$Tillamook
df_cas = nep_filtered_data$Cascobay

#### Tillamook: ####

# Create Alkalinity from alk-sal regression from Pacella et al 2024 (https://pmc.ncbi.nlm.nih.gov/articles/PMC11462966/)
# Error from paper: RMSE = 51 umol/kg
df_til = df_til %>% 
  mutate(alk_umolkg = 55.2*sal_ppt + 452) # regression slope in paper above
# filter for rows where sal & temp are not-NA
df_til = df_til %>% 
  filter(!is.na(sal_ppt) & !is.na(temp_c))

# Calculate tillamook carbonate system: pH + Alkalinity (derived from alk-sal regression in Pacella 2024)
# Units for carb() function:
# T - Celsius
# S - PSU or PPT
# P - bar
# Alk - mol/kg
# DIC - mol/kg
# pCO2 - uatm
carb_til = carb(flag=8,var1=df_til$ph_T,var2=df_til$alk_umolkg/1E6,df_til$sal_ppt,df_til$temp_c,P=df_til$depth_m/10)
# Pull specific columns to add to carbonate
selected_cols_til = carb_til %>% 
  select(OmegaAragonite,pCO2,DIC) #columns to pull
# Merge data with selected carbonate output:
df_til = bind_cols(df_til,selected_cols_til) %>% 
  rename(OmegaAr=OmegaAragonite, dic_molkg = DIC, pco2_uatm_fromcarb = pCO2) # rename columns

# calculate errors() for Tillamook Bay
# pH error: 0.1
ph_err = 0.1
# Alkalinity error: 51 umol/kg from Pacella et al 2024 (https://pmc.ncbi.nlm.nih.gov/articles/PMC11462966/)
alk_err = 51/1E6
til_err = errors(flag=8,var1=df_til$ph_T,var2=df_til$alk_umolkg/1E6,df_til$sal_ppt,df_til$temp_c,P=df_til$depth_m/10,evar1=ph_err,evar2=alk_err)
selected_err_cols = tilla_err %>% 
  select(OmegaAragonite,pCO2,DIC) %>% 
  rename(OmegaAr_err=OmegaAragonite, pco2_err=pCO2, dic_err=DIC)
df_til = bind_cols(df_til, selected_err_cols)

# pH and DO thresholds for plotting:
tilla_ph_std = 6.5 # referenced in Steve's paper https://pmc.ncbi.nlm.nih.gov/articles/PMC11462966/
tilla_do_std = 6.5 # Tomasetti and Gobler 2020 - https://doi.org/10.1126/science.aba4896
df_til = df_til %>% 
  mutate(
    pH_std = case_when(ph_T >= tilla_ph_std ~ 'pass', TRUE ~ 'fail'),
    DO_std = case_when(do_mgl >= tilla_do_std ~ 'pass', TRUE ~ 'fail')
  )

#### Casco Bay: ####

####  Casco Bay: pH + Alk ####
df_cas = df_cas %>% 
  filter(!is.na(sal_ppt) & !is.na(temp_c))
carb_cas = carb(flag=8,var1=df_cas$ph_T,var2=df_cas$alk_umolkg/1E6,df_cas$sal_ppt,df_cas$temp_c,P=df_cas$depth_m/10)

# Merge results of carb() to casco data 
# !! REQUIRES EQUAL SIZED DATA & carb() OUTPUT:
selected_cols = carb_cas %>% 
  select(OmegaAragonite,pCO2,DIC) # name columns which we want to pull into 
df_cas = bind_cols(df_cas,selected_cols) %>%  # merge together.. if ran more than once -> must re-define df
  rename(OmegaAr=OmegaAragonite, dic_molkg = DIC, pco2_ppm = pCO2) # rename into new column names


# calculate errors() for Casco: 
# choose 0.1 uncertainty for pH (for YSI, +/- 0.02 for SeapHOx)
# look at equation and see if they report a RMSE
# default temp/sal uncertainty of 0.01 degC/PSU

casco_err = errors(flag=8,var1=df_cas$ph_T,var2=df_cas$alk_umolkg/1E6,df_cas$sal_ppt,df_cas$temp_c,P=df_cas$depth_m/10,evar1=0.1,evar2=27.3/1E6)
selected_err_cols = casco_err %>% 
  select(OmegaAragonite,pCO2,DIC) %>% 
  rename(OmegaAr_err=OmegaAragonite, pCO2_err=pCO2, DIC_err=DIC)
df_cas = bind_cols(df_cas, selected_err_cols)

# pH and DO thresholds for plotting:
casco_ph_std = 7.7
casco_do_std = 7
df_cas = df_cas %>% 
  mutate(
    pH_std = case_when(ph_T >= casco_ph_std ~ 'pass', TRUE ~ 'fail'),
    DO_std = case_when(do_mgl >= casco_do_std ~ 'pass', TRUE ~ 'fail')
  )




#### Plotting: ####

# Scatter: Omega vs pH (Sal-colored)
ggplot(df_til, aes(x=pH,y=OmegaAr,color=Salinity_YSI_QA_all_filter))+
  geom_point(alpha=0.1)+
  geom_hline(yintercept=1.4, linetype='dashed',color='purple',linewidth=1)+ # horizontal line for Omega = 1.4
  scale_color_cmocean(name='haline')+
  theme_minimal()

# Time-series: Omega over time (Sal-colored)
ggplot(df_til, aes(x=Timestamp_PST,y=OmegaAr,color=Salinity_YSI_QA_all_filter))+
  geom_point(alpha=0.1)+
  geom_hline(yintercept=1.4, linetype='dashed',color='purple',linewidth=1)+ # horizontal line for Omega = 1.4
  scale_color_cmocean(name='haline')+
  theme_minimal()

df_til_lowOmega = df_til %>% 
  filter(OmegaAr < 1.4)
# Time-series of low Omega
ggplot(df_til_lowOmega, aes(x=Timestamp_PST,y=OmegaAr,color=Salinity_YSI_QA_all_filter))+
  geom_point(alpha=0.1)+
  scale_color_cmocean(name='haline')+
  theme_minimal()
# Time-series of low Omega + high sal
df_til_lowOmega_highSal = df_til %>% 
  filter(OmegaAr < 1.4 & Salinity_YSI_QA_all_filter > 30)
ggplot(df_til_lowOmega_highSal, aes(x=Timestamp_PST,y=OmegaAr,color=Salinity_YSI_QA_all_filter))+
  geom_point(alpha=0.1)+
  scale_color_cmocean(name='haline')+
  labs(title='Tillamook: Upwelling events?', subtitle='Omega < 1.4, Salinity > 30 PSU')+
  theme_minimal()

# Omega vs Depth
ggplot(df_til, aes(x=OmegaAr, y=Depth, color=Salinity_YSI_QA_all_filter))+
  geom_point(alpha=0.1)+
  scale_color_cmocean(name='haline')+
  scale_y_reverse() # reverse depth on y-axis

# pCO2 vs depth
ggplot(df_til, aes(x=pCO2_uatm,y=Depth, color=Salinity_YSI_QA_all_filter))+
  geom_point(alpha=0.3)+
  scale_color_cmocean(name='haline')+
  scale_y_reverse() # reverse depth on y-axis


##### All below here: Prior to Nov 2025 ####

### ### ### PLOTTING ### ### ####
setwd(figure_path)

# OmegaAr vs pH with error bars - colored by SALINITY
plot = ggplot(df_cas, aes(x=ph.T,y=OmegaAr,color=sal.ppt))+
  geom_point(size=2,alpha=0.5)+
  geom_errorbar(aes(ymin=OmegaAr-OmegaAr_err,ymax=OmegaAr+OmegaAr_err),alpha=0.4)+
  geom_hline(yintercept=1.4, linetype='dashed',color='purple',linewidth=1)+ # horizontal line for Omega = 1.4
  # geom_vline(xintercept=casco_ph_std, linetype='dashed',color='darkorange',linewidth=1)+ # vertical line for pH Standard
  # annotate('text',x=casco_ph_std+0.05, y=4.5,label='pH Threshold',hjust=0,vjust=1,angle=0,size=6,color='darkorange')+
  ylim(0,5)+
  xlim(6.5,8.5)+
  annotate('text',x=6.7,y=1.6,label=expression(~Omega~'Ar Threshold'),hjust=0,vjust=1,angle=0,size=6,color='purple')+
  scale_color_cmocean(name='haline')+
  labs(title=expression('Casco Bay'~Omega~'Ar'),x='pH',y=expression(Omega~'Ar +/- Error'),color='Salinity')+
  theme_minimal()+
  theme(plot.title=element_text(size=22,face='bold'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(plot.margin=unit(c(1,1,1,1),'mm'))
plot = ggMarginal(plot, type='densigram')
ggsave(plot, file='Omega_pH_Errorbars_Casco_densityMargins_SalColor2.png')
plot = ggplot(df_til, aes(x=ph.T_ysi,y=OmegaAr,color=sal.ppt_ysi))+
  geom_point(size=2,alpha=0.4)+
  geom_errorbar(aes(ymin=OmegaAr-OmegaAr_err,ymax=OmegaAr+OmegaAr_err),alpha=0.3)+
  geom_hline(yintercept=1.4, linetype='dashed',color='purple',linewidth=1)+ # horizontal line for Omega = 1.4
  # geom_vline(xintercept=casco_ph_std, linetype='dashed',color='darkorange',linewidth=1)+ # vertical line for pH Standard
  # annotate('text',x=casco_ph_std+0.05, y=4.5,label='pH Threshold',hjust=0,vjust=1,angle=0,size=6,color='darkorange')+
  ylim(0,5)+
  xlim(6.5,8.5)+
  annotate('text',x=6.7,y=1.6,label=expression(~Omega~'Ar Threshold'),hjust=0,vjust=1,angle=0,size=6,color='purple')+
  scale_color_cmocean(name='haline')+
  labs(title=expression('Tillamook Bay'~Omega~'Ar'),x='pH',y=expression(Omega~'Ar +/- Error'),color='Salinity')+
  theme_minimal()+
  theme(plot.title=element_text(size=22,face='bold'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(plot.margin=unit(c(1,1,1,1),'mm'))
plot = ggMarginal(plot, type='densigram')
ggsave(plot, file='Omega_pH_Errorbars_Tillamook_densityMargins_SalColor2.png')

# OmegaAr vs pH with error bars - colored by DO STANDARD
plot = ggplot(df_cas, aes(x=ph.T, y=OmegaAr, color=DO_std))+
  geom_point(size=2, alpha=0.3)+
  geom_errorbar(aes(ymin=OmegaAr-OmegaAr_err,ymax=OmegaAr+OmegaAr_err),alpha=0.5)+
  geom_hline(yintercept=1.4, linetype='dashed',color='purple',linewidth=1)+ # horizontal line for Omega = 1.4
  geom_vline(xintercept=7.7, linetype='dashed',color='darkorange',linewidth=1)+ # vertical line for pH Standard
  annotate('text',x=casco_ph_std+0.05, y=4.5,label='pH Standard',hjust=0,vjust=1,angle=0,size=6,color='darkorange')+
  annotate('text',x=8.2,y=0.8,label=expression(~Omega~'Ar Threshold'),hjust=0,vjust=1,angle=0,size=6,color='purple')+
  scale_color_manual(values = c('pass' = 'black','fail' = 'red'))+# color scheme for DO standard
  labs(title=expression('Casco Bay'~Omega~'Ar'),x='pH',y=expression(Omega~'Ar +/- Error'),color=paste('DO Standard =',casco_do_std))+
  theme_minimal()+
  theme(plot.title=element_text(size=22,face='bold'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))
plot = ggMarginal(plot, type='densigram')
ggsave(plot, file='Omega_pH_Ebars_Casco_densitymargins_DOstdColor.png')

# pH data, color-coding by fail (Omega, DO, pH) - making figures similar to 'Sample Analysis: Tillamook' figures
df_cas = df_cas %>% 
  mutate(all_std = case_when(
    OmegaAr < 1.4 ~ 'omega_fail', # fails omega --> red
    DO_std == 'fail' ~ 'do_fail', # fails DO --> blue
    pH_std == 'fail' ~ 'ph_fail', # fails pH --> purple
    TRUE ~ 'pass' # passes omega and DO --> black
    ))
df_til = df_til %>% 
  mutate(all_std = case_when(
    OmegaAr < 1.4 ~ 'omega_fail', # fails omega --> red
    DO_std == 'fail' ~ 'do_fail', # fails DO --> blue
    pH_std == 'fail' ~ 'ph_fail', # fails pH --> purple
    TRUE ~ 'pass' # passes omega and DO --> black
  ))

# Scatter Plots: pH vs Time and pH vs Salinity for Casco and Tillamook Bays
# pH Time Series - Tillamook                                                                ***!!!!
plot = ggplot(df_til, aes(x=datetime.pst,y=ph.T_ysi,color=all_std))+
  geom_point(size=2,alpha=0.4)+
  # geom_hline(yintercept=casco_ph_std, linetype='dashed',color='darkorange',linewidth=1)+ # horizontal line for pH Standard
  scale_color_manual(values = c('omega_fail'='red','do_fail'='blue','ph_fail'='green','pass'='black'))+
  labs(title='Tillamook Bay pH Timeseries', y='pH',x='Date')+
  theme_light()+
  theme(plot.title=element_text(size=22,face='bold'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.position='none')
ggsave(plot, file='pH_time_Tillamook_OmegaFailColor3.png')

# Omega Time Series - Tillamook                                                                  ***!!!!
plot = ggplot(df_til, aes(x=datetime.pst,y=OmegaAr,color=all_std))+
  geom_point(size=2,alpha=0.3)+
  # geom_errorbar(aes(ymin=OmegaAr-OmegaAr_err,ymax=OmegaAr+OmegaAr_err),alpha=0.3)+
  # geom_hline(yintercept=casco_ph_std, linetype='dashed',color='darkorange',linewidth=1)+ # vertical line for pH Standard
  scale_color_manual(values = c('omega_fail'='red','do_fail'='blue','ph_fail'='green','pass'='black'))+
  labs(title='Tillamook Bay Omega Timeseries', y=expression(~Omega~'Ar'),x='Date')+
  theme_bw()+
  theme(plot.title=element_text(size=22,face='bold'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.position='none')
ggsave(plot, file='Omega_time_Tillamook_FailColor_noEbar3.png')

# pH Time series - Casco
plot = ggplot(df_cas, aes(x=datetime.est,y=ph.T,color=all_std))+
  geom_point(size=2,alpha=0.3)+
  # geom_hline(yintercept=casco_ph_std, linetype='dashed',color='darkorange',linewidth=1)+ # horizontal line for pH Standard
  scale_color_manual(values = c('omega_fail'='red','do_fail'='blue','ph_fail'='green','pass'='black'))+
  labs(title='Casco Bay pH Timeseries', y='pH',x='Date')+
  theme_light()+
  theme(plot.title=element_text(size=22,face='bold'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.position='none')
ggsave(plot, file='pH_time_Casco_OmegaFailColor.png')

# pH vs Salinity - Tillamook
plot = ggplot(df_til, aes(x=sal.ppt_ysi, y=ph.T_ysi, color=all_std))+
  geom_point(size=2,alpha=0.3)+
  scale_color_manual(values = c('omega_fail'='red','do_fail'='blue','ph_fail'='green','pass'='black'))+
  labs(title='Tillamook Bay pH vs Salinity', y='pH', x='Salinity (PSU)')+
  theme_light()+
  xlim(0,35)+
  ylim(6.5,8.5)+
  theme(plot.title=element_text(size=22,face='bold'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.position='none')
ggsave(plot, file='pH_Sal,Tillamook_FailColor2.png')

# pH vs Salinity - Casco
plot = ggplot(df_cas, aes(x=sal.ppt, y=ph.T, color=all_std))+
  geom_point(size=2,alpha=0.3)+
  scale_color_manual(values = c('omega_fail'='red','do_fail'='blue','ph_fail'='green','pass'='black'))+
  labs(title='Casco Bay pH vs Salinity', y='pH', x='Salinity (PSU)')+
  theme_light()+
  xlim(0,35)+
  ylim(6.5,8.5)+
  theme(plot.title=element_text(size=22,face='bold'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.position='none')
ggsave(plot, file='pH_Sal,Casco_FailColor2.png')



# Omega Time Series - Casco
plot = ggplot(df_cas, aes(x=datetime.est,y=OmegaAr,color=all_std))+
  geom_point(size=2,alpha=0.3)+
  # geom_errorbar(aes(ymin=OmegaAr-OmegaAr_err,ymax=OmegaAr+OmegaAr_err),alpha=0.3)+
  # geom_hline(yintercept=casco_ph_std, linetype='dashed',color='darkorange',linewidth=1)+ # vertical line for pH Standard
  scale_color_manual(values = c('omega_fail'='red','do_fail'='blue','ph_fail'='green','pass'='black'))+
  labs(title='Casco Bay Omega Timeseries', y=expression(~Omega~'Ar'),x='Date')+
  theme_light()+
  theme(plot.title=element_text(size=22,face='bold'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.position='none')
ggsave(plot, file='Omega_time_Casco_FailColor_noEbar.png')


# Omega vs Salinity - Casco
plot = ggplot(df_cas, aes(x=sal.ppt, y=OmegaAr, color=all_std))+
  geom_point(size=2,alpha=0.3)+
  # geom_errorbar(aes(ymin=OmegaAr-OmegaAr_err,ymax=OmegaAr+OmegaAr_err),alpha=0.3)+
  scale_color_manual(values = c('omega_fail'='red','do_fail'='blue','ph_fail'='green','pass'='black'))+
  labs(title='Casco Bay Omega vs Salinity', y=expression(~Omega~'Ar'), x='Salinity (PSU)')+
  theme_light()+
  xlim(0,35)+
  ylim(0,5)+
  theme(plot.title=element_text(size=22,face='bold'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.position='none')
ggsave(plot, file='Omega_Sal,Casco_FailColor_noEbar2.png')


# Omega vs Salinity - Tillamook
plot = ggplot(df_til, aes(x=sal.ppt_ysi, y=OmegaAr, color=all_std))+
  geom_point(size=2,alpha=0.3)+
  # geom_errorbar(aes(ymin=OmegaAr-OmegaAr_err,ymax=OmegaAr+OmegaAr_err),alpha=0.3)+
  scale_color_manual(values = c('omega_fail'='red','do_fail'='blue','ph_fail'='green','pass'='black'))+
  labs(title='Tillamook Bay Omega vs Salinity', y=expression(~Omega~'Ar'), x='Salinity (PSU)')+
  theme_light()+
  xlim(0,35)+
  ylim(0,5)+
  theme(plot.title=element_text(size=22,face='bold'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.position='none')
ggsave(plot, file='Omega_Sal,Tillamook_FailColor_noEbar2.png')
  

# Omega vs pH, with error bars for Omega, plus density axes
plot = ggplot(df_cas, aes(x=sal.ppt,y=OmegaAr, color=ph.T))+
  geom_point(size=2, alpha=0.7)+
  geom_errorbar(aes(ymin=OmegaAr-OmegaAr_err,ymax=OmegaAr+OmegaAr_err),alpha=0.2)+
  geom_hline(yintercept=1, linetype='dashed',color='black',size=1)+ # horizontal line for Omega = 1
  labs(title=expression('Casco Bay'~Omega~'Ar'),x='Salinity (PSU)',y=expression(Omega~'Ar +/- Error'),color='pH')
plot = ggMarginal(plot, type='densigram') # add histograms to the x-y margins to show data density
ggsave(plot, file='Omega_Sal_Errorbars_Casco_densityMargins.png')

plot(df_cas$ph.T,df_cas$OmegaAr_err)

plot = ggplot(df_cas, aes(x=sal.ppt,y=OmegaAr_err, color=ph.T))+
  geom_point()+
  scale_color_cmocean(name='thermal',direction=-1)
ggsave(plot, file='OmegaError_Sal_Casco.png')

plot = ggplot(df_cas, aes(x=ph.T, y=OmegaAr_err, color=sal.ppt))+
  geom_point()+
  scale_color_cmocean(name='ice',direction=-1)
ggsave(plot, file='OmegaError_pH_Casco_icecolorscale.png')

ggplot(df_cas, aes(x=OmegaAr_err,y=site.code))+
  geom_density_ridges_gradient(aes(fill=after_stat(x)),scale=1)+ # scale affects how tall the bars can get (>1 means overlapping with other rows)
  scale_fill_cmocean(name='haline')+
  labs(title='Title')+
  theme(legend.position = 'none')
 
##### HYPOTHETICAL (NOT ACTUAL DATA!!!) Seaphox being used (0.02 pH uncertainty) instead of YSI (0.1 uncertainty) ####
casco_err_seaphox = errors(flag=8,var1=df$ph.T,var2=df$alk.umolkg/1E6,df$sal.ppt,df$temp.c,P=df$depth.m/10,evar1=0.02,evar2=27.3/1E6) %>% 
  select(OmegaAragonite,pCO2,DIC) %>% 
  rename(OmegaAr_err_seaphox=OmegaAragonite, pCO2_err_seaphox=pCO2, DIC_err_seaphox=DIC)
df_cas = bind_cols(df_cas,casco_err_seaphox)

# Omega vs pH, with error bars for Omega, plus density axes
plot = ggplot(df_cas, aes(x=sal.ppt,y=OmegaAr, color=ph.T))+
  geom_point(size=2, alpha=0.7)+
  geom_errorbar(aes(ymin=OmegaAr-OmegaAr_err_seaphox,ymax=OmegaAr+OmegaAr_err_seaphox),alpha=0.2)+
  geom_hline(yintercept=1, linetype='dashed',color='black',size=1)+ # horizontal line for Omega = 1
  labs(title=expression('Casco Bay'~Omega~'Ar'),x='Salinity (PSU)',y=expression(Omega~'Ar +/- Error'),color='pH')
plot = ggMarginal(plot, type='densigram') # add histograms to the x-y margins to show data density
ggsave(plot, file='Omega_Sal_Errorbars_Casco_densityMargins_SeaphoxHYPOTHETICAL.png')

# Omega vs pH, with error bars for Omega, plus density axes
plot = ggplot(df_cas, aes(x=ph.T,y=OmegaAr, color=sal.ppt))+
  geom_point(size=2, alpha=0.7)+
  geom_errorbar(aes(ymin=OmegaAr-OmegaAr_err_seaphox,ymax=OmegaAr+OmegaAr_err_seaphox),alpha=0.2)+
  geom_hline(yintercept=1, linetype='dashed',color='black',size=1)+ # horizontal line for Omega = 1
  scale_color_cmocean(name='haline')+
  labs(title=expression('Casco Bay'~Omega~'Ar'),x='pH',y=expression(Omega~'Ar +/- Error'),color='Salinity')
plot = ggMarginal(plot, type='densigram') # add histograms to the x-y margins to show data density
ggsave(plot, file='Omega_pH_Errorbars_Casco_densityMargins_SeaphoxHYPOTHETICAL.png')


# OmegaAr vs pH with error bars
plot = ggplot(df_cas, aes(x=ph.T,y=OmegaAr))+
  geom_hex(bins=50)+
  geom_errorbar(aes(ymin=OmegaAr-OmegaAr_err,ymax=OmegaAr+OmegaAr_err))+
  geom_hline(yintercept=1, linetype='dashed',color='black',size=1)+ # horizontal line for Omega = 1
  scale_color_cmocean(name='haline')+
  labs(title=expression('Casco Bay'~Omega~'Ar'),x='pH',y=expression(Omega~'Ar +/- Error'))



# can calculate uncertainties vs a hypothetical SeapHOx being used

#### PLOTTING ####

# Basic plot of OmegaAr vs pH
ggplot(df, aes(x=ph.T,y=OmegaAragonite,color=sal.ppt))+
  geom_point(alpha=0.6)+
  scale_color_cmocean(name='haline')

# OmegaAr vs pH, colored by where pH is beneath threshold
pH_threshold = 7.8
ggplot(df, aes(x=ph.T,y=OmegaAragonite))+
  geom_point(aes(color=ifelse(ph.T < pH_threshold,'below','above')),alpha=0.3)+
  geom_hline(yintercept=1, linetype='dashed',color='black',size=1)+ # horizontal line for Omega = 1
  scale_color_manual(values=c('below'='red','above'='blue'))+
  labs(title='Casco Bay Omega-Aragonite',x='pH',y=expression(Omega~'Ar'),color='pH Threshold')

# OmegaAr vs pH, colored by sal with vert-line for pH threshold
pH_threshold = 7.8
ggplot(df, aes(x=ph.T,y=OmegaAragonite,color=sal.ppt))+
  geom_point(alpha=0.3)+
  geom_hline(yintercept=1, linetype='dashed',color='black',size=1)+ # horizontal line for Omega = 1
  geom_vline(xintercept=pH_threshold, linetype='dashed',color='red',size=1)+ # vertical line for pH threshold
  # annotate('text',x=min(df$pH),y=1.1,label=expression(Omega~'Ar Threshold'),color='black',hjust=1,vjust=-0.5)+
  # annotate('text',x=pH_threshold+0.1,y=max(df$OmegaAragonite),label='pH Threshold',color='red')+
  scale_color_cmocean(name='haline')+
  labs(title=expression('Casco Bay'~Omega~'Ar'),x='pH',y=expression(Omega~'Ar'),color='Salinity')



#### Coastal Bend: pH + pCO2 ####
df = pass_data_list_revision$Coastalbend 
cbend_carb = carb(flag=21,var1=df$pco2.ppm,var2=df$ph.T,df$sal.ppt,df$temp.c,Patm=df$depth.m/10.3,P=df$depth.m/10)



#### Delaware: pH + regression Alkalinity ####
df_del = pass_data_list_revision$DelawareInland %>% 
  mutate(alk.umolkg = 48.62*sal.ppt + 749.87) %>%  # regression from discrete alkalinity
  filter(!is.na(sal.ppt) & !is.na(temp.c) & !is.na(ph.T) & !is.na(depth.m)) %>% 
  select() # select first 100 rows
df_del = head(df_del,n=100)
del_carb = carb(flag=8,var1=df_del$ph.T,var2=df_del$alk.umolkg/1E6,df_del$sal.ppt,df_del$temp.c,P=df_del$depth.m/10)
# troubleshooting:
problem_rows = c()
for (i in 1:nrow(df_del)) {
  # wrap the function call in tryCatch to catch the error
  result = tryCatch({
    carb(flag = 8,
         var1 = df$ph.T[i],
         var2 = df$alk.umolkg[i]/1E6,
         S = df$sal.ppt[i],
         df$temp.c[i],
         P = df$depth.m[i]/10
         )
  }, error = function(e) {
    # if an error occurs, print the row number and the error message:
    message(paste('Error in row',i,':',conditionMessage(e)))
    return(NA)
  })
  # if function failed, add the row number to the list:
  if (is.na(result)) {
    problem_rows = c(problem_rows,i)
  }
}


