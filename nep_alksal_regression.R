library(ggplot2)
library(readxl)
library(dplyr)
library(tidyverse)

file_path = 'C:/Users/amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/NEP_Monitoring_Project_AWM/Data/Discrete Data - Formatted/'
figure_path = 'C:/Users/amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/NEP_Monitoring_Project_AWM/Figures/Alk Sal Regressions/'

#### # # Testing Ground: # # ####

file_name = list.files(file_path)[1]
last_3_chars = str_sub(file_name,-3,-1)


file_num = 1 # file number within the folder
file_name = list.files(file_path)[file_num]
data = read_excel(paste0(file_path,file_name))
siteName = sub('_.*','',file_name)
# filter out unrealistic data:
filtered_data = data %>% 
  mutate(sal.ppt = as.numeric(sal.ppt), alk.umolkg = as.numeric(alk.umolkg)) %>% 
  filter(sal.ppt >= 0 & alk.umolkg >= 0) 
  # filter(sal.ppt >= 33) %>% # MORRO FILTER
  # mutate(sal.ppt = as.character(sal.ppt), alk.umolkg = as.character(alk.umolkg))
# perform linear regression
model = lm(alk.umolkg ~ sal.ppt, data = filtered_data)
intercept = coef(model)[1]
slope = coef(model)[2]
r_squared = summary(model)$r.squared
predictions = predict(model, newdata = filtered_data)
filtered_data = filtered_data %>% 
  mutate(sal.ppt = as.numeric(sal.ppt),alk.umolkg = as.numeric(alk.umolkg)) # convert back into numeric
rmse = sqrt(mean((filtered_data$alk.umolkg-predictions)^2)) 
# format equation and statistics for display
equation_text = paste0('y = ',round(slope,2),'x + ',round(intercept,2))
r_squared_text = paste0('R-squared = ',round(r_squared,3))
rmse_text = paste0('RMSE = ',round(rmse,2))
annotation_vert=0.65

plot = ggplot(filtered_data, aes(x=sal.ppt, y=alk.umolkg, color = Site))+
  # geom_hex()+
  # scale_fill_viridis_c()+
  geom_point()+
  geom_smooth(method='lm',formula=y ~ x,color='black')+
  labs(title=paste0('Alkalinity-Salinity Regression for ',siteName))+
  annotate('text',x=min(filtered_data$sal.ppt) + (max(filtered_data$sal.ppt)-min(filtered_data$sal.ppt)) * 0.6,
           y=max(filtered_data$alk.umolkg) - (max(filtered_data$alk.umolkg)-min(filtered_data$alk.umolkg)) *annotation_vert,
           label=paste('N = ',nrow(filtered_data)), hjust=0)+
  annotate('text',x=min(filtered_data$sal.ppt) + (max(filtered_data$sal.ppt)-min(filtered_data$sal.ppt)) * 0.6,
           y=max(filtered_data$alk.umolkg) - (max(filtered_data$alk.umolkg)-min(filtered_data$alk.umolkg)) *(annotation_vert+0.05),
           label=equation_text, hjust=0)+
  annotate('text',x=min(filtered_data$sal.ppt) + (max(filtered_data$sal.ppt)-min(filtered_data$sal.ppt)) * 0.6,
           y=max(filtered_data$alk.umolkg) - (max(filtered_data$alk.umolkg)-min(filtered_data$alk.umolkg)) *(annotation_vert+0.1),
           label=r_squared_text, hjust=0)+
  annotate('text',x=min(filtered_data$sal.ppt) + (max(filtered_data$sal.ppt)-min(filtered_data$sal.ppt)) * 0.6,
           y=max(filtered_data$alk.umolkg) - (max(filtered_data$alk.umolkg)-min(filtered_data$alk.umolkg)) *(annotation_vert+0.15),
           label=rmse_text, hjust=0)+
  theme_minimal()+
  # theme(legend.position = 'bottom',legend.direction = 'horizontal') # putting the legend below the figure
  theme(legend.position = c(0.5,0.05), 
        legend.direction = 'horizontal', 
        legend.box.background = element_rect(color='black',size=0.5),
        legend.title = element_blank())
print(plot)

################# PLOTTING #######################
setwd(figure_path)
# Plotting Functions:
alksal_regression_plot = function(file_path, file_num,  sal_filter = 0, subtitle = NULL, annotation_vert = 0.65) {
  # # # Loop through full list of files:
  # for (file in list.files(file_path)) {
  #   file_name = file
  # }
  
  # Read in File:
  file_name = list.files(file_path)[file_num]
  last_3_chars = str_sub(file_name,-3,-1)
  if (last_3_chars == 'csv') {
    data = read_csv(paste0(file_path,file_name))
  } else {
    data = read_excel(paste0(file_path,file_name))
  }
  siteName = sub('_.*','',file_name)
  
  # Filter data
  filtered_data = data %>% 
    # convert to numeric
    mutate(sal.ppt = as.numeric(sal.ppt), alk.umolkg = as.numeric(alk.umolkg)) %>%
    # filter out all 0s, NAs and negatives
    filter(sal.ppt >= sal_filter & alk.umolkg >= 0) 
  
  # Perform linear regression
  model = lm(alk.umolkg ~ sal.ppt, data = filtered_data)
  intercept = coef(model)[1]
  slope = coef(model)[2]
  r_squared = summary(model)$r.squared
  predictions = predict(model, newdata = filtered_data)
  filtered_data = filtered_data %>% # convert back into numeric
    mutate(sal.ppt = as.numeric(sal.ppt),alk.umolkg = as.numeric(alk.umolkg)) 
  rmse = sqrt(mean((filtered_data$alk.umolkg-predictions)^2)) 
  
  # Format equation and statistics texts for display
  equation_text = paste0('y = ',round(slope,2),'x + ',round(intercept,2))
  r_squared_text = paste0('R-squared = ',round(r_squared,3))
  rmse_text = paste0('RMSE = ',round(rmse,2))
  
  # Create plot
  plot = ggplot(filtered_data, aes(x=sal.ppt, y=alk.umolkg))+
    # geom_hex()+
    # scale_fill_viridis_c()+
    geom_point()+
    geom_smooth(method='lm',color='black')+
    labs(title=paste0('Alkalinity-Salinity Regression for ',siteName),
         subtitle = subtitle)+
    annotate('text',x=min(filtered_data$sal.ppt) + (max(filtered_data$sal.ppt)-min(filtered_data$sal.ppt)) * 0.6,
             y=max(filtered_data$alk.umolkg) - (max(filtered_data$alk.umolkg)-min(filtered_data$alk.umolkg)) *annotation_vert,
             label=paste('N = ',nrow(filtered_data)), hjust=0)+
    annotate('text',x=min(filtered_data$sal.ppt) + (max(filtered_data$sal.ppt)-min(filtered_data$sal.ppt)) * 0.6,
             y=max(filtered_data$alk.umolkg) - (max(filtered_data$alk.umolkg)-min(filtered_data$alk.umolkg)) *(annotation_vert+0.05),
             label=equation_text, hjust=0)+
    annotate('text',x=min(filtered_data$sal.ppt) + (max(filtered_data$sal.ppt)-min(filtered_data$sal.ppt)) * 0.6,
             y=max(filtered_data$alk.umolkg) - (max(filtered_data$alk.umolkg)-min(filtered_data$alk.umolkg)) *(annotation_vert+0.1),
             label=r_squared_text, hjust=0)+
    annotate('text',x=min(filtered_data$sal.ppt) + (max(filtered_data$sal.ppt)-min(filtered_data$sal.ppt)) * 0.6,
             y=max(filtered_data$alk.umolkg) - (max(filtered_data$alk.umolkg)-min(filtered_data$alk.umolkg)) *(annotation_vert+0.15),
             label=rmse_text, hjust=0)+
    theme_minimal()
  # return(filtered_data)
  return(plot)
}
alksal_regression_plot_sitecolor = function(file_path, file_num, sal_filter = 0, subtitle = NULL, annotation_vert = 0.65, legend_off = FALSE, legend_position = c(0.5,0.05)) {
  # # # Loop through full list of files:
  # for (file in list.files(file_path)) {
  #   file_name = file
  # }
  
  # Read in File:
  file_name = list.files(file_path)[file_num]
  last_3_chars = str_sub(file_name,-3,-1)
  if (last_3_chars == 'csv') {
    data = read_csv(paste0(file_path,file_name))
  } else {
    data = read_excel(paste0(file_path,file_name))
  }
  siteName = sub('_.*','',file_name)
  
  # Filter data
  filtered_data = data %>% 
    # convert to numeric
    mutate(sal.ppt = as.numeric(sal.ppt), alk.umolkg = as.numeric(alk.umolkg)) %>%
    # filter out all 0s, NAs and negatives
    filter(sal.ppt >= sal_filter & alk.umolkg >= 0) 
  
  # Perform linear regression
  model = lm(alk.umolkg ~ sal.ppt, data = filtered_data)
  intercept = coef(model)[1]
  slope = coef(model)[2]
  r_squared = summary(model)$r.squared
  predictions = predict(model, newdata = filtered_data)
  filtered_data = filtered_data %>% # convert back into numeric
    mutate(sal.ppt = as.numeric(sal.ppt),alk.umolkg = as.numeric(alk.umolkg)) 
  rmse = sqrt(mean((filtered_data$alk.umolkg-predictions)^2)) 
  
  # Format equation and statistics texts for display
  equation_text = paste0('y = ',round(slope,2),'x + ',round(intercept,2))
  r_squared_text = paste0('R-squared = ',round(r_squared,3))
  rmse_text = paste0('RMSE = ',round(rmse,2))
  
  # Create plot
  plot = ggplot(filtered_data, aes(x=sal.ppt, y=alk.umolkg, color = Site))+
    # geom_hex()+
    # scale_fill_viridis_c()+
    geom_point()+
    geom_smooth(method='lm',color='black')+
    labs(title=paste0('Alkalinity-Salinity Regression for ',siteName),
         subtitle = subtitle)+
    annotate('text',x=min(filtered_data$sal.ppt) + (max(filtered_data$sal.ppt)-min(filtered_data$sal.ppt)) * 0.6,
             y=max(filtered_data$alk.umolkg) - (max(filtered_data$alk.umolkg)-min(filtered_data$alk.umolkg)) *annotation_vert,
             label=paste('N = ',nrow(filtered_data)), hjust=0)+
    annotate('text',x=min(filtered_data$sal.ppt) + (max(filtered_data$sal.ppt)-min(filtered_data$sal.ppt)) * 0.6,
             y=max(filtered_data$alk.umolkg) - (max(filtered_data$alk.umolkg)-min(filtered_data$alk.umolkg)) *(annotation_vert+0.05),
             label=equation_text, hjust=0)+
    annotate('text',x=min(filtered_data$sal.ppt) + (max(filtered_data$sal.ppt)-min(filtered_data$sal.ppt)) * 0.6,
             y=max(filtered_data$alk.umolkg) - (max(filtered_data$alk.umolkg)-min(filtered_data$alk.umolkg)) *(annotation_vert+0.1),
             label=r_squared_text, hjust=0)+
    annotate('text',x=min(filtered_data$sal.ppt) + (max(filtered_data$sal.ppt)-min(filtered_data$sal.ppt)) * 0.6,
             y=max(filtered_data$alk.umolkg) - (max(filtered_data$alk.umolkg)-min(filtered_data$alk.umolkg)) *(annotation_vert+0.15),
             label=rmse_text, hjust=0)+
    theme_minimal()+
    # theme(legend.position = 'bottom',legend.direction = 'horizontal') # putting the legend below the figure
    if (legend_off) {
      theme(legend.position = 'none')
    } else {
      theme(legend.position = legend_position, 
            legend.direction = 'horizontal', 
            legend.box.background = element_rect(color='black',size=0.5),
            legend.title = element_blank())
    }
  # print(plot)
  return(plot)
}

#### Delaware: 
del_plot = alksal_regression_plot(file_path, 1)
# del_data = alksal_regression_plot(file_path, 1)
print(del_plot)
ggsave(del_plot, file='del_alksal.png',width=7,height=7)

del_plot_sitecolor = alksal_regression_plot_sitecolor(file_path, 1)
print(del_plot_sitecolor)
ggsave(del_plot_sitecolor, file='del_alksal_sitecolor.png',width=7,height=7)

#### Morro Bay: 
morro_data = read_xlsx(paste0(file_path,'Morro_full.xlsx'))

morro_plot = alksal_regression_plot(file_path, 2,annotation_vert = 0.1)
print(morro_plot)
ggsave(morro_plot, file='morro_alksal.png',width=7,height=7)

morro_plot_sitecolor = alksal_regression_plot_sitecolor(file_path, 2 ,annotation_vert = 0.1, legend_position = c(0.3,0.15))
print(morro_plot_sitecolor)
ggsave(morro_plot_sitecolor, file='morro_alksal_sitecolor.png',width=7,height=7)

# Morro Bay - nearsites only:
morro_plot_nearsites = alksal_regression_plot(file_path, 3, subtitle='Only BM1, Shore and Channel', annotation_vert = 0.05)
print(morro_plot_nearsites)
ggsave(morro_plot_nearsites, file='morro_alksal_bm1.png',width=7,height=7)

morro_plot_sitecolor_nearsites = alksal_regression_plot_sitecolor(file_path, 3, subtitle='Only BM1, Shore and Channel', annotation_vert = 0.05)
print(morro_plot_sitecolor_nearsites)
ggsave(morro_plot_sitecolor_nearsites, file='morro_alksal_bm1_nochannel_sitecolor.png',width=7,height=7)

morro_plot_ovr33sal = alksal_regression_plot_sitecolor(file_path,3,sal_filter=33,subtitle='Only BM1, Shore, and Channel | >33 Sal', annotation_vert = 0.1)
print(morro_plot_ovr33sal)
ggsave(morro_plot_ovr33sal, file='morro_alksal_over33sal.png',width=7,height=7)

# Morro - Only BM1:
morro_plot_onlyBM1 = alksal_regression_plot(file_path, 12, subtitle='Only BM1', annotation_vert = 0.05)
print(morro_plot_onlyBM1)
ggsave(morro_plot_onlyBM1, file='morro_alksal_BM1only.png',width=7,height=7)

morro_only 



#### Narragansett Bay:
narra_plot = alksal_regression_plot(file_path, 4, annotation_vert = 0.1)
print(narra_plot)
ggsave(narra_plot, file='narra_alksal.png',width=7,height=7)
narra_plot_sitecolor = alksal_regression_plot_sitecolor(file_path, 4, annotation_vert = 0.1)
print(narra_plot_sitecolor)
ggsave(narra_plot_sitecolor, file='narra_alksal_sitecolor.png',width=7,height=7)

# Narragansett - site-specific:
narra_plot_nearsites = alksal_regression_plot(file_path, 5, subtitle='Near Chosen NEP Sites Only', annotation_vert = 0.1)
print(narra_plot_nearsites)
ggsave(narra_plot_nearsites, file='narra_alksal_nearsites.png',width=7,height=7)
narra_plot_nearsites_sitecolor = alksal_regression_plot_sitecolor(file_path, 5, subtitle='Near Chosen NEP Sites Only', annotation_vert = 0.1)
print(narra_plot_nearsites_sitecolor)
ggsave(narra_plot_nearsites_sitecolor, file='narra_alksal_nearsites_sitecolor.png',width=7,height=7)

#### Pensacola Bay:
pensa_plot = alksal_regression_plot(file_path,6)
print(pensa_plot)
ggsave(pensa_plot, file='pensa_alksal.png',width=7, height=7)
pensa_plot_sitecolor = alksal_regression_plot_sitecolor(file_path, 6)
print(pensa_plot_sitecolor)
ggsave(pensa_plot_sitecolor, file='pensa_alksal_sitecolor.png',width=7, height=7)

# site-specific:
pensa_plot_nearsites = alksal_regression_plot(file_path,7,subtitle='Near Chosen NEP Sites Only')
print(pensa_plot_nearsites)
ggsave(pensa_plot_nearsites, file='pensa_alksal_nearsites.png',width=7,height=7)
pensa_plot_nearsites_sitecolor = alksal_regression_plot_sitecolor(file_path,7,subtitle='Near Chosen NEP Sites Only')
print(pensa_plot_nearsites_sitecolor)
ggsave(pensa_plot_nearsites_sitecolor, file='pensa_alksal_nearsites_sitecolor.png',width=7,height=7)


#### Tampa Bay:
tampa_plot = alksal_regression_plot(file_path,8)
print(tampa_plot)
ggsave(tampa_plot, file='tampa_alksal.png',width=7,height=7)
tampa_plot_sitecolor=alksal_regression_plot_sitecolor(file_path,8)
print(tampa_plot_sitecolor)
ggsave(tampa_plot_sitecolor, file='tampa_alksal_sitecolor.png',width=7,height=7)

tampa_plot_nearsites = alksal_regression_plot(file_path,9,subtitle='Excluding Gulf Sites')
print(tampa_plot_nearsites)
ggsave(tampa_plot_nearsites, file='tampa_alksal_bayOnly.png',width=7,height=7)

#### Tillamook Bay:
tilla_plot = alksal_regression_plot(file_path,11)
print(tilla_plot)




################################
################# OLD : ####  
alksal_regression_multisite = function(file_path,title) {
  data = read.csv(file_path)
  data = data[data$SALINITY >= 0 & data$ALKALINITY > 1000,]
  min_salinity = min(data$SALINITY, na.rm=TRUE)
  
  # perform linear regression
  rmse_list = list()
  site_counts = list()
  sites = unique(data$Site)
  
  for (site in sites) {
    site_data = subset(data,Site==site)
    if (nrow(site_data) > 1) {
      model = lm(ALKALINITY ~ SALINITY, data=site_data)
      predictions = predict(model, site_data)
      rmse = sqrt(mean((site_data$ALKALINITY - predictions)^2))
      rmse_list[[site]] = rmse
      site_counts[[site]] = nrow(site_data)
    } else {
      rmse_list[[site]] = NA
      site_counts[[site]] = nrow(site_data)
    }
  }
  # create annotation text dynamically
  # annotation_text = ''
  # for (site in sites) {
  #   if (!is.na(rmse_list[[site]])) {
  #     annotation_text = paste(annotation_text, paste('\n',site,': N =', site_counts[[site]],
  #                                                    ', RMSE =',round(rmse_list[[site]],2),sep=''))
  #   } else {
  #     annotation_text = paste(annotation_text, paste('\n',site,': N =',site_counts[[site]],
  #                                                    ', RMSE = NA',sep=''))
  #   }
    # }
  
  # create scatter plot with regression lines and text annotations:
  plot = ggplot(data, aes(x=SALINITY, y=ALKALINITY, color=Site))+
    geom_point()+
    geom_smooth(method='lm', se=FALSE)+
    ggtitle(title)+
    annotate('text',x=min_salinity,y=Inf,label=annotation_text,
             hjust=-0.1,vjust=1.1,size=4,color='red')+
    theme(plot.title=element_text(hjust=0.5)) # center the title
  return(rmse_list)
}

alksal_regression_multisite('C:/Users/amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/NEP_Monitoring_Project/Data/Raw Data/Narragansett Bay/Alkalinity History.rev.sal.csv','Narragansett Bay')
