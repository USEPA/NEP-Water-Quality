## Master script for SF ###

# Set file path where all SF-related scripts are downloaded to:
SF_R_path = 'C:/Users/Amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/R/SF subfolder'
setwd(SF_R_path)

source('qaqc_NEP_main.R')
source('qaqc_NEP_SanFrancisco.R')

# spike "group" adjustment to spike test: flagging points between two spikes:
source('qaqc_sf_spikegroup.R')
# perform manual QC and save re-merged file:
source('sfbay_manual_qaqc.R')

#Export data for review in matlab
# source('sf_nep_cma_export.R')

# # Save data to O:Drive with unique timestamp: 
# timestamp <- format(Sys.time(), "%Y%m%d-%H%M")
# save(nep_unfiltered_data,file=paste0("O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/nep_unfiltered_data_",timestamp,".Rdata"))
# save(nep_filtered_data,file=paste0("O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/nep_filtered_data_",timestamp,".Rdata"))