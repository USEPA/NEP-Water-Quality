## Master script for SF ###
setwd('C:/Users/spacella/OneDrive - Environmental Protection Agency (EPA)/NEP OA standards analysis')
source('qaqc_NEP_main.R')
source('qaqc_NEP_SanFrancisco.R')
source('qaqc_spikegroup.R')
source('sfbay_manual_qaqc.R')

#Export data for review in matlab
source('sf_nep_cma_export.R')
