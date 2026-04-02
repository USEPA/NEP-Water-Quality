# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Mar 5, 2025
# Last updated: Apr 10, 2025
# -------------------------------------------------------------------------------
# From this script, the user may run the entire QA-QC process for each NEP included here which has a file within the same directory
#
# ! LOOK HERE!!!!!! To run the QA process, read all commented sections
# ! 
# ! PRIOR TO RUNNING: The user must ensure that:
# ! 1. All required files are downloaded and saved into the same local folder 
# !   - Required files: this file (qaqc_run_all.R), qaqc_NEP_main.R, and every qaqc_NEP_xxx.R
# ! 2. Have setwd(local_R_path) correctly assigned to local folder path (lines 39-40)
# ! 3. Each qaqc_NEP_xxx.R file:
# !   3a. Has had QA thresholds entered for that NEP (they may be dummy/old values)
# !   3b. Has the DATE and NAME OF LAST UPDATE filled in for future users
# ! (Note: User will be prompted on saving preferences prior to QA script running)
# !
# ---------------------------------------------------
# This script:
# 1. Loads in data defined by the below data_path
# 2. Prompts user for output save preferences
# 3. Runs the following scripts:
# > qaqc_NEP_main.R 
#     a. Loads in necessary packages
#     b. Loads in data to be QA'd
#     c. Defines necessary functions to be used
# > qaqc_NEP_Barnegat/Casco/Pensacola.R
#     a. Enters user-defined thresholds for each NEP
#     b. Runs QA on specific NEPs 
# --------------------------------------------------
start_time = Sys.time()
cat('Beginning script... \n Loading data from O:drive...\n')
#### USER PROMPTS:
# Prompt to determine if data needs to be loaded in:
data_path = 'O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/4. Finalized Data from NEPs/'
load_data_option = readline(prompt='Load in Data (data_list) from O:Drive? (y/n). Respond y if data is not yet loaded in, or if using an older version of data_list: ')
if (interactive()) {
  if (tolower(load_data_option) %in% c('y','yes')) {
    load(paste0(data_path,'data_list.Rdata'))
  }
}
# Prompts to determine if saving locally / O:drive
save_Odrive_option = readline(prompt='Save QAd NEP Data for each NEP to O:drive (O:/.../NEP Acidification Impacts and WQS/Data/4. Finalized Data from NEPs/) as .Rdata? (y/n): ')
save_local_option = readline(prompt='Save QAd NEP Data for each NEP locally (to where you setwd() to in lines 39-40)? (y/n): ')

# Prompt to ask user if they want printed progress statements throughout the process:
progress_print_option = readline(prompt='Would you like timestamped progress statements in R Console through the process for troubleshooting? (y/n): ')

#
# Set local path to location of folder with saved scripts (including this one)
local_R_path = 'C:/Users/amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/R/'
setwd(local_R_path)
# # # # # # 

# create initial qa_data_list from data_list - will be added with QA'd NEP data (flags added, no filtering)
qa_data_list = data_list

# Begin QA Process:
cat('Starting QA Process... Loading main QA .R script... \n')
# start_time = Sys.time()
source('qaqc_NEP_main.R')

# Barnegat:
source('qaqc_NEP_Barnegat.R')
cat('Barnegat Bay QA process complete. \n')

# Casco:
source('qaqc_NEP_Casco.R')
cat('Casco Bay QA process complete. \n')

# Pensacola:
source('qaqc_NEP_Pensacola.R')
cat('Pensacola Bay QA process complete. \n')

# Delaware:
source('qaqc_NEP_Delaware.R')
cat('Delaware Inland Bays QA process complete. \n')

# San Francisco:
source('sf_master.R')
cat('San Francisco Bay QA Process complete. \n')

############################ Save Data to O:Drive ################################
timestamp <- format(Sys.time(), "%Y%m%d-%H%M")
save(nep_unfiltered_data,file=paste0("O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/nep_unfiltered_data_",timestamp,".Rdata"))
save(nep_filtered_data,file=paste0("O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/5. Revised Data June 2025/nep_filtered_data_",timestamp,".Rdata"))
##################################################################################

end_time = Sys.time()
time_taken = end_time - start_time
cat('*~*~* All QA Processes completed! ^_^ *~*~* \n Completion time:',round(time_taken,1),'min. \n')
cat('Your datasets: \n data_list_qa: the full dataset, post-QA performed here (includes all NEP data, e.g. data_list_qa$Barnegat or data_list_qa$Tillamook)  \n qa_barnegat, qa_casco, and qa_pensacola: the individual data.frames for the NEPs which underwent QA here ')
