# INSTRUCTIONS

This entire Repository has been cloned from amandovi/NEP-QA-WQ to USEPA/NEP-Water-Quality as of January 17, 2025. If you do not have access to that repository, please email Steve Pacella (pacella.stephen@epa.gov) or Andrew Mandovi (mandovi.andrew@epa.gov). 

The purpose of this repository is to store shared R code for working with data from National Estuary Program (NEP) monitoring sites to: 

I. QA/QC-ing raw data for analysis for the following NEPs (4): Barnegat Bay, Casco Bay, Delaware Inland Bays, and Pensacola Bay

IIa. Creating a new version of the data_list (qa_data_list) which contains all QA flags performed in step I, and harmonizing all QA flags for all variables into a common 'flags' column. 

IIb. Filtering qa_data_list based on the  

III. (IN FUTURE) Performing analysis on the data 

IV. (IN FUTURE) Creating visualizations of data for communication and publication of results

--------------------------------------------------------------------------------------
**I. QA/QC-ing raw data for analysis**, steps:
1. This requires the following files to be downloaded to your local machine:
 - qaqc_NEP_main.R
 - qaqc_run_all.R
 - qaqc_NEP_Barnegat.R
 - qaqc_NEP_Casco.R
 - qaqc_NEP_Delaware.R
 - qaqc_NEP_Pensacola.R
 - (and any corresponding additional NEP files to QA, but as of 6/23/25, these are the only 4)
2. Then, the user must ensure that the thresholds for each NEP file are correct, . 
3. Once R is opened, the user should open the **'qaqc_run_all.R'** script and customize **row 39**:
   - this is where the user sets their working directory to the local filepath where the R scripts were downloaded (NOT where the data is on the O:drive)
4. Finally, the user can execute all of the scripts by either:
   - Running the following line of code in the R console: **source('qaqc_run_all.R')**
   - **Or** running the full qaqc_run_all.R script by pressing **ctrl-shift-enter**
5. The user will be prompted on save preferences (local and O:drive) for the output dataframe .Rdata

**II. Creating qa_data_list and pass_data_list**:
1. Run qaqc_NEP_filter_pass.R script (must also be downloaded onto machine)
2. This will create the R objects qa_data_list and pass_data_list which must be saved for future use

-----------------------------------------------------------------------------------
Naming conventions:
**.Rdata files:**
- data_list: the raw data, organized into a list of data frames, each data frame corresponding to an NEP (e.g. data_list$Barnegat for Barnegat Bay's data)
- qa_data_list: data_list post-QA/QC flagging and having a harmonized "flags" 
- pass_data_list: qa_data_list filtered for passing "flags" (i.e. where flags == 1)

The **PREFIX** of a R script file dictates the file's category:
- **qaqc_**: scripts containing R code for performing QA/QC and filtering of data
- **calc_**: scripts containing R code for running analysis on the data. This includes performing carbonate calculations, calculating uncertainties, or other statistical calculations.
- **plot_**: scripts containing R code for plotting visualizations of data
- **test_**: scripts written  for testing, practice, or reference material

