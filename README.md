![Status](https://img.shields.io/badge/Study_status-For_publication-blue) 

> [!IMPORTANT]
> This is a mirrored repository for publication purposes.
> For new releases, please request access to https://github.com/UMC-Utrecht-RWE/ConcePTION-INSIGHT-Level3

<!-- PROJECT LOGO -->
<br />
<p align="left">
  <a href="https://github.com/vjolahoxhaj/Level-3-checks">
    <img src="images/conception_logo.png" alt="Logo" width="250" height="60">
  </a>
  </p>
  
 <h3 align="center">Level 3 checks</h3>
 <p align="center"> R scripts to produce high-level characterization data to benchmark across DAPs and with external resources. </p>
 
<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#level-3-checks">Level 3 checks</a>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
        <li><a href="#uploading">Uploading results to the online research environment</a></li>
        <li><a href="#links">Data characterization study links</a></li> 
        <li><a href="#version">Current version</a></li>
      </ul>
    </li>
    <li><a href="#license">License</a></li>
    <li><a href="#about-the-project">About The Project</a></li>
    <li><a href="#funder">Funder</a></li>
     <li><a href="#citation">Citation</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>

<!-- Level 3 checks -->
## Level 3 checks

**Aims of Level 3 quality checks:**      
**1.**	To perform high-level data characterization of the [ConcePTION CDM]((https://docs.google.com/spreadsheets/d/1hc-TBOfEzRBthGP78ZWIa13C0RdhU7bK/edit#gid=413205035)) instance for each DAP and benchmark across DAPs and with external resources.     
***a.*** Assessing medication use in study population.    
***b.*** Assessing vaccine exposure in study population.   
***c.*** Calculation of incidence rates of events during study period.                   
***d.*** Assessing outcomes in relation to drug exposure for signal generation and signal evaluation.     
***e.*** Assessing lifestyle factors in study population. 
***f.*** Assessing pregnancy status in study population.
***g.*** Assessing sub-populations (or populations of interest) within study population.

***Level 3 checks will quantify population and person time in each data source for the source and study population as a whole as well as for subpopulations of interest. Examples of this type of check include: counts of codes extracted to identify each event and exposure of interest, counts of medication prescription and vaccine administrations etc.*** 

**The level 3 checks are divided in 8 major steps:**   

1.	Source and study population.   
2.	Medicines.   
3.	Vaccines.   
4.	Diagnoses.   
5.	Pregnancy.    
6.	Populations of interest.   
7.	Health-seeking behaviour and lifestyle factors.   
8.	EUROCAT indicators (For pregnancy-related studies).   

<!-- GETTING STARTED -->
## Getting Started

Follow the steps below to run Level 3 checks in your data.   

### Prerequisites

R version 4.1.0 (2021-05-18)   

### Installation

1. Download the ZIP folder and extract the contents.   
2. Create a main folder with the name of your project(if you already have done so for Level 1/2 checks skip this step).     
3. Inside the main folder create the folder `Data characterisation`. Put the extracted folder inside.   
4. Inside the main folder create a folder named `CDMInstances`, which will be used to store the .csv files representing the CDM tables(if you already have done so for Level 1/2 checks skip this step).      
5. Inside the `CDMInstances` folder create a folder with the name of your project and inside the latter put all your .csv files(if you already have done so for Level 1/2 checks skip this step).      
6. In the folder `Level_3_checks_to_be_deployed_v1.0`, go to the `p_steps` folder and replace the file called study_parameters.csv with the file study_parameters.csv containing the pre-specified parameters applicable to your DAP. Do not make changes to the name of the file. Instructions on how to fill in this file are given in the file itself, look at the tabs.          
9. Open the to_run.R script. We recommend to run the script to_run.R by chunks, start with study source population until line. If reports are not generated, please go `p_step` folder, open the R script named Main_01_studysourcepopulation.R and run line 65, this will save the temporary file. This error is due to the size of the reports but the study source population is generated, so subsequent reports will be generated. Please continue with following chuncks.
10. Once the to_run.R script is finish, please open the script to_run_results.R. Select all by using ctrl+A(Windows) or cmmd+A(Mac) and run.   
12. After everything is complete, see outputs/result in the `g_ouputs` folder.

***Folder structure***

Main folder

 * [CDMInstances](./CDMInstances)
   * [Project_name](./CDMInstances/Project_name)
     * [files.csv](./CDMInstances/Project_name/files.csv)
     
 * [Data characterisation](./Data_characterisation)
   * [Level_1_checks_to_be_deployed_v5.2](./Data_characterisation/Level_1_checks_to_be_deployed_v5.2)
   * [Level_2_checks_to_be_deployed2.0](./Data_characterisation/Level_2_checks_to_be_deployed2.0)
   * [Level_3_to_be_deployed1.0](./Data_characterisation/Level_3_to_be_deployed1.0)

<!-- UPLOADING -->
### Uploading results to the online research environment

**Uploading to anDREa**
1.	In a web browser, Go To: mydre.org.    
2.	Click on 'Click here to login'. Pick an account and enter password.   
3.	Click on Workspaces in upper left and then double click on the project workspace.    
4.	Click on Files tab at top.    
5.	Double click on 'inbox' folder.    
6.	Click on 'Level3'.     
7.	Create a folder by clicking on the folder icon with + on it.         
8.	Click on the folder you created.    
9.	Click on cloud icon to upload files.    
10.	Click on select and upload.    
11.	Open the `ForDashboard` folder which is located inside `Level_3_to_be_deployed1.0/g_output/`. Hold down control and select all files within your prepared folder (can only do one folder at a time).    
13.	Click on open.    
14.	When it asks to confirm: "Would like to upload the inbox?" select 'OK'.     
15.	Note: It may take many minutes for your upload to complete. You should receive an email once they are uploaded.    
16.	If you find that your files are not in the corresponding level directory, check if the files are in the inbox and move them to the corresponding level directory.     

<!-- LINKS -->
### Data characterization study links   

[Level 1 checks](https://github.com/UMC-Utrecht-RWE/INSIGHT-Level1): Checking the integrity of the ETL procedure.     
[Level 2 checks](https://github.com/UMC-Utrecht-RWE/INSIGHT-Level2): Checking the logical relationship of the CDM tables.    
[Level 3 checks](https://github.com/UMC-Utrecht-RWE/INSIGHT-Level3): Benchamrking across DAPs and external sources.     

<!-- VERSION -->
### Current version

The current version of the script is 2.0. Released on July 26th, 2023. 

<!-- LICENSE -->
## License
[![License](https://img.shields.io/badge/License-BSD_2--Clause-orange.svg)](https://opensource.org/licenses/BSD-2-Clause)

<!-- ABOUT THE PROJECT -->
## About the project
[ConcePTION](https://www.imi-conception.eu) aims to build an ecosystem that can use Real World Data (RWD) to generate Real World Evidence (RWE) that may be used for clinical and regulatory decision making, closing the big information gap of medication safety in pregnancy. As part of WP7, level checks were design to assess the quality of the data supporting RWE. Level checks described here has been successfully implemented in VAC4EU, EMA-tendered risk minimization studies, COVID vaccines effectiveness study, Post-Authorization Safety Studies, and CONSIGN. For details, please find the scientific pre-print article on INSIGHT [here](https://www.medrxiv.org/content/10.1101/2023.10.30.23297753v1)

<!-- FUNDER -->
## Funder
The ConcePTION project has received funding from the Innovative Medicines Initiative 2 Joint Undertaking under grant agreement No 821520. This Joint Undertaking receives support from the European Union’s Horizon 2020 research and innovation programme and EFPIA

<!-- CITATION -->
## Citation
[![DOI](https://zenodo.org/badge/708767063.svg)](https://zenodo.org/doi/10.5281/zenodo.10035170)

<!-- CONTACT -->
## Contact

Vjola Hoxhaj - v.hoxhaj@umcutrecht.nl     
Roel Elbers - R.J.H.Elbers@umcutrecht.nl       
Ema Alsina - palsinaaer@gmail.com  

Project Link: [https://github.com/UMC-Utrecht-RWE/INSIGHT-Level3](https://github.com/UMC-Utrecht-RWE/INSIGHT-Level3)

