---
title: "**p53 Somatic Mutation Spectrum from cBioPortal**"
author: "**Neha Wali**"
date: "**December 31, 2025**"
output: 
  bookdown::html_document2:              
    code_download: true       #allows code to be downloaded from HTML
    toc: true                 #creates table of contents with clickable headings
    toc_float: true           #moves table of contents to top left of HTML
    df_print: kable           #type of table made in HTML
    fig_width: 6              #set size of figures included in HTML
    fig_height: 4
    number_sections: true     #have numbered sections H1 is #, H2 is ##, etc.; default in bookdown
    fig_caption: true         #add caption below figures which show up in HTML  
    keep_md: true             #allows figures to be saved outside of R
bibliography: ["tables/somaticpkgs.bib", "tables/somaticpapers.bib"]
csl: "tables/apa.csl"
link-citations: true
nocite: '@*'
---

$~$

The overall purpose of this project is to [].

$~$

# **Code Setup** {#codesetup}

$~$

This code applies to all code chunks in this document, like ensuring the figures in this HTML will be centered and fullsize, saving all figures to the computer as high-resolution SVG files, and removing comment marks in outputted text.

$~$


``` r
overall_path <- c("")
fig_path <- paste0(overall_path, "figures/")
table_path <- paste0(overall_path, "tables/")
input_path <- paste0(overall_path, "inputs/")
analogous_germline_path <- c("../confirmed-p53-germline-variant-carriers-mut-spectrum-TP53-database_files/tables/")

knitr::opts_chunk$set(fig.align = "center",
                      comment = "",
                      dev = "svg",
                      dpi = 600,
                      out.width = "100%",
                      out.height = "100%",
                      fig.path = fig_path)
```

$~$

# **Libraries Used** {#librariesused}

$~$

This code calls all the packages used in this document's analysis, and sets a seed for reproducibility.

$~$


``` r
#load (and install if needed) pacman and use it to load important packages
if (!require("pacman", quietly = TRUE)) install.packages("pacman"); library(pacman)
pacman::p_load(BiocManager, cBioPortalData, AnVIL, janitor, knitr, devtools, rmarkdown, tidyverse, ggplot2, dplyr, plotly, cowplot, readr, scales, grid, gridExtra, condformat, cbioportalR, pivottabler, reshape2, RColorBrewer, ComplexHeatmap, ggalluvial, gt, gtsummary, webshot2, svglite, ggh4x, webr)

#update all loaded packages
#pacman::p_update() 

#set seed for reproducibility
set.seed(1212)

# create function to show significance stars in gt summary tables 
pvalue_with_stars <- function(x) {
  dplyr::case_when(
    x < 0.001 ~ paste0(style_pvalue(x, digits = 3), " (***)"),
    x < 0.01 ~ paste0(style_pvalue(x, digits = 3), " (**)"),
    x < 0.05 ~ paste0(style_pvalue(x, digits = 3), " (*)"),
    TRUE ~ style_pvalue(x, digits = 3)
  )
}
```

$~$

# **Input Data** {#inputdata}

$~$

[]

$~$


``` r
#find all nonoverlapping studies by going to cbioportal in chrome, hit F12, click curated list of nonredundant studies button, then within F12 console go to network --> fetch/XHR --> fetch entry --> payload --> right-click on second study ids for copy value and paste to Excel and from Excel paste to here (remove precancerous/nl studies in Excel itself so not added here); 237 total nonoverlapping - 5 precan/nl = 232 studies as of 12.23.25
nonoverlapping <- list("acbc_mskcc_2015",
                       "acc_2019",
                       "acc_tcga_pan_can_atlas_2018",
                       "acyc_fmi_2014",
                       "acyc_jhu_2016",
                       "acyc_mda_2015",
                       "acyc_mskcc_2013",
                       "acyc_sanger_2013",
                       "all_phase2_target_2018_pub",
                       "all_stjude_2016",
                       "aml_ohsu_2018",
                       "aml_ohsu_2022", 
                       "aml_target_2018_pub",
                       "ampca_bcm_2016",
                       "angs_painter_2020",
                       "angs_project_painter_2018",
                       "bcc_unige_2016",
                       "bfn_duke_nus_2015",
                       "biliary_tract_summit_2022",
                       "bladder_columbia_msk_2018", 
                       "blca_bcan_hcrn_2022",
                       "blca_bgi",
                       "blca_cornell_2016",
                       "blca_dfarber_mskcc_2014",
                       "blca_mskcc_solit_2012",
                       "blca_mskcc_solit_2014",
                       "blca_tcga_pan_can_atlas_2018",
                       "brain_cptac_2020",
                       "brca_bccrc",
                       "brca_broad",
                       "brca_cptac_2020",
                       "brca_dfci_2020",
                       "brca_dldccc_2022",
                       "brca_fuscc_2020",
                       "brca_hta9_htan_2022",
                       "brca_igr_2015",
                       "brca_mbcproject_wagle_2017",
                       "brca_metabric",
                       "brca_mskcc_2019",
                       "brca_sanger",
                       "brca_smc_2018",
                       "brca_tcga_pan_can_atlas_2018",
                       "ccle_genentech_2014",
                       "ccrcc_dfci_2019",
                       "ccrcc_irc_2014",
                       "ccrcc_utokyo_2013",
                       "cesc_tcga_pan_can_atlas_2018",
                       "chl_sccc_2023",
                       "chol_icgc_2017",
                       "chol_jhu_2013",
                       "chol_nccs_2013",
                       "chol_nus_2012",
                       "chol_tcga_pan_can_atlas_2018",
                       "cll_broad_2015",
                       "cll_broad_2022",
                       "cll_iuopa_2015",
                       "cllsll_icgc_2011",
                       "coad_caseccc_2015",
                       "coad_cptac_2019",
                       "coad_silu_2022", 
                       "coadread_cass_2020",
                       "coadread_dfci_2016",
                       "coadread_genentech",
                       "coadread_mskcc",
                       "coadread_tcga_pan_can_atlas_2018",
                       "crc_nigerian_2020",
                       "crc_orion_2024",
                       "crc_sysucc_2022",
                       "cscc_dfarber_2015",
                       "cscc_hgsc_bcm_2014",
                       "cscc_ucsf_2021",
                       "ctcl_columbia_2015",
                       "desm_broad_2015",
                       "difg_glass_2019",
                       "dlbc_tcga_pan_can_atlas_2018",
                       "dlbcl_dfci_2018",
                       "dlbcl_duke_2017",
                       "egc_tmucih_2015",
                       "es_dfarber_broad_2014",
                       "es_iocurie_2014",
                       "esca_broad",
                       "esca_tcga_pan_can_atlas_2018",
                       "escc_icgc",
                       "escc_ucla_2014",
                       "gbc_shanghai_2014",
                       "gbm_columbia_2019",
                       "gbm_cptac_2021",
                       "gbm_tcga_pan_can_atlas_2018",
                       "glioma_msk_2018",
                       "hcc_clca_2024",
                       "hcc_inserm_fr_2015",
                       "hcc_meric_2021",
                       "hcc_msk_venturaa_2018",
                       "hccihch_pku_2019",
                       "histiocytosis_cobi_msk_2019",
                       "hnsc_broad",
                       "hnsc_jhu",
                       "hnsc_mdanderson_2013",
                       "hnsc_tcga_pan_can_atlas_2018",
                       "ihch_ismms_2015",
                       "ihch_smmu_2014",
                       "kich_tcga_pan_can_atlas_2018",
                       "kirc_bgi",
                       "kirc_tcga_pan_can_atlas_2018",
                       "kirp_tcga_pan_can_atlas_2018",
                       "laml_tcga_pan_can_atlas_2018",
                       "lcll_broad_2013",
                       "lgg_tcga_pan_can_atlas_2018",
                       "lgg_ucsf_2014",
                       "lgsoc_mapk_msk_2022",
                       "liad_inserm_fr_2014",
                       "lihc_amc_prv",
                       "lihc_riken",
                       "lihc_tcga_pan_can_atlas_2018",
                       "luad_broad",
                       "luad_cas_2020",
                       "luad_cptac_2020",
                       "luad_oncosg_2020",
                       "luad_tcga_pan_can_atlas_2018",
                       "luad_tsp",
                       "lung_nci_2022",
                       "lung_smc_2016",
                       "lusc_cptac_2021",
                       "lusc_tcga_pan_can_atlas_2018",
                       "mbl_broad_2012",
                       "mbl_dkfz_2017",
                       "mbl_pcgp",
                       "mbl_sickkids_2016",
                       "mcl_idibips_2013",
                       "mds_iwg_2022",
                       "mds_tokyo_2011",
                       "mel_dfci_2019",
                       "mel_tsam_liang_2017",
                       "mel_ucla_2016",
                       "meso_tcga_pan_can_atlas_2018",
                       "metastatic_solid_tumors_mich_2017",
                       "mixed_allen_2018",
                       "mixed_pipseq_2017",
                       "mixed_selpercatinib_2020",
                       "mm_broad",
                       "mng_utoronto_2021",
                       "mnm_washu_2016",
                       "mpcproject_broad_2021",
                       "mpn_cimr_2013",
                       "mpnst_mskcc",
                       "mrt_bcgsc_2016",
                       "msk_chord_2024",
                       "nbl_amc_2012",
                       "nbl_target_2018_pub",
                       "nbl_ucologne_2015",
                       "nccrcc_genentech_2014",
                       "nepc_wcm_2016",
                       "nhl_bcgsc_2011",
                       "nhl_bcgsc_2013",
                       "npc_nusingapore",
                       "nsclc_mskcc_2018",
                       "nsclc_tracerx_2017",
                       "nsclc_unito_2016",
                       "ov_tcga_pan_can_atlas_2018",
                       "paac_jhu_2014",
                       "paad_cptac_2021",
                       "paad_qcmg_uq_2016",
                       "paad_tcga_pan_can_atlas_2018",
                       "paad_utsw_2015",
                       "pact_jhu_2011",
                       "pan_origimed_2020",
                       "pancan_pcawg_2020",
                       "panet_arcnet_2017",
                       "panet_jhu_2011",
                       "panet_shanghai_2013",
                       "pcnsl_mayo_2015",
                       "pcpg_tcga_pan_can_atlas_2018",
                       "pediatric_dkfz_2017",
                       "plmeso_nyu_2015",
                       "pog570_bcgsc_2020",
                       "pptc_2019",
                       "prad_broad",
                       "prad_eururol_2017",
                       "prad_fhcrc",
                       "prad_mich",
                       "prad_msk_2019",
                       "prad_msk_mdanderson_2023",
                       "prad_mskcc",
                       "prad_mskcc_cheny1_organoids_2014",
                       "prad_su2c_2019",
                       "prad_tcga_pan_can_atlas_2018",
                       "prostate_dkfz_2018",
                       "prostate_pcbm_swiss_2019",
                       "rms_nih_2014",
                       "rt_target_2018_pub",
                       "sarc_mskcc",
                       "sarc_tcga_pan_can_atlas_2018",
                       "sarcoma_msk_2022",
                       "sarcoma_ucla_2024",
                       "scco_mskcc",
                       "sclc_cancercell_gardner_2017",
                       "sclc_jhu",
                       "sclc_ucologne_2015",
                       "sft_sysucc_2023",
                       "skcm_broad",
                       "skcm_broad_brafresist_2012",
                       "skcm_dfci_2015",
                       "skcm_mskcc_2014",
                       "skcm_tcga_pan_can_atlas_2018",
                       "skcm_vanderbilt_mskcc_2015",
                       "skcm_yale",
                       "stad_oncosg_2018",
                       "stad_pfizer_uhongkong",
                       "stad_tcga_pan_can_atlas_2018",
                       "stad_utokyo",
                       "stmyec_wcm_2022", 
                       "summit_2018",
                       "tet_nci_2014",
                       "tgct_tcga_pan_can_atlas_2018",
                       "thca_tcga_pan_can_atlas_2018",
                       "thym_tcga_pan_can_atlas_2018",
                       "uccc_nih_2017",
                       "ucec_ccr_cfdna_msk_2022",
                       "ucec_ccr_msk_2022",
                       "ucec_cptac_2020",
                       "ucec_tcga_pan_can_atlas_2018",
                       "ucs_jhu_2014",
                       "ucs_tcga_pan_can_atlas_2018",
                       "um_qimr_2016",
                       "urcc_mskcc_2016",
                       "utuc_cornell_baylor_mdacc_2019",
                       "utuc_igbmc_2021",
                       "utuc_msk_2019",
                       "utuc_mskcc_2015",
                       "uvm_tcga_pan_can_atlas_2018",
                       "vsc_cuk_2018",
                       "wt_target_2018_pub"
)

#feed in altered and unaltered samples lists from cbioportal download when TP53 queried in cBioPortal website as only muts checkbox genomic profiles, only cases with muts data dropdown, OQL search datatypes: mut; tp53, combine into 1 list, rename columns 
alteredsamples <- read.delim(paste0(input_path, 
                                    "altered_samples (2).txt"),  # ALL 232 unique studies
                             header = FALSE, 
                             sep = ":")
unalteredsamples <- read.delim(paste0(input_path, 
                                      "unaltered_samples (2).txt"), # ALL 232 unique studies
                               header = FALSE, 
                               sep = ":")

# # because T125T is an important mutation from germline data, but it's not annotated properly in TCGA samples in cbioportal somatic p53muts df, read in cases of GDC TCGA T125= i.e. T125T manually gotten from --> <https://portal.gdc.cancer.gov/v1/exploration?cases_size=100&cases_sort=%5B%7B%22field%22%3A%22project.project_id%22%2C%22order%22%3A%22asc%22%7D%5D&facetTab=cases&filters=%7B%22content%22%3A%5B%7B%22content%22%3A%7B%22field%22%3A%22cases.case_id%22%2C%22value%22%3A%5B%22set_id%3Adeee4d915eb6683cc5d01bf5197456a4f18103a434bb3447efbb7b16d8324d032a9699c386a38c7c22fe6cf84aafe4b4c81f347e4bd5e3e288239dd21ba9f027%22%5D%7D%2C%22op%22%3A%22IN%22%7D%2C%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22genes.gene_id%22%2C%22value%22%3A%5B%22ENSG00000141510%22%5D%7D%7D%5D%2C%22op%22%3A%22AND%22%7D&searchTableTab=cases>. once file read in, change incorrect annotations in p53muts to T125= for consistency with other studies' annotations
# tcga_known_t125t <- read.delim(paste0(input_path, 
#                                       "explore-case-table.2024-02-20 GDC TCGA T125=.tsv"), 
#                                header = TRUE,
#                                sep = "\t") %>%
#   clean_names()

# because above gdc link was for v1 and does not work now due to v2, obtain GDC TCGA T125= i.e., T125T manually from <https://portal.gdc.cancer.gov/> with program TCGA, mutated gene TP53, tissue type tumor, tumor descriptor primary, and ssm ID three muts that generate T125= (make mutation set after other parameters filter down cases a lot), final list of 40 cases is identical to above file
tcga_known_t125t <- read.delim(paste0(input_path,
                                      "biospecimen.cohort.2025-12-24/sample.tsv"), 
                               na.strings = "'--", 
                               header = TRUE, 
                               sep = "\t") %>%
  clean_names() %>% # next few lines are to collapse down to 40 cases and remove empty rows/cols
  filter(samples_sample_type == "Primary Tumor") %>% 
  distinct(cases_submitter_id, .keep_all = TRUE) %>% 
  remove_empty(which = c("rows", 
                         "cols")) 

# read in functional and structural annotations of p53 for all codons, obtained from: https://tp53.cancer.gov/view_data?bq_view_name=MutationView and downloaded as-is with no filters
#how to get to download page --> https://tp53.cancer.gov/ --> Functional/Structural Data --> Data Downloads Functional/Structural Data --> Functional/structural data in TP53 with their annotations (includes validated polymorphisms) --> click Preview icon to get to filterable data table --> download entire table without filters
origfunction <- read.csv(file = paste0(input_path, 
                                       "MutationView_r21.csv"), 
                         sep = ",",
                         header = TRUE) %>% 
  distinct() %>% 
  remove_rownames()

# read in arsenic-trioxide (ATO) rescuable p53 muts compiled in germline muts analyses
mut_rescued <- readRDS(file = paste0(analogous_germline_path, 
                                     "mut_rescued.rds"))

mut_rescue_PAT <- readRDS(file = paste0(analogous_germline_path, 
                                     "mut_rescue_PAT.rds"))

mut_not_rescued <- readRDS(file = paste0(analogous_germline_path, 
                                     "mut_not_rescued.rds"))

# mapped TP53 consensus SD and SA sites from hg19 UCSC same link as in isoforms Rmd file, to count as mut at consensus position, needs to start at these bp, which is how germline defines consensus muts too
intron_1_consensus_SD_bp <- c("7590694",
                              "7590693")

intron_1_consensus_SA_bp <- c("7579942",
                              "7579941") 

intron_2_consensus_SD_bp <- c("7579838",
                              "7579837")

intron_2_consensus_SA_bp <- c("7579723",
                              "7579722")

intron_3_consensus_SD_bp <- c("7579699",
                              "7579698")

intron_3_consensus_SA_bp <- c("7579592",
                              "7579591") 

intron_4_consensus_SD_bp <- c("7579311",
                              "7579310")

intron_4_consensus_SA_bp <- c("7578556",
                              "7578555")

intron_5_consensus_SD_bp <- c("7578370",
                              "7578369")

intron_5_consensus_SA_bp <- c("7578291",
                              "7578290")

intron_6_consensus_SD_bp <- c("7578176",
                              "7578175")

intron_6_consensus_SA_bp <- c("7577610",
                              "7577609")

intron_7_consensus_SD_bp <- c("7577498",
                              "7577497")

intron_7_consensus_SA_bp <- c("7577157",
                              "7577156")

intron_8_consensus_SD_bp <- c("7577018",
                              "7577017")

intron_8_consensus_SA_bp <- c("7576928",
                              "7576927")

intron_9_consensus_SD_bp <- c("7576852",
                              "7576851")

intron_9_consensus_SA_bp <- c("7574035",
                              "7574034")

intron_10_consensus_SD_bp <- c("7573926",
                               "7573925")

intron_10_consensus_SA_bp <- c("7573010",
                               "7573009")
```

$~$

# **Getting Cases from cBioPortal** {#getcases}

$~$

[]

$~$


``` r
#start package to get data from cBioPortal
cbio <- cBioPortal()

#get all clinical data of studies and add column listing study name 
clindata <- list()
for(i in nonoverlapping){
  clindata[[i]] <- clinicalData(api = cbio, studyId = i)
  clindata[[i]]$StudyID <- i
}

#COMBINE ALL CLINICAL DATA TIBBLES WITHIN LIST WITHOUT LOSING DATA SINCE COLUMNS ARE DIFFERENT 
clindatacombined <- Reduce(full_join, clindata) %>% 
  drop_na(sampleId) %>% #remove rows without sample ID
  drop_na(CANCER_TYPE_DETAILED) #remove rows without detailed cancer type

# combine read in samples into the search query list of samples
queriedsamples <- rbind(alteredsamples, 
                        unalteredsamples) %>% #can rbind because same column names and width
  remove_rownames() %>% 
  dplyr::rename(StudyID = V1, 
                SampleID = V2)

#create unique queried samples
uniquequeriedsamples <- distinct(queriedsamples, 
                                 SampleID) %>% 
  remove_rownames()

#only keep clinical data of unique queried samples and remove cancer of unknown primary rows, but keep NA cancer type since we just care about detailed cancer anyways, and only keep primary samples
clindatacombined <- clindatacombined %>% 
  subset(clindatacombined$sampleId %in% uniquequeriedsamples$SampleID) %>% 
  subset(CANCER_TYPE != "Cancer of Unknown Primary" | is.na(CANCER_TYPE)) %>%
  subset(SAMPLE_TYPE %in% (clindatacombined[grepl("Primary", 
                                                  clindatacombined$SAMPLE_TYPE, 
                                                  ignore.case = TRUE), # take upper or lowercase primary phrase
                                            "SAMPLE_TYPE"] %>% 
                             unique() %>% 
                             pull()))

#combine duplicate cancers that only have slight differences in spelling but are seen as different in R
clindatacombined <- clindatacombined %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Glioblastoma', 
                                                                          'Glioblastoma multiforme'), 
                                              'Glioblastoma Multiforme', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Papillary Thyroid Cancer', 
                                                                          'Papillary Throid Carcinoma'), 
                                              'Papillary Thyroid Carcinoma', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Acute myeloid leukemia', 
                                                                          'Acute myeloid leukemias'), 
                                              'Acute Myeloid Leukemia', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Adrenocortical carcinoma'),
                                              'Adrenocortical Carcinoma', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Breast Invasive Carcinoma, NOS', 
                                                                          'Breast Invasive Carcinoma (NOS)',
                                                                          'Breast Invasive Cancer, NOS', 
                                                                          'Breast Invasive Carcinoma', 
                                                                          'Invasive Breast Carcinoma'), 
                                              'Breast Invasive Carcinoma, NOS', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Atypical teratoid/rhabdoid tumor'),
                                              'Atypical Teratoid/Rhabdoid Tumor', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Anaplastic ependymoma'), 
                                              'Anaplastic Ependymoma', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(tolower(CANCER_TYPE_DETAILED) %>% 
                                                str_detect('chromophobe.*renal cell carcinoma'), 
                                              'Chromophobe Renal Cell Carcinoma', 
                                              CANCER_TYPE_DETAILED)) %>% 
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Anaplastic pleomorphic xanthoastrocytoma'), 
                                              'Anaplastic Pleomorphic Xanthoastrocytoma', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Ewing's sarcoma"), 
                                              'Ewing Sarcoma',
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Follicular Thyroid Cancer'), 
                                              'Follicular Thyroid Carcinoma', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Head and Neck Carcinoma Other'), 
                                              'Head and Neck Carcinoma, Other', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Head and Neck Squamous Cell CarcinomaÃŠ'), 
                                              'Head and Neck Squamous Cell Carcinoma',
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Hepatocellular carcinoma', 
                                                                          'Liver Hepatocellular Carcinoma'),
                                              'Hepatocellular Carcinoma', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Liver Hepatocellular Carcinoma plus Intrahepatic Cholangiocarcinoma'),
                                              'Hepatocellular Carcinoma plus Intrahepatic Cholangiocarcinoma',
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Inflammatory myofibroblastic tumor'),
                                              'Inflammatory Myofibroblastic Tumor', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Low-Grade Glioma (NOS)'), 
                                              'Low-Grade Glioma, NOS', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Low grade Fibromyxoid Sarcoma'), 
                                              'Low-Grade Fibromyxoid Sarcoma', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Medullary Thyroid Cancer'), 
                                              'Medullary Thyroid Carcinoma', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Neuroendocrine Tumor, NOS'),
                                              'Neuroendocrine Carcinoma, NOS', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Ovarian Carcinoma Other', 
                                                                          'Ovarian Cancer, Other'), 
                                              'Ovarian Carcinoma, Other', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Papillary Kidney Renal Cell Carcinoma'),
                                              'Papillary Renal Cell Carcinoma', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Pilocytic astrocytoma'),
                                              'Pilocytic Astrocytoma', 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Wilms' tumors",
                                                                          'Wilms Tumor'), 
                                              "Wilms' Tumor", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Prostate"), 
                                              "Prostate Cancer, NOS",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Breast"),
                                              "Breast Cancer", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Bowel"), 
                                              "Bowel Cancer, NOS", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Adenocarcinoma"),
                                              "Pancreatic Adenocarcinoma", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Well Differentiated Liposarcoma'), 
                                              "Well-Differentiated Liposarcoma", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Undifferentiated Sarcoma"),
                                              "Undifferentiated Soft Tissue Sarcoma", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Undifferentiated Pleomorphic Sarcoma Malignant Fibrous Histiocytoma"), 
                                              "Undifferentiated Pleomorphic Sarcoma/Malignant Fibrous Histiocytoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Unclassified Kidney Renal Cell Carcinoma'), 
                                              "Unclassified Renal Cell Carcinoma", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Diffse large B-cell lymphoma"), 
                                              "Diffuse Large B-Cell Lymphoma", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c('Follicular lymphoma'), 
                                              "Follicular Lymphoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Gallbladder Cancer"), 
                                              "Gallbladder Carcinoma", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Head and Neck"), 
                                              "Head and Neck Cancer, NOS",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Kidney Renal Cell Carcinoma Other"),
                                              "Renal Cell Carcinoma, Other", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Lung"), 
                                              "Lung Cancer, NOS", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Mixed Carcinoma"), 
                                              "Breast Mixed Carcinoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Sarcoma, NOS"), 
                                              "Soft Tissue Sarcoma, NOS", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Gastric Adenocarcinoma", 
                                                                          'Stomach Adenocarcinoma',
                                                                          "Diffuse Type Stomach Adenocarcinoma",
                                                                          "Intestinal Type Stomach Adenocarcinoma",
                                                                          "Mucinous Stomach Adenocarcinoma",
                                                                          "Papillary Stomach Adenocarcinoma",
                                                                          "Signet Ring Cell Carcinoma of the Stomach",
                                                                          "Tubular Stomach Adenocarcinoma"),
                                              "Gastric Adenocarcinoma", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Bone Sarcoma Other"),
                                              "Bone Sarcoma, Other", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Breast Carcinoma Other", 
                                                                          "Breast Neoplasm, NOS"), 
                                              "Breast Carcinoma, Other", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Colon Adenocarcinoma",
                                                                          "Rectal Adenocarcinoma",
                                                                          "Mucinous Adenocarcinoma of the Colon and Rectum",
                                                                          "Signet Ring Cell Adenocarcinoma of the Colon and Rectum"),
                                              "Colorectal Adenocarcinoma", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Colorectal Carcinoma Other"), 
                                              "Colorectal Carcinoma, Other", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Esophageal Carcinoma Other"),
                                              "Esophageal Carcinoma, Other", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Gallbladder Carcinoma Other"),
                                              "Gallbladder Carcinoma, Other", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Gastric Carcinoma Other"), 
                                              "Gastric Carcinoma, Other", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Melanoma Other"), 
                                              "Melanoma, Other", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Non Small Cell Lung Cancer Other"), 
                                              "Non Small Cell Lung Cancer, Other", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Pancreatic Cancer Other"),
                                              "Pancreatic Cancer, Other", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Soft Tissue Sarcoma Other"), 
                                              "Soft Tissue Sarcoma, Other", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Thyroid Carcinoma Other"), 
                                              "Thyroid Carcinoma, Other", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Urothelial Carcinoma Other"),
                                              "Urothelial Carcinoma, Other", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Uterine Corpus Endometrial Carcinoma Other"), 
                                              "Uterine Corpus Endometrial Carcinoma, Other", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Breast Cancer", 
                                                                          "Breast Carcinoma, Other"), 
                                              "Breast Carcinoma, Other", 
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Diffuse Large B-Cell Lymphoma"),
                                              "Diffuse Large B-Cell Lymphoma, NOS",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Anaplastic medulloblastoma"),
                                              "Anaplastic Medulloblastoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Ductal Carcinoma In Situ (DCIS)"),
                                              "Breast Ductal Carcinoma In Situ",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Endometrioid Carcinoma"),
                                              "Cervical Endometrioid Carcinoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Ependymomal Tumor"),
                                              "Ependymoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Adenocarcinoma of the Gastroesophageal Junction"),
                                              "Esophagogastric Adenocarcinoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Gallbladder Carcinoma, Other"),
                                              "Gallbladder Carcinoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Head and Neck Carcinoma, Other", 
                                                                          "Head and Neck Cancer, NOS"),
                                              "Head and Neck Carcinoma, NOS",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Infiltrating Ductal Carcinoma"),
                                              "Breast Invasive Ductal Carcinoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Infiltrating Lobular Carcinoma"),
                                              "Breast Invasive Lobular Carcinoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Infiltrating Tubular Carcinoma"),
                                              "Breast Invasive Tubular Carcinoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Melanoma, Other",
                                                                          "Cutaneous Melanoma",
                                                                          "Uveal Melanoma",
                                                                          "Acral Melanoma",
                                                                          "Anorectal Mucosal Melanoma",
                                                                          "Mucosal Melanoma of the Vulva/Vagina",
                                                                          "Head and Neck Mucosal Melanoma",
                                                                          "Mucosal Melanoma of the Esophagus",
                                                                          "Mucosal Melanoma of the Urethra",
                                                                          "Melanoma of Unknown Primary"),
                                              "Melanoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Non Small Cell Lung Cancer, Other",
                                                                          "Poorly Differentiated Non-Small Cell Lung Cancer",
                                                                          "Large Cell Lung Carcinoma",
                                                                          "Sarcomatoid Carcinoma of the Lung",
                                                                          "Lung Adenosquamous Carcinoma",
                                                                          "Spindle Cell Carcinoma of the Lung",
                                                                          "Lung Adenocarcinoma",
                                                                          "Lung Squamous Cell Carcinoma",
                                                                          "Basaloid Large Cell Carcinoma of the Lung",
                                                                          "Adenoid Cystic Carcinoma of the Lung",
                                                                          "Ciliated Muconodular Papillary Tumor of the Lung"),
                                              "Non-Small Cell Lung Cancer",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Renal Cell Carcinoma, Other"),
                                              "Renal Cell Carcinoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Salivary Carcinoma, Other"),
                                              "Salivary Carcinoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Soft Tissue Sarcoma, Other"),
                                              "Soft Tissue Sarcoma, NOS",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Extrahepatic Cholangiocarcinoma",
                                                                          "Intrahepatic Cholangiocarcinoma",
                                                                          "Perihilar Cholangiocarcinoma"),
                                              "Cholangiocarcinoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("High-Grade Serous Ovarian Cancer",
                                                                          "Low-Grade Serous Ovarian Cancer",
                                                                          "Ovarian Serous Carcinoma"),
                                              "Serous Ovarian Cancer",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Anaplastic Astrocytoma",
                                                                          "Diffuse Astrocytoma",
                                                                          "Pilocytic Astrocytoma"),
                                              "Astrocytoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Anaplastic Oligoastrocytoma"),
                                              "Oligoastrocytoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Anaplastic Oligodendroglioma"),
                                              "Oligodendroglioma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Nasopharyngeal Carcinoma",
                                                                          "Oropharynx Squamous Cell Carcinoma",
                                                                          "Hypopharynx Squamous Cell Carcinoma",
                                                                          "Oral Cavity Squamous Cell Carcinoma",
                                                                          "Larynx Squamous Cell Carcinoma",
                                                                          "Sinonasal Squamous Cell Carcinoma"),
                                              "Head and Neck Squamous Cell Carcinoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Lung Neuroendocrine Tumor",
                                                                          "Large Cell Neuroendocrine Carcinoma",
                                                                          "Atypical Lung Carcinoid",
                                                                          "Lung Carcinoid"),
                                              "Lung Neuroendocrine Tumor, Other",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Pleural Mesothelioma, Epithelioid Type",
                                                                          "Pleural Mesothelioma, Biphasic Type",
                                                                          "Pleural Mesothelioma, Sarcomatoid Type",
                                                                          "Pleural Mesothelioma",
                                                                          "Peritoneal Mesothelioma"),
                                              "Mesothelioma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Papillary Thyroid Carcinoma",
                                                                          "Poorly Differentiated Thyroid Cancer",
                                                                          "Anaplastic Thyroid Cancer",
                                                                          "Follicular Thyroid Carcinoma",
                                                                          "Medullary Thyroid Carcinoma",
                                                                          "Hurthle Cell Thyroid Cancer",
                                                                          "Thyroid Carcinoma, Other"),
                                              "Thyroid Carcinoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Chondroblastic Osteosarcoma",
                                                                          "Osteoblastic Osteosarcoma",
                                                                          "Fibroblastic Osteosarcoma",
                                                                          "Small Cell Osteosarcoma"),
                                              "Osteosarcoma",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("Endometrioid Ovarian Cancer",
                                                                          "Clear Cell Ovarian Cancer",
                                                                          "Mucinous Ovarian Cancer",
                                                                          "Ovarian Epithelial Tumor"),
                                              "Ovarian Epithelial Tumor, Other",
                                              CANCER_TYPE_DETAILED)) %>%
  dplyr::mutate(CANCER_TYPE_DETAILED = ifelse(CANCER_TYPE_DETAILED %in% c("High-Grade Glioma, NOS",
                                                                          "Diffuse Glioma",
                                                                          "Low-Grade Glioma, NOS",
                                                                          "Diffuse Intrinsic Pontine Glioma",
                                                                          "Anaplastic Ganglioglioma",
                                                                          "Ganglioglioma"),
                                              "Glioma, Other",
                                              CANCER_TYPE_DETAILED))

#make new altered and unaltered samples and queried sample list now that problematic rows are removed
alteredsamples <- alteredsamples %>% 
  subset(alteredsamples$V2 %in% clindatacombined$sampleId)
unalteredsamples <- unalteredsamples %>% 
  subset(unalteredsamples$V2 %in% clindatacombined$sampleId)
queriedsamples <- rbind(alteredsamples, 
                        unalteredsamples) %>% 
  remove_rownames() %>% 
  dplyr::rename(StudyID = V1, 
                SampleID = V2)

#make new unique queried samples list and apply again to clinical data (should not change latter)
uniquequeriedsamples <- distinct(queriedsamples, 
                                 SampleID) %>% 
  remove_rownames()
clindatacombined <- clindatacombined %>%  
  subset(clindatacombined$sampleId %in% uniquequeriedsamples$SampleID)

#sort cancer type detailed by unique sample count, take only cancers with 100+ unique samples, add total row
cancersunique <- clindatacombined %>%                              
  dplyr::group_by(CANCER_TYPE_DETAILED) %>%
  dplyr::summarise(count = n_distinct(sampleId))
cancersunique <- cancersunique[order(-cancersunique$count),]
cancersuniquewithtotal <- cancersunique %>% 
  adorn_totals()
cancersover100 <- cancersunique %>%  
  subset(count >= 100)
cancersover100 <- cancersover100[order(-cancersover100$count),]
cancersover100withtotal <- cancersover100 %>% 
  adorn_totals()

# save as table
write_csv(cancersover100withtotal, 
          file = paste0(table_path, 
                        "cancersover100.csv"))

# further clean up clindatacombined df
clindatacombined <- clindatacombined %>% 
  remove_empty("cols") %>%
  relocate(c(sampleId,
             StudyID,
             CANCER_TYPE,
             CANCER_TYPE_DETAILED,
             SAMPLE_TYPE,
             SEX,
             RACE,
             ETHNICITY,
             COUNTRY,
             COUNTRY_OF_PROCUREMENT
             #,
             #TMB_NONSYNONYMOUS,
             #MUTATION_COUNT
             ),
           .after = patientId) %>%
  mutate_at(c(#"TMB_NONSYNONYMOUS",
              #"MUTATION_COUNT",
              "SAMPLE_COUNT"),
            as.numeric)

#extract samples of interest with study ID, cancer info
set_cbioportal_db(db = "public")
test_cbioportal_db()
studiesandsamples <- clindatacombined[, c("sampleId", 
                                          "StudyID", 
                                          "CANCER_TYPE", 
                                          "CANCER_TYPE_DETAILED")] # can only have columns with no NAs
studiesandsamples <- studiesandsamples %>% 
  dplyr::rename(sample_id = sampleId, 
                study_id = StudyID)

#get p53 mutation data for samples
p53muts <- get_mutations_by_sample(sample_study_pairs = studiesandsamples, 
                                   genes = "TP53")

#add cancer type and detailed columns
studiesandsamples <- studiesandsamples %>% 
  dplyr::rename(sampleId = sample_id, 
                studyId = study_id)
p53muts <- merge(p53muts, 
                 studiesandsamples, 
                 by = c("sampleId", 
                        "studyId")) 
p53muts <- merge(p53muts,
                 clindatacombined %>%
                   dplyr::select(c("sampleId", 
                                   "StudyID", 
                                   "SAMPLE_TYPE",
                                   "RACE",
                                   "ETHNICITY",
                                   "COUNTRY",
                                   "COUNTRY_OF_PROCUREMENT"
                                   #,
                                   #"TMB_NONSYNONYMOUS",
                                   #"MUTATION_COUNT"
                                   )) %>%
                   dplyr::rename(studyId = StudyID), 
                 by = c("sampleId", 
                        "studyId")) 

#rearrange columns 
p53muts <- p53muts %>% 
  relocate(hugoGeneSymbol, 
           sampleId, 
           studyId, 
           CANCER_TYPE, 
           CANCER_TYPE_DETAILED, 
           proteinChange, 
           mutationType, 
           variantType, 
           #TMB_NONSYNONYMOUS,
           #MUTATION_COUNT,
           #referenceAllele, 
           #variantAllele, 
           .before = entrezGeneId)

# freezing it in time almost, so that can add to clindata before mut subsetting which loses pts
p53muts_forclindata <- p53muts

# combine duplicate mut types based on spelling and remove any muts denoted as germline but keep NA in that column
p53muts <- p53muts %>% 
  subset(mutationStatus != "GERMLINE" | is.na(mutationStatus)) %>% 
  dplyr::mutate(mutationType = ifelse(mutationType %in% c("Frame_Shift_Del", 
                                                          'frame_shift_del'), 
                                      "Frame_Shift_Del", 
                                      mutationType)) %>% 
  remove_empty("cols") # remove all cols which have only NA in them

# remove mutations with unspecified protein change 
p53muts <- p53muts %>% 
  subset(proteinChange != "MUTATED") %>% 
  subset(proteinChange != "-") %>%
  remove_empty("cols") # remove all cols which have only NA in them

#check correct joining of cancer columns to p53muts dataset by concatenating key columns
test <- as.list(paste0(studiesandsamples$sampleId, 
                       studiesandsamples$studyId, 
                       studiesandsamples$CANCER_TYPE, 
                       studiesandsamples$CANCER_TYPE_DETAILED))
test2 <- as.list(paste0(p53muts$sampleId, 
                        p53muts$studyId, 
                        p53muts$CANCER_TYPE, 
                        p53muts$CANCER_TYPE_DETAILED))
all(test2 %in% test) #should be TRUE if joined correctly as it checks if all p53muts columns exactly in main

# # some studies overlap with TP53 database somatic studies so removing them from here, comment these lines out if these studies need to be retained i.e. data not shown in conjunction with TP53 somatic data
# p53muts <- p53muts %>% 
#   subset(!(studyId %in% c("sarc_mskcc", # soft tissue sarcoma paper nature genetics 2010
#                           "hnsc_broad", # HNSCC paper science 2011
#                           "hnsc_jhu"))) # HNSCC paper science 2011

# correct T125= annotations which have been misannotated in cbioportal but are proper in GDC
p53muts <- p53muts %>% 
  mutate(proteinChange = ifelse((patientId %in% tcga_known_t125t$cases_submitter_id & proteinChange == "X125_splice"), 
                                "T125=", 
                                proteinChange))

# correct splice muts not annotated as T125= although same genomic position and nucleotide changes
p53muts <- p53muts %>%
  dplyr::mutate(proteinChange = ifelse((chr == "17" & startPosition == 7579312 & endPosition == 7579312 & ncbiBuild == "GRCh37" & referenceAllele == "C" & variantAllele %in% c("A", "G", "T") & proteinChange == "X125_splice"),
                                       "T125=", 
                                       proteinChange))

# add ages to p53mutscancersover100 and p53muts dfs
clindataages <- clindatacombined %>% 
  dplyr::select(c(sampleId, 
                  starts_with("AGE"),
                  contains("diagnosis age", 
                           ignore.case = TRUE),
                  contains("CURRENT_AGE_DEID", 
                           ignore.case = TRUE))) %>% 
  distinct() %>% 
  remove_empty("cols") # remove all cols which have only NA in them
# %>%dplyr::select(-c(AGENT, 
#           AGE_IN_DAYS, 
#           AGE_CLASS,
#           AGE_CURRENT,
#           AGE_AT_INITIAL_DIAGNOSIS, 
#           AGE_AT_LAST_KNOWN_CLINICAL_STATUS, 
#           AGE_AT_CHEMOTHERAPY_STOP, 
#           AGE_AT_RADIATION_START, 
#           AGE_AT_RADIATION_STOP,
#           AGE_AT_SPECIMEN_DIAGNOSIS,
#           AGE_AT_CHEMOTHERAPY_START, 
#           AGE_GROUP, 
#           AGE_TESTING_YEARS))
clindataages <- clindataages[!duplicated(clindataages$sampleId),]
clindataages$Age_used <- clindataages$AGE
table(is.na(clindataages$AGE))
table(is.na(clindataages$Age_used)) # check how many are NA in column every time, should be decreasing over time
clindataages <- clindataages %>% 
  mutate(Age_used = ifelse(is.na(clindataages$Age_used),
                           AGE_AT_DIAGNOSIS,
                           Age_used))
table(is.na(clindataages$Age_used))
clindataages <- clindataages %>%
  mutate(Age_used = ifelse(is.na(clindataages$Age_used),
                           AGE_AT_DX,
                           Age_used))
table(is.na(clindataages$Age_used))
clindataages <- clindataages %>%
  mutate(Age_used = ifelse(is.na(clindataages$Age_used),
                           `DIAGNOSIS AGE`,
                           Age_used))
table(is.na(clindataages$Age_used))
clindataages <- clindataages %>%
  mutate(Age_used = ifelse(is.na(clindataages$Age_used),
                           AGE_AT_PROCUREMENT,
                           Age_used))
table(is.na(clindataages$Age_used))
clindataages <- clindataages %>%
  mutate(Age_used = ifelse(is.na(clindataages$Age_used),
                           AGE_AT_LAST_FOLLOWUP,
                           Age_used))
table(is.na(clindataages$Age_used)) 
clindataages <- clindataages %>%
  mutate(Age_used = ifelse(is.na(clindataages$Age_used),
                           AGE_CURRENT,
                           Age_used))
table(is.na(clindataages$Age_used)) 
clindataages <- clindataages %>%
  mutate(Age_used = ifelse(is.na(clindataages$Age_used),
                           CURRENT_AGE_DEID,
                           Age_used))
table(is.na(clindataages$Age_used)) # still don't have all ages annotated but these are best possible ages added
clindataages <- clindataages %>% 
  dplyr::select(c(sampleId, 
                  Age_used)) %>% 
  # drop_na() %>% 
  distinct() 

# p53mutscancersover100 <- left_join(p53mutscancersover100, 
#                                    clindataages, 
#                                    by = 'sampleId')
# table(is.na(p53mutscancersover100$Age_used)) # not all muts have ages associated w/ them as evidenced by TRUE NAs
# class(p53mutscancersover100$Age_used) <- 'numeric'
# p53mutscancersover100 <- p53mutscancersover100 %>% 
#   transform(Age_stratum = ifelse(Age_used < 20,
#                                  "0-19",
#                                  "notyet")) %>% 
#   transform(Age_stratum = ifelse(Age_used >= 20 & Age_used < 30,
#                                  "20-29",
#                                  Age_stratum)) %>% 
#   transform(Age_stratum = ifelse(Age_used >= 30 & Age_used < 40,
#                                  "30-39",
#                                  Age_stratum)) %>% 
#   transform(Age_stratum = ifelse(Age_used >= 40 & Age_used < 50,
#                                  "40-49",
#                                  Age_stratum)) %>% 
#   transform(Age_stratum = ifelse(Age_used >= 50 & Age_used < 60,
#                                  "50-59",
#                                  Age_stratum)) %>% 
#   transform(Age_stratum = ifelse(Age_used >= 60 & Age_used < 70,
#                                  "60-69",
#                                  Age_stratum)) %>% 
#   transform(Age_stratum = ifelse(Age_used >= 70 & Age_used < 80,
#                                  "70-79",
#                                  Age_stratum)) %>% 
#   transform(Age_stratum = ifelse(Age_used >= 80,
#                                  "80 and up",
#                                  Age_stratum)) %>% # some pts repeated in studies but ages not updated
#   transform(Age_stratum = ifelse(patientId %in% c("TCGA-06-0190"),
#                                  "60-69",
#                                  Age_stratum)) %>% # some pts repeated in studies but ages not updated
#   transform(Age_stratum = ifelse(patientId %in% c("TCGA-06-0210"),
#                                  "70-79",
#                                  Age_stratum)) %>% # some pts repeated in studies but ages not updated
#   transform(Age_stratum = ifelse(patientId %in% c("TCGA-06-0221"),
#                                  "30-39",
#                                  Age_stratum)) %>% # some pts repeated in studies but ages not updated
#   transform(Age_used = ifelse(patientId %in% c("TCGA-06-0190"),
#                               "62",
#                               Age_used)) %>% # some pts repeated in studies but ages not updated
#   transform(Age_used = ifelse(patientId %in% c("TCGA-06-0210"),
#                               "72",
#                               Age_used)) %>% # some pts repeated in studies but ages not updated
#   transform(Age_used = ifelse(patientId %in% c("TCGA-06-0221"),
#                               "31",
#                               Age_used)) %>% 
#   # drop_na(Age_stratum) %>% 
#   distinct()

p53muts <- left_join(p53muts, 
                     clindataages, 
                     by = 'sampleId')
table(is.na(p53muts$Age_used)) # not all muts have ages associated with them as evidenced by TRUE NAs
class(p53muts$Age_used) <- 'numeric'
p53muts <- p53muts %>% 
  transform(Age_stratum = ifelse(Age_used < 20,
                                 "0-19",
                                 "notyet")) %>% 
  transform(Age_stratum = ifelse(Age_used >= 20 & Age_used < 30,
                                 "20-29",
                                 Age_stratum)) %>% 
  transform(Age_stratum = ifelse(Age_used >= 30 & Age_used < 40,
                                 "30-39",
                                 Age_stratum)) %>% 
  transform(Age_stratum = ifelse(Age_used >= 40 & Age_used < 50,
                                 "40-49",
                                 Age_stratum)) %>% 
  transform(Age_stratum = ifelse(Age_used >= 50 & Age_used < 60,
                                 "50-59",
                                 Age_stratum)) %>% 
  transform(Age_stratum = ifelse(Age_used >= 60 & Age_used < 70,
                                 "60-69",
                                 Age_stratum)) %>% 
  transform(Age_stratum = ifelse(Age_used >= 70 & Age_used < 80,
                                 "70-79",
                                 Age_stratum)) %>% 
  transform(Age_stratum = ifelse(Age_used >= 80,
                                 "80 and up",
                                 Age_stratum)) %>% # some pts repeated in studies but ages not updated
  transform(Age_stratum = ifelse(patientId %in% c("TCGA-06-0190"),
                                 "60-69",
                                 Age_stratum)) %>% # some pts repeated in studies but ages not updated
  transform(Age_stratum = ifelse(patientId %in% c("TCGA-06-0210"),
                                 "70-79",
                                 Age_stratum)) %>% # some pts repeated in studies but ages not updated
  transform(Age_stratum = ifelse(patientId %in% c("TCGA-06-0221"),
                                 "30-39",
                                 Age_stratum)) %>% # some pts repeated in studies but ages not updated
  transform(Age_used = ifelse(patientId %in% c("TCGA-06-0190"),
                              "62",
                              Age_used)) %>% # some pts repeated in studies but ages not updated
  transform(Age_used = ifelse(patientId %in% c("TCGA-06-0210"),
                              "72",
                              Age_used)) %>% # some pts repeated in studies but ages not updated
  transform(Age_used = ifelse(patientId %in% c("TCGA-06-0221"),
                              "31",
                              Age_used)) %>% 
  # drop_na(Age_stratum) %>% 
  distinct()

# add sexes to p53mutscancersover100 and p53muts dfs
clindatasexes <- clindatacombined %>% 
  dplyr::select(c(sampleId, 
                  SEX)) %>% 
  distinct()
clindatasexes <- clindatasexes[!duplicated(clindatasexes$sampleId),]
# clindatasexes <- clindatasexes %>% 
#   drop_na()

# p53mutscancersover100 <- left_join(p53mutscancersover100, 
#                                    clindatasexes, 
#                                    by = 'sampleId')
# p53mutscancersover100 <- p53mutscancersover100 %>%
#   transform(SEX = ifelse(CANCER_TYPE_DETAILED %in% c("Uterine Carcinosarcoma/Uterine Malignant Mixed Mullerian Tumor",
#                                                      "Breast Invasive Carcinoma, NOS",
#                                                      "Serous Ovarian Cancer"),
#                          "Female",
#                          SEX))
# # p53mutscancersover100 <- p53mutscancersover100 %>% 
# #   drop_na(SEX)
# p53mutscancersover100 <- p53mutscancersover100 %>% 
#   transform(SEX = ifelse(SEX %in% c("FEMALE"),
#                          "Female",
#                          SEX)) %>% 
#   transform(SEX = ifelse(SEX %in% c("MALE"),
#                          "Male",
#                          SEX)) %>% 
#   transform(SEX = ifelse(SEX %in% c("U") | is.na(SEX),
#                          "Unknown",
#                          SEX)) %>% 
#   transform(SEX = ifelse(patientId %in% c("TCGA-06-0210"), # some pts repeated in studies but sexes not updated
#                          "Female",
#                          SEX)) %>% 
#   transform(SEX = ifelse(patientId %in% c("TCGA-06-0190",
#                                           "TCGA-06-0221"), # some pts repeated in studies but sexes not updated
#                          "Male",
#                          SEX))  

p53muts <- left_join(p53muts, 
                     clindatasexes, 
                     by = 'sampleId')
p53muts <- p53muts %>%
  transform(SEX = ifelse(CANCER_TYPE_DETAILED %in% c("Uterine Carcinosarcoma/Uterine Malignant Mixed Mullerian Tumor",
                                                     "Breast Invasive Carcinoma, NOS",
                                                     "Serous Ovarian Cancer"),
                         "Female",
                         SEX))
# p53muts <- p53muts %>% 
#   drop_na(SEX)
p53muts <- p53muts %>% 
  transform(SEX = ifelse(SEX %in% c("FEMALE"),
                         "Female",
                         SEX)) %>% 
  transform(SEX = ifelse(SEX %in% c("MALE"),
                         "Male",
                         SEX)) %>% 
  transform(SEX = ifelse(SEX %in% c("U") | is.na(SEX),
                         "Unknown",
                         SEX)) %>% 
  transform(SEX = ifelse(patientId %in% c("TCGA-06-0210"), # some pts repeated in studies but sexes not updated
                         "Female",
                         SEX)) %>% 
  transform(SEX = ifelse(patientId %in% c("TCGA-06-0190",
                                          "TCGA-06-0221"), # some pts repeated in studies but sexes not updated
                         "Male",
                         SEX)) 

# update clin data sexes and ages too
clindatacombined <- Reduce(full_join, 
                           list((clindatacombined %>% 
                                   dplyr::select(-c(SEX))), # don't want to retain old sexes
                                clindataages,
                                clindatasexes)) %>% 
  remove_empty("cols")

class(clindatacombined$Age_used) <- 'numeric'
clindatacombined <- clindatacombined %>% 
  transform(Age_stratum = ifelse(Age_used < 20,
                                 "0-19",
                                 "notyet")) %>% 
  transform(Age_stratum = ifelse(Age_used >= 20 & Age_used < 30,
                                 "20-29",
                                 Age_stratum)) %>% 
  transform(Age_stratum = ifelse(Age_used >= 30 & Age_used < 40,
                                 "30-39",
                                 Age_stratum)) %>% 
  transform(Age_stratum = ifelse(Age_used >= 40 & Age_used < 50,
                                 "40-49",
                                 Age_stratum)) %>% 
  transform(Age_stratum = ifelse(Age_used >= 50 & Age_used < 60,
                                 "50-59",
                                 Age_stratum)) %>% 
  transform(Age_stratum = ifelse(Age_used >= 60 & Age_used < 70,
                                 "60-69",
                                 Age_stratum)) %>% 
  transform(Age_stratum = ifelse(Age_used >= 70 & Age_used < 80,
                                 "70-79",
                                 Age_stratum)) %>% 
  transform(Age_stratum = ifelse(Age_used >= 80,
                                 "80 and up",
                                 Age_stratum)) %>% # some pts repeated in studies but ages not updated
  transform(Age_stratum = ifelse(patientId %in% c("TCGA-06-0190"),
                                 "60-69",
                                 Age_stratum)) %>% # some pts repeated in studies but ages not updated
  transform(Age_stratum = ifelse(patientId %in% c("TCGA-06-0210"),
                                 "70-79",
                                 Age_stratum)) %>% # some pts repeated in studies but ages not updated
  transform(Age_stratum = ifelse(patientId %in% c("TCGA-06-0221"),
                                 "30-39",
                                 Age_stratum)) %>% # some pts repeated in studies but ages not updated
  transform(Age_used = ifelse(patientId %in% c("TCGA-06-0190"),
                              "62",
                              Age_used)) %>% # some pts repeated in studies but ages not updated
  transform(Age_used = ifelse(patientId %in% c("TCGA-06-0210"),
                              "72",
                              Age_used)) %>% # some pts repeated in studies but ages not updated
  transform(Age_used = ifelse(patientId %in% c("TCGA-06-0221"),
                              "31",
                              Age_used)) %>% 
  remove_empty("cols") %>% 
  distinct()

clindatacombined <- clindatacombined %>%
  transform(SEX = ifelse(CANCER_TYPE_DETAILED %in% c("Uterine Carcinosarcoma/Uterine Malignant Mixed Mullerian Tumor",
                                                     "Breast Invasive Carcinoma, NOS",
                                                     "Serous Ovarian Cancer"),
                         "Female",
                         SEX)) %>% 
  transform(SEX = ifelse(SEX %in% c("FEMALE"),
                         "Female",
                         SEX)) %>% 
  transform(SEX = ifelse(SEX %in% c("MALE"),
                         "Male",
                         SEX)) %>% 
  transform(SEX = ifelse(SEX %in% c("U") | is.na(SEX),
                         "Unknown",
                         SEX)) %>% 
  transform(SEX = ifelse(patientId %in% c("TCGA-06-0210"), # some pts repeated in studies but sexes not updated
                         "Female",
                         SEX)) %>% 
  transform(SEX = ifelse(patientId %in% c("TCGA-06-0190",
                                          "TCGA-06-0221"), # some pts repeated in studies but sexes not updated
                         "Male",
                         SEX)) %>% 
  remove_empty("cols") 



# ignore this, it loses info and still has same number of rows as regular full join
# full_join((clindatacombined %>% # match on fewest cols to be safe since dfs edited along the way
#              dplyr::select(-c(#patientId,
#                               #sampleId,
#                               #CANCER_TYPE,
#                               #CANCER_TYPE_DETAILED,
#                               SAMPLE_TYPE,
#                               RACE,
#                               ETHNICITY,
#                               COUNTRY,
#                               TMB_NONSYNONYMOUS,
#                               MUTATION_COUNT,
#                               Age_used,
#                               SEX,
#                               Age_stratum))),
#           (p53muts %>%
#              dplyr::select(-c(#patientId,
#                               #sampleId,
#                               #CANCER_TYPE,
#                               #CANCER_TYPE_DETAILED,
#                               SAMPLE_TYPE,
#                               RACE,
#                               ETHNICITY,
#                               COUNTRY,
#                               TMB_NONSYNONYMOUS,
#                               MUTATION_COUNT,
#                               Age_used,
#                               SEX,
#                               Age_stratum))))


# identify pts with multiple distinct mutations and remove them, so that only have pts with 1 mut each
dup_muts_pts <- p53muts %>% 
  dplyr::select(c(proteinChange, 
                  patientId)) %>% 
  group_by(patientId) %>% 
  filter(n() > 1) %>% # first get all pts with multiple entries regardless of muts
  distinct() %>% # only keep pts with multiple entries and multiple distinct muts
  group_by(patientId) %>% 
  filter(n() > 1) %>% # get all pts with multiple distinct muts
  arrange(patientId) %>%
  pull(patientId) %>%
  unique()

p53muts <- p53muts %>%
  subset(!(patientId %in% dup_muts_pts)) %>%
  distinct()


#make subset of p53muts which only have cancers with at least 100 samples; not changing object name
p53mutscancersover100 <- p53muts
# %>% subset(p53muts$CANCER_TYPE_DETAILED %in% cancersover100$CANCER_TYPE_DETAILED)

#check that cancers in new subset of p53 muts are only those with over 100 samples
#all(p53mutscancersover100$CANCER_TYPE_DETAILED %in% cancersover100$CANCER_TYPE_DETAILED) #should be TRUE

#identify which of 100+ samples cancers don't have p53 muts and will not be included in pivot tables
#cancersover100$CANCER_TYPE_DETAILED[!cancersover100$CANCER_TYPE_DETAILED %in% p53mutscancersover100$CANCER_TYPE_DETAILED] #can search orig p53muts for ID'd cancers to confirm their absence


# find patients listed once or multiple times, subset data to such patients, and compare if equivalent
ptswithcancer <- p53mutscancersover100 %>% 
  dplyr::count(patientId, 
               #Age_used, # duplicates b/c if pts repeated across studies (rare) could have missing ages
               #CANCER_TYPE, # duplicates b/c slightly different cancer name despite same age and detailed cancer
               CANCER_TYPE_DETAILED, 
               sort = TRUE) %>% 
  distinct() %>% 
  remove_rownames()
ptswithcancer <- ptswithcancer %>% 
  dplyr::count(patientId,
               sort = TRUE) %>% 
  distinct() %>% 
  remove_rownames()

solepts <- ptswithcancer %>% 
  subset(n == 1) %>% 
  remove_rownames()
solepts <- solepts[order(-solepts$n),] %>% 
  remove_rownames()
p53mutssolecancer <- p53mutscancersover100 %>% 
  subset(patientId %in% solepts$patientId) %>% 
  distinct()
all(p53mutssolecancer$patientId %in% solepts$patientId) # should be TRUE in console

multpts <- ptswithcancer %>% 
  subset(n > 1) %>% 
  remove_rownames()
multpts <- multpts[order(-multpts$n),] %>% 
  remove_rownames()
p53mutsmultcancer <- p53mutscancersover100 %>% 
  subset(patientId %in% multpts$patientId) %>% 
  distinct()
all(p53mutsmultcancer$patientId %in% multpts$patientId) # should be TRUE in console

# not all multpts actually have mult cancers because called diff cancers in diff studies where same pts repeated, but since can't change cancer types listed b/c don't know which one is correct, inevitably counted mult times

# not worth doing separate mult vs sole analyses b/c most of mult not actually mult and are so few

# however, since most key analyses done on p53mutsover100 df, just make that df have only sole pts to match better with germline
p53mutscancersover100 <- p53mutscancersover100 %>% 
  subset(patientId %in% solepts$patientId) %>% 
  remove_empty("cols") %>% 
  distinct()

p53muts <- p53muts %>% 
  subset(patientId %in% solepts$patientId) %>% 
  remove_empty("cols") %>% 
  distinct()

# need to add mut info to clindata for future calcs of muts out of all possible somatic cancers
clindatacombined <- clindatacombined %>% # do by sample so that if mult cancers in pt, only mut marked
  mutate(p53mut = ifelse(sampleId %in% p53muts_forclindata$sampleId,
                         "yes", 
                         "no")) %>% 
  remove_empty("cols") %>%
  distinct()

# because # of rows changes when p53muts added to clindatacombined, keep as separate df just for specific pos calcs across all possible somatic cancers
clindatacombined_forpos <- full_join(clindatacombined, 
                                     p53muts_forclindata) %>% 
  remove_empty("cols") %>%
  distinct()

# save p53muts and clin data as tables and rds objects
write_csv(p53muts %>% 
            remove_empty("cols"), 
          file = paste0(table_path, 
                        "p53muts.csv"))

write_csv(p53mutscancersover100 %>% 
            remove_empty("cols"),
          file = paste0(table_path, 
                        "p53mutscancersover100.csv"))

write_csv(clindatacombined %>% 
            remove_empty("cols"),
          file = paste0(table_path, 
                        "clindatacombined.csv"))

write_csv(clindatacombined_forpos %>% 
            remove_empty("cols"),
          file = paste0(table_path, 
                        "clindatacombined_forpos.csv"))

saveRDS(p53muts %>% 
          remove_empty("cols"), 
        file = paste0(table_path, 
                      "p53muts.rds"))

saveRDS(p53mutscancersover100 %>% 
          remove_empty("cols"),
        file = paste0(table_path, 
                      "p53mutscancersover100.rds"))

saveRDS(clindatacombined %>% 
          remove_empty("cols"),
        file = paste0(table_path, 
                      "clindatacombined.rds"))

saveRDS(clindatacombined_forpos %>% 
          remove_empty("cols"),
        file = paste0(table_path, 
                      "clindatacombined_forpos.rds"))

# pull out damage info of muts in origfunction df, remove p. prefix, arrange by increasing codon, remove rows without damage information
origfunction_damageinfo <- origfunction %>% 
  dplyr::select(c(ProtDescription, # first few cols are mut info, then agvgdc onwards are damage info
                  Effect,
                  AGVGDClass,
                  BayesDel,
                  REVEL,
                  SIFTClass,
                  Polyphen2,
                  TransactivationClass,
                  DNE_LOFclass,
                  DNEclass,
                  StructureFunctionClass)) %>% 
  filter(!str_detect(ProtDescription,
                     "\\?")) %>% # must remove these because can't exactly match my muts
  filter(!str_detect(ProtDescription,
                     "delins")) %>% # don't have any info on damage muts
  filter(!str_detect(ProtDescription, 
                     "\\;")) %>% # double muts which can't be properly accounted for
  subset(Effect %in% c("missense",
                       "nonsense",
                       "silent") | ProtDescription %in% c("p.T125T",
                                                          "p.E224D",
                                                          "p.E224E",
                                                          "p.G187R",
                                                          "p.S261T",
                                                          "p.T125N")) %>% # can only keep these because they have fixed final residues/effects, adding muts with specified final residues in splice category so that not overlooked
  dplyr::select(-Effect) %>% # causing duplicated residues otherwise because at nucleotide level were diff
  distinct() %>% 
  remove_rownames()

origfunction_damageinfo$ProtDescription <- substring(origfunction_damageinfo$ProtDescription, 3)

# origfunction_damageinfo <- origfunction_damageinfo[!(origfunction_damageinfo$DNE_LOFclass == "unclass." & is.na(origfunction_damageinfo$SIFTClass)),] # removes all that effectively had no damage information as unclass means nothing and sift class chosen as it had the next most information in unclass residues

origfunction_damageinfo <- origfunction_damageinfo %>% 
  arrange(abs(parse_number(ProtDescription))) %>% 
  remove_rownames() 
# %>% 
# remove_empty("rows")

# get number of studies that final list of 12793 patients come from

# identify studies with same suffixes and thus potentially duplicate studies
(p53muts$studyId %>% 
    unique() %>% 
    gsub("^.*?_", # remove characters before first underscore so that study suffix is all that remains
         "", 
         .) %>% 
    sort() %>% 
    table())[(p53muts$studyId %>% 
                unique() %>% 
                gsub("^.*?_", 
                     "", 
                     .) %>% 
                sort() %>%
                table()) > 1]

# get number of studies
p53muts$studyId %>% 
  unique() %>% # gives 67 studies but many are just cancers from same 2018 tcga study
  gsub("^.*?_", # remove characters before first underscore so that study suffix is all that remains
       "", 
       .) %>% 
  unique() %>% # to collapse all 31 cancers of 2018 tcga study down to 1 study (so -30)
  n_distinct() + 1 # 37 (+ 1 because mskcc suffix is for 2 different studies but collapsed into 1 in unique)

# get number of distinct studies in nonoverlapping

# identify studies with same suffixes and thus potentially duplicate studies
(unlist(nonoverlapping) %>% 
    unique() %>% 
    gsub("^.*?_", # remove characters before first underscore so that study suffix is all that remains
         "", 
         .) %>% 
    sort() %>% 
    table())[(unlist(nonoverlapping) %>% 
                unique() %>% 
                gsub("^.*?_", 
                     "", 
                     .) %>% 
                sort() %>%
                table()) > 1]

# get number of studies
unlist(nonoverlapping) %>% 
    n_distinct() - 4 - 31 # TARGET has 5 overall (including phase II) and TCGA has 32 overall so need to collapse to 1 each
```

$~$


``` r
#graph table as a plot and save
cancersover100grid <- tableGrob(cancersover100withtotal, 
                                rows = NULL, 
                                theme = ttheme_default(base_size = 10, 
                                                       core = list(padding = unit(c(2, 2), 
                                                                                  "mm"))))
cancersgrid <- grid.arrange(cancersover100grid)
```

<img src="figures/cancersover100-grid-1.svg" alt="" width="100%" height="100%" style="display: block; margin: auto;" />

$~$

# **Generating Pivot Tables and Calculations** {#pivotsandcalc}

$~$

[]

$~$


``` r
#make pivot table based on num pts per study, sorted by descending counts for rows and columns
studyptspivot <- PivotTable$new()
studyptspivot$addData(p53muts)
studyptspivot$addRowDataGroups("studyId")
studyptspivot$defineCalculation(calculationName = "Count of distinct individuals", 
                                summariseExpression = "n_distinct(patientId)")
studyptspivot$sortRowDataGroups(levelNumber = 1,
                                orderBy = "calculation", 
                                sortOrder = "desc")
studyptspivot$evaluatePivot()

#make pivot table as dataframe for further calculations
pivotdf_studypts <- studyptspivot$asDataFrame(rowGroupsAsColumns = TRUE) %>% 
  remove_rownames()

#make pivot table based on mutation type, sorted by descending counts for rows and columns
muttypepivot <- PivotTable$new()
muttypepivot$addData(p53mutscancersover100)
muttypepivot$addRowDataGroups("CANCER_TYPE_DETAILED")
muttypepivot$addColumnDataGroups("mutationType")
muttypepivot$defineCalculation(calculationName = "Count of distinct individuals", 
                               summariseExpression = "n_distinct(patientId)")
muttypepivot$sortColumnDataGroups(levelNumber = 1, 
                                  orderBy = "calculation", 
                                  sortOrder = "desc")
muttypepivot$sortRowDataGroups(levelNumber = 1,
                               orderBy = "calculation", 
                               sortOrder = "desc")
muttypepivot$evaluatePivot()

#make pivot table as dataframe for further calculations
pivotdf <- muttypepivot$asDataFrame(rowGroupsAsColumns = TRUE) %>% 
  remove_rownames()

# make pivot table on prevalence of p53 muts across all somatic cancers

# don't really need this pivot since i can just keep total row of per cancer for overall proportion
# mutsomatic <- PivotTable$new()
# mutsomatic$addData(clindatacombined)
# mutsomatic$addRowDataGroups("p53mut")
# mutsomatic$defineCalculation(calculationName = "Count of distinct individuals", 
#                                    summariseExpression = "n_distinct(patientId)")
# mutsomatic$sortColumnDataGroups(levelNumber = 1, 
#                                       orderBy = "calculation", 
#                                       sortOrder = "desc")
# mutsomatic$sortRowDataGroups(levelNumber = 1,
#                                    orderBy = "calculation", 
#                                    sortOrder = "desc")
# mutsomatic$evaluatePivot()

muttissuesomatic <- PivotTable$new()
muttissuesomatic$addData(clindatacombined)
muttissuesomatic$addRowDataGroups("CANCER_TYPE_DETAILED")
muttissuesomatic$addColumnDataGroups("p53mut")
muttissuesomatic$defineCalculation(calculationName = "Count of distinct individuals", 
                                   summariseExpression = "n_distinct(patientId)")
muttissuesomatic$sortColumnDataGroups(levelNumber = 1, 
                                      orderBy = "calculation", 
                                      sortOrder = "desc")
muttissuesomatic$sortRowDataGroups(levelNumber = 1,
                                   orderBy = "calculation", 
                                   sortOrder = "desc")
muttissuesomatic$evaluatePivot()

# need to get actual mut prevalence across all somatic cancers
# pivottabler takes too long; make pivot table using dplyr, widen table to make cols = muts, add totals
mutpossomaticpivot <- clindatacombined_forpos %>% 
  group_by(CANCER_TYPE_DETAILED, 
           proteinChange) %>%
  summarise(n = n_distinct(patientId)) %>%
  arrange(-n)
mutpossomaticpivot <- pivot_wider(mutpossomaticpivot, 
                                  names_from = proteinChange, 
                                  values_from = n) 

# can't adorn_totals because it just adds rows and columns up, doesn't do distinct indiv like pivottabler, so need to make 1 row/col dfs of each distinct number of individuals for each position and cancer and add to pivot
# first getting distinct indivs for each mut and adding to pivot df
zed <- lapply(colnames(mutpossomaticpivot), 
              FUN = function(x) clindatacombined_forpos %>%
                subset(proteinChange == x) %>% 
                pull(patientId) %>% 
                n_distinct())
names(zed) <- colnames(mutpossomaticpivot) # ensure list names are muts
zed <- reshape2::melt(zed) %>% 
  column_to_rownames("L1") %>% 
  as.matrix() %>% 
  t() %>% 
  as.data.frame() %>% 
  remove_rownames()
zed[1,1] <- "Total" # rename 0 placeholder as Total

pivotdfpos_somatic <- bind_rows(mutpossomaticpivot,
                                zed)

# now getting distinct indivs for each cancer and adding to pivot df
zed <- lapply(mutpossomaticpivot$CANCER_TYPE_DETAILED,
              FUN = function(x) clindatacombined_forpos %>%
                subset(CANCER_TYPE_DETAILED == x) %>% 
                pull(patientId) %>% 
                n_distinct())
names(zed) <- mutpossomaticpivot$CANCER_TYPE_DETAILED
zed <- reshape2::melt(zed) %>% 
  dplyr::rename(CANCER_TYPE_DETAILED = L1,
                Total = value)

pivotdfpos_somatic <- full_join(pivotdfpos_somatic,
                                zed,
                                by = "CANCER_TYPE_DETAILED")

# add total distinct indivs as final df total
pivotdfpos_somatic[nrow(pivotdfpos_somatic),ncol(pivotdfpos_somatic)] <- n_distinct(clindatacombined_forpos$patientId)

# add num of pts who dont have mut
pivotdfpos_somatic[nrow(pivotdfpos_somatic),"NA"] <- (setdiff(unique(clindatacombined_forpos$patientId), 
                                                              unique(p53muts$patientId)) %>%
                                                        length())

# add num pts per cancer/total to cancer col, divide across for prop of muts per cancer/total, remove total col and rownames
pivotdfpos_somatic_calc <- pivotdfpos_somatic %>% 
  subset(Total >= 50) %>% # not removing bottom total since we want that as part of barchart
  mutate(CANCER_TYPE_DETAILED = paste0(CANCER_TYPE_DETAILED,
                                       " (",
                                       format(Total, 
                                              big.mark = ",", 
                                              trim = TRUE),
                                       " individuals)")) %>%
  mutate_at(vars(2:Total), 
            .funs = ~./Total) %>% 
  subset(select = -c(Total)) %>%
  remove_rownames()

# melt for tables, barcharts, heatmaps later on
pivotdfpos_somatic_melted <- pivotdfpos_somatic_calc %>%
  reshape2::melt() %>% 
  dplyr::rename(Mutation = variable, 
                Cancer = CANCER_TYPE_DETAILED, 
                Proportion = value)

# make pivot tables as dataframes for further calculations
# pivotdf_mutsomatic <- mutsomatic$asDataFrame(rowGroupsAsColumns = TRUE) %>% 
#   remove_rownames()

pivotdf_muttissuesomatic <- muttissuesomatic$asDataFrame(rowGroupsAsColumns = TRUE) %>% 
  remove_rownames()

# #add counts of cancer distinct samples to help to divide pivot table rows by # distinct samples
# cancersover100used <- cancersover100 %>% 
#   subset(cancersover100$CANCER_TYPE_DETAILED %in% pivotdf$CANCER_TYPE_DETAILED)
# pivotdfnobottomtotal <- pivotdf %>% 
#   subset(CANCER_TYPE_DETAILED != "Total")
# pivotdfnobottomtotal <- merge(pivotdfnobottomtotal, 
#                               cancersover100used, 
#                               by = "CANCER_TYPE_DETAILED")
# 
# #check correct joining of distinct sample counts to pivot table by concatenating key columns
# testagain <- as.list(paste0(cancersover100used$CANCER_TYPE_DETAILED, 
#                             cancersover100used$count))
# testagain2 <- as.list(paste0(pivotdfnobottomtotal$CANCER_TYPE_DETAILED, 
#                              pivotdfnobottomtotal$count))
# all(testagain2 %in% testagain) #should be TRUE if joined correctly as it checks if all pivot columns exactly in main
# 
# #divide pivot table rows by # distinct samples
# pivotdfnobottomtotal <- pivotdfnobottomtotal %>%
#   mutate_at(vars(2:Total), 
#             .funs = ~./count)
# 
# #remove counts and total columns so that they're not included in heatmap or any other visualization
# pivotdfnobottomtotal <- pivotdfnobottomtotal %>% 
#   subset(select = -c(count, 
#                      Total)) %>% 
#   remove_rownames()

#make heatmap with ggplot2

# #make dataframe into long form for ggplot2 
# data_melt <- reshape2::melt(pivotdfnobottomtotal) %>% 
#   dplyr::rename(Mutation = variable,
#                 Cancer = CANCER_TYPE_DETAILED, 
#                 Prevalence = value)
# head(data_melt)
# 
# #remove mutations with less than 1% prevalence in distinct samples
# data_meltover0.01 <- data_melt %>% 
#   subset(Prevalence >= 0.01)
# 
# #sort cancers by lowest to highest missense mutation prevalence for heatmap
# missensemutorder = data_meltover0.01[data_meltover0.01$Mutation == 'Missense_Mutation',]
# data_meltover0.01$Cancer = factor(data_meltover0.01$Cancer, 
#                                   levels = missensemutorder$Cancer[order(missensemutorder$Prevalence)])

#make 100% stacked barchart based on cancers with at least 100 indivs
#remove bottom total row again
pivotdfnobottomtotal100muts <- pivotdf %>% 
  subset(CANCER_TYPE_DETAILED != "Total")

#subset dataframe to cancers with at least 100 indivs ; not changing object name
pivotdfnobottomtotal100muts <- pivotdfnobottomtotal100muts %>% 
  subset(Total >= 50) %>%
  mutate(CANCER_TYPE_DETAILED = paste0(CANCER_TYPE_DETAILED,
                                       " (",
                                       format(Total, 
                                              big.mark = ",", 
                                              trim = TRUE),
                                       " individuals)"))

pivotdf_muttissuesomatic100muts <- pivotdf_muttissuesomatic %>% 
  subset(Total >= 50) %>% # not removing bottom total since we want that as part of barchart
  mutate(CANCER_TYPE_DETAILED = paste0(CANCER_TYPE_DETAILED,
                                       " (",
                                       format(Total, 
                                              big.mark = ",", 
                                              trim = TRUE),
                                       " individuals)"))

#divide each column by total # indiv
pivotdfnobottomtotal100muts <- pivotdfnobottomtotal100muts %>%
  mutate_at(vars(2:Total), 
            .funs = ~./Total)

pivotdf_muttissuesomatic100muts <- pivotdf_muttissuesomatic100muts %>%
  mutate_at(vars(2:Total), 
            .funs = ~./Total)

#remove total column so it's not included in barchart
pivotdfnobottomtotal100muts <- pivotdfnobottomtotal100muts %>% 
  subset(select = -c(Total)) %>%
  remove_rownames()

pivotdf_muttissuesomatic100muts <- pivotdf_muttissuesomatic100muts %>% 
  subset(select = -c(Total)) %>%
  remove_rownames()

#melt dataframe and rename columns to use for ggplot2 100% stacked barchart
data_barchart <- reshape2::melt(pivotdfnobottomtotal100muts) %>% 
  dplyr::rename(Mutation = variable, 
                Cancer = CANCER_TYPE_DETAILED, 
                Proportion = value)
head(data_barchart)
```

<div class="kable-table">

|Cancer                                               |Mutation          | Proportion|
|:----------------------------------------------------|:-----------------|----------:|
|Colorectal Adenocarcinoma (3,855 individuals)        |Missense_Mutation |  0.6573281|
|Non-Small Cell Lung Cancer (3,698 individuals)       |Missense_Mutation |  0.6471065|
|Breast Invasive Ductal Carcinoma (2,106 individuals) |Missense_Mutation |  0.5655271|
|Pancreatic Adenocarcinoma (1,970 individuals)        |Missense_Mutation |  0.6329949|
|Hepatocellular Carcinoma (792 individuals)           |Missense_Mutation |  0.6641414|
|Gastric Adenocarcinoma (713 individuals)             |Missense_Mutation |  0.6016830|

</div>

``` r
data_muttissuesomatic <- reshape2::melt(pivotdf_muttissuesomatic100muts) %>% 
  dplyr::rename(Mutation = variable, 
                Cancer = CANCER_TYPE_DETAILED, 
                Proportion = value)

#sort cancers by lowest to highest missense mutation proportion for stacked barchart
missensemutorderbarchart = data_barchart[data_barchart$Mutation == 'Missense_Mutation',]
data_barchart$Cancer = factor(data_barchart$Cancer, 
                              levels = missensemutorderbarchart$Cancer[order(missensemutorderbarchart$Proportion)])

# call low-frequency cancers as other for subsequent pivot tables and work
p53mutscancersover100 <- p53mutscancersover100 %>% 
  transform(Cancer_Label = ifelse(CANCER_TYPE_DETAILED %in% gsub(" \\(.*", 
                                                                 "",
                                                                 pivotdfnobottomtotal100muts$CANCER_TYPE_DETAILED),
                                  CANCER_TYPE_DETAILED, 
                                  "OTHER"))

# save table and object
write_csv(p53mutscancersover100,
          file = paste0(table_path, 
                        "p53mutscancersover100.csv"))

saveRDS(p53mutscancersover100,
        file = paste0(table_path, 
                      "p53mutscancersover100.rds"))

#make pivot tables based on mutation position, sorted by descending counts for rows and columns
# mutpospivot <- PivotTable$new()
# mutpospivot$addData(p53mutscancersover100)
# mutpospivot$addRowDataGroups("CANCER_TYPE_DETAILED")
# mutpospivot$addColumnDataGroups("proteinChange")
# mutpospivot$defineCalculation(calculationName = "Count of distinct individuals",
#                               summariseExpression = "n_distinct(patientId)")
# mutpospivot$sortColumnDataGroups(levelNumber = 1, 
#                                  orderBy = "calculation",
#                                  sortOrder = "desc")
# mutpospivot$sortRowDataGroups(levelNumber = 1, 
#                               orderBy = "calculation", 
#                               sortOrder = "desc")
# mutpospivot$evaluatePivot()

#make pivot table as dataframe for further calculations
# pivotdfpos <- mutpospivot$asDataFrame(rowGroupsAsColumns = TRUE) %>% remove_rownames()

# because pivottabler taking too long, make pivot table using dplyr, widen table to make cols = muts, add totals
mutpospivot <- p53mutscancersover100 %>% 
  group_by(CANCER_TYPE_DETAILED, 
           proteinChange) %>%
  summarise(n = n_distinct(patientId)) %>%
  arrange(-n)
mutpospivot <- pivot_wider(mutpospivot, 
                           names_from = proteinChange, 
                           values_from = n) 

# can't adorn_totals because it just adds rows and columns up, doesn't do distinct indiv like pivottabler, so need to make 1 row/col dfs of each distinct number of individuals for each position and cancer and add to pivot
# first getting distinct indivs for each mut and adding to pivot df
zed <- lapply(colnames(mutpospivot), 
              FUN = function(x) p53mutscancersover100 %>%
                subset(proteinChange == x) %>% 
                pull(patientId) %>% 
                n_distinct())
names(zed) <- colnames(mutpospivot) # ensure list names are muts
zed <- reshape2::melt(zed) %>% 
  column_to_rownames("L1") %>% 
  as.matrix() %>% 
  t() %>% 
  as.data.frame() %>% 
  remove_rownames()
zed[1,1] <- "Total" # rename 0 placeholder as Total

pivotdfpos <- bind_rows(mutpospivot,
                        zed)

# now getting distinct indivs for each cancer and adding to pivot df
zed <- lapply(mutpospivot$CANCER_TYPE_DETAILED,
              FUN = function(x) p53mutscancersover100 %>%
                subset(CANCER_TYPE_DETAILED == x) %>% 
                pull(patientId) %>% 
                n_distinct())
names(zed) <- mutpospivot$CANCER_TYPE_DETAILED
zed <- reshape2::melt(zed) %>% 
  dplyr::rename(CANCER_TYPE_DETAILED = L1,
                Total = value)

pivotdfpos <- full_join(pivotdfpos,
                        zed,
                        by = "CANCER_TYPE_DETAILED")

# although not necessary, can add total distinct indivs as final df total
pivotdfpos[nrow(pivotdfpos),ncol(pivotdfpos)] <- n_distinct(p53mutscancersover100$patientId)

# get mut pos of pts with less prevalent cancers

# prepared it this way as well just in case pivottabler becomes unusuably slow later on
# mutposotherpivot <- p53mutscancersover100 %>% 
#   subset(Cancer_Label == "OTHER") %>% 
#   group_by(CANCER_TYPE_DETAILED, 
#            proteinChange) %>%
#   summarise(n = n_distinct(patientId)) %>%
#   arrange(-n)
# mutposotherpivot <- pivot_wider(mutposotherpivot, 
#                            names_from = proteinChange, 
#                            values_from = n) 
# 
# # can't adorn_totals because it just adds rows and columns up, doesn't do distinct indiv like pivottabler, so need to make 1 row/col dfs of each distinct number of individuals for each posotherition and cancer and add to pivot
# # first getting distinct indivs for each mut and adding to pivot df
# zed <- lapply(colnames(mutposotherpivot), 
#               FUN = function(x) p53mutscancersover100 %>%
#                 subset(Cancer_Label == "OTHER") %>% 
#                 subset(proteinChange == x) %>% 
#                 pull(patientId) %>% 
#                 n_distinct())
# names(zed) <- colnames(mutposotherpivot) # ensure list names are muts
# zed <- reshape2::melt(zed) %>% 
#   column_to_rownames("L1") %>% 
#   as.matrix() %>% 
#   t() %>% 
#   as.data.frame() %>% 
#   remove_rownames()
# zed[1,1] <- "Total" # rename 0 placeholder as Total
# 
# pivotdfposother <- bind_rows(mutposotherpivot,
#                         zed)
# 
# # now getting distinct indivs for each cancer and adding to pivot df
# zed <- lapply(mutposotherpivot$CANCER_TYPE_DETAILED,
#               FUN = function(x) p53mutscancersover100 %>%
#                 subset(Cancer_Label == "OTHER") %>% 
#                 subset(CANCER_TYPE_DETAILED == x) %>% 
#                 pull(patientId) %>% 
#                 n_distinct())
# names(zed) <- mutposotherpivot$CANCER_TYPE_DETAILED
# zed <- reshape2::melt(zed) %>% 
#   dplyr::rename(CANCER_TYPE_DETAILED = L1,
#                 Total = value)
# 
# pivotdfposother <- full_join(pivotdfposother,
#                         zed,
#                         by = "CANCER_TYPE_DETAILED")
# 
# # although not necessary, can add total distinct indivs as final df total
# pivotdfposother[nrow(pivotdfposother),ncol(pivotdfposother)] <- n_distinct(
#   p53mutscancersover100 
#   %>% subset(Cancer_Label == "OTHER") %>% 
#     pull(patientId)
#   )

mutposotherpivot <- PivotTable$new()
mutposotherpivot$addData(p53mutscancersover100 %>% subset(Cancer_Label == "OTHER"))
mutposotherpivot$addRowDataGroups("CANCER_TYPE_DETAILED")
mutposotherpivot$addColumnDataGroups("proteinChange")
mutposotherpivot$defineCalculation(calculationName = "Count of distinct individuals",
                                   summariseExpression = "n_distinct(patientId)")
mutposotherpivot$sortColumnDataGroups(levelNumber = 1,
                                      orderBy = "calculation",
                                      sortOrder = "desc")
mutposotherpivot$sortRowDataGroups(levelNumber = 1,
                                   orderBy = "calculation",
                                   sortOrder = "desc")
mutposotherpivot$evaluatePivot()

# make pivot table as dataframe for further calculations
pivotdfposother <- mutposotherpivot$asDataFrame(rowGroupsAsColumns = TRUE) %>% remove_rownames()

# change T125= to T125T for grid
colnames(pivotdfposother) <- gsub("T125=",
                                  "T125T",
                                  colnames(pivotdfposother))

#make 100% stacked barchart based on cancers with at least 100 indivs
#remove bottom total row again
pivotdfposnobottomtotal <- pivotdfpos %>% 
  subset(CANCER_TYPE_DETAILED != "Total")

#subset to cancers with at least 100 total p53 indivs; not changing object name, add # indivs to cancers
pivotdfposnobottomtotal100muts <- pivotdfposnobottomtotal %>% 
  subset(Total >= 50) %>%
  mutate(CANCER_TYPE_DETAILED = paste0(CANCER_TYPE_DETAILED,
                                       " (",
                                       format(Total, 
                                              big.mark = ",", 
                                              trim = TRUE),
                                       " individuals)"))

#divide pivot table rows by # distinct indiv
pivotdfposnobottomtotal100muts <- pivotdfposnobottomtotal100muts %>%
  mutate_at(vars(2:Total), 
            .funs = ~./Total)

#remove total column so that not included in heatmap or any other visualization
pivotdfposnobottomtotal100muts <- pivotdfposnobottomtotal100muts %>% 
  