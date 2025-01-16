# Analiza statystyczna danych
Repozytorium zawiera materiały z wykładów ze Statystycznej analizy danych:
- prezentacja (plik StatLec1Z_2024.pdf)
- skrypty R (pliki lectures_partI.R i lectures_partII.R)
- dodatkowe dane niezbędne do wykonania fragmentów kodu z uczenia maszynowego (folder "dane")
<br>
<br>


> sessionInfo()
R version 4.3.2 (2023-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 11 x64 (build 22621)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

time zone: Europe/Warsaw
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] tmap_3.99.9000     spdep_1.3-1        spData_2.3.0       sf_1.0-15          raster_3.6-26      sp_2.1-2           astsa_2.0          tseries_0.10-55    moments_0.14.1    
[10] ineq_0.2-13        pollster_0.1.6     knitr_1.45         epiDisplay_3.5.0.2 nnet_7.3-19        MASS_7.3-60        survival_3.5-7     foreign_0.8-85     viridis_0.6.4     
[19] viridisLite_0.4.2  hrbrthemes_0.8.0   babynames_1.0.1    lubridate_1.9.3    forcats_1.0.0      stringr_1.5.1      dplyr_1.1.4        purrr_1.0.2        readr_2.1.4       
[28] tidyr_1.3.0        tibble_3.2.1       ggplot2_3.4.4      tidyverse_2.0.0   

loaded via a namespace (and not attached):
  [1] RColorBrewer_1.1-3      wk_0.9.1                rstudioapi_0.15.0       jsonlite_1.8.8          magrittr_2.0.3          modeltools_0.2-23       farver_2.1.1           
  [8] rmarkdown_2.25          vctrs_0.6.5             base64enc_0.1-3         terra_1.7-65            rstatix_0.7.2           leafsync_0.1.0          htmltools_0.5.7        
 [15] curl_5.2.0              haven_2.5.4             broom_1.0.5             s2_1.1.6                TTR_0.24.4              KernSmooth_2.23-22      htmlwidgets_1.6.4      
 [22] stars_0.6-4             zoo_1.8-12              mime_0.12               lifecycle_1.0.4         pkgconfig_2.0.3         cols4all_0.7            Matrix_1.6-4           
 [29] R6_2.5.1                fastmap_1.1.1           widgetframe_0.3.1       shiny_1.8.0             digest_0.6.33           colorspace_2.1-0        patchwork_1.1.3        
 [36] leafem_0.2.3            crosstalk_1.2.1         ggpubr_0.6.0            lwgeom_0.2-13           labeling_0.4.3          fansi_1.0.5             timechange_0.2.0       
 [43] abind_1.4-5             compiler_4.3.2          proxy_0.4-27            fontquiver_0.2.1        withr_2.5.2             backports_1.4.1         carData_3.0-5          
 [50] DBI_1.2.0               Rttf2pt1_1.3.12         ggsignif_0.6.4          tmaptools_3.1-1         leaflet_2.2.1           classInt_0.4-10         gfonts_0.2.0           
 [57] tools_4.3.2             units_0.8-5             prabclus_2.3-3          leaflegend_1.1.1        quantmod_0.4.25         httpuv_1.6.13           extrafontdb_1.0        
 [64] glue_1.6.2              quadprog_1.5-8          promises_1.2.1          grid_4.3.2              rsconnect_1.2.0         cluster_2.1.4           generics_0.1.3         
 [71] gtable_0.3.4            labelled_2.12.0         tzdb_0.4.0              class_7.3-22            data.table_1.14.10      hms_1.1.3               car_3.1-2              
 [78] utf8_1.2.4              flexmix_2.3-19          pillar_1.9.0            later_1.3.2             robustbase_0.99-1       splines_4.3.2           lattice_0.21-9         
 [85] deldir_2.0-2            tidyselect_1.2.0        fontLiberation_0.1.0    fontBitstreamVera_0.1.1 gridExtra_2.3           crul_1.4.0              stats4_4.3.2           
 [92] xfun_0.41               diptest_0.77-0          DEoptimR_1.1-3          stringi_1.8.3           boot_1.3-28.1           evaluate_0.23           codetools_0.2-19       
 [99] httpcode_0.3.0          kernlab_0.9-32          extrafont_0.19          gdtools_0.3.5           cli_3.6.1               xtable_1.8-4            systemfonts_1.0.5      
[106] munsell_0.5.0           dichromat_2.0-0.1       pscl_1.5.5.1            Rcpp_1.0.11             png_0.1-8               XML_3.99-0.16           parallel_4.3.2         
[113] ellipsis_0.3.2          mclust_6.0.1            scales_1.3.0            xts_0.13.1              e1071_1.7-14            crayon_1.5.2            fpc_2.2-11             
[120] rlang_1.1.2  
