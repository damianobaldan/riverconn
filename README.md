# riverconn
git repository for R package riverconn

# Installation
devtools::install_github("damianobaldan/riverconn", build_vignettes = TRUE)

# Changelog
0.3.1 to 0.3.2  
* vignette updated

0.2.9 to 0.3.1  
* minor changes in d_index_calculation function to avoid dplyr::join printing messages to screen

0.2.8 to 0.2.9  
* minor changes in d_index_calculation documentation

0.2.7 to 0.2.8  
* minor changes in B_ij_fun and c_ij_fun

0.2.6 to 0.2.7  
* changed architecture of B_ij_fun to improve speed (now relying on dodgr package to speed up shortest paths calculations on graphs)

0.2.5 to 0.2.6  
* changed architecture of c_ij_fun to improve speed (now relying on dodgr package to speed up shortest paths calculations on graphs)
* B_ij_fun is still the slow version

0.2.4 to 0.2.5  
* edited vignette

0.2.3 to 0.2.4  
* fixed bug on t_index_calculation (... argument and partial match issue)
* added error message on index_calculation when nodes_id is not charachter type

0.2.2 to 0.2.3  
* fixed bug on d_index_calculation (inputs pass_u and pass_d not recognized)

0.2.1 to 0.2.2  
* minor changes to functions documentation and error messages
* fixed bug on d_index_calculation (... argument and partial match issue)

0.1.9 to 0.2.1  
* changed architecture of d_index_calculation to simplify debug  
* introduced and documented function one_barrier_removal_index

0.1.8 to 0.1.9  
* fixed bug on d_index_calculation

0.1.7 to 0.1.8  
* fixed bug on set_c_directionality and set_B_directionality when argument "asymmetric" is provided  
* fixed issue on index_calculation and d_index_calculation: now output labels are consistent with user-inputted labels

0.1.7  
* first version on GitHub
