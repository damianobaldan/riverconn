# riverconn
git repository for R package riverconn

# Installation
devtools::install_github("damianobaldan/riverconn", build_vignettes = TRUE)

# Changelog
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
