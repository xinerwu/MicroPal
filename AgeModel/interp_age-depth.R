# This script allows to extract sample age corresponding to its depth
# through a linear interpolation, using median values
# Install packages if you haven't already:
#install.packages("pracma")
#install.packages("writexl")
#install.packages("rio")

bacon_file <- '/Users/xinerwu/Documents/Ateliers/MD2220_52_ages.txt'
sample_file <- '/Users/xinerwu/Documents/Ateliers/sample_depths.xlsx'

# Don't modify things below this line if you don't know what you are doing!
library(pracma)
library(writexl)
age_model <- rio::import(bacon_file,sheet=1)
depths <- rio::import(sample_file)
ages <- interp1(age_model$depth,age_model$median,depths$depth,
                method="linear")
depths$age <- ages
write_xlsx(depths,sample_file)