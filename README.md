# EVA
Code accompanying the Short Course  "An introduction to extreme-value analysis, with applications to climate data", given by Johan Segers (KU Leuven) and Anna Kiriliouk (UNamur) at UCLouvain on Oct 24-25, 2024

# Wind.RData
Loads three objects : an (n x d) matrix of observations, a (d x 2) matrix of coordinates and a vector of dates

Daily maximal speeds of wind gusts, measured in km/h, observed at d = 35 weather stations in the Netherlands during extended winter (October–March), from October 2001 up to and including March 2022. The data set is freely available from the Royal Netherlands Meteorological Institute (KNMI), https://climexp.knmi.nl/, and has been analyzed in Kiriliouk & Zhou (2024).

# Rain.RData
Loads three objects : an (n x d) matrix of observations, a (d x 2) matrix of coordinates (in kms, to be removed) and a vector of dates

Daily precipitation amounts in millimeters for June, July, August of the years 1962-2012 for d = 44 stations around Zurich, Switzerland. The dataset (from MétéoSuisse) has been analyzed in Thibaud & Opitz (2015) and Cooley & Thibaud (2O19), among others. 

# Temp.RData
Loads three objects:  an (n x d) matrix of observations, a (d x 2) matrix of coordinates and a vector of years

Annual maxima of daily maximum temperatures for the years 1950-2018 for d = 54 stations around Belgium. Derived from the homogenized E-OBS dataset E-OBS v19.0eHOM data, made available in the supplementary material of Auld et al. (2023).

# CO2.RData
An (n x 2) matrix, whose first column represents years and whose second column represents the average global atmospheric CO2 concentration in each year. Pre-industrial level: around 280 (to be explained more).
