# AIMS-Essay

The files contained in this repository constitute the social network analysis carried out in the work titled "A Dynamic Approach for Longitudinal Network Analysis: the Stochastic Actor-Oriented Model" by Emily Muller. This work was submitted in partial fulfillment of a structured masters degree at AIMS South Africa.

The files in the repository are described below:

Please note, I have removed the files 1) and 2) since I did not obtain consent from participants to host online. Please contact me if you wish to have access to the dataset (emily@aims.ac.za). 

1) DecemberCut.csv
This file contains the AIMS 2016/17 social network data for observation period December. The file codes individuals as numerics 0:41. For each individual, the file contains; County of Origin, Field of Study, Sex, Language, Appointed Supervisor and friendship nominations. The number of friendship nominations is between 0:8.

2) AprilCut.csv
As above for April period

3) PeriodII.R
This file reads the csv files into R as matrices and graphs (using the igraph package). It includes plots of the network, descriptive statistics for the network and histogram plots.

7) GlobalMetricPlots.R
This file plots the global metrics on each cross sectional network for December and April. 

4) RSienaII.R
This file implements the RSiena package on the networks created in PeriodII.R. It includes backward and forward selection steps. The algorithm produces 1000 network simulations as output.

5) DistancesII.R
This file applies the global difference metrics (described in the above essay) to the 1000 simulated networks and applies univariate difference tests. It includes graphs.

6) DistancesIIPlots.R
This file applies the local difference metrics (described in the above essay) to the 1000 simulated networks and applies multivariate difference tests. 

