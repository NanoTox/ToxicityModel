# NanoTox: Toxicity Modelling

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4055281.svg)](https://doi.org/10.5281/zenodo.4055281)

Computational hazard assessment of MeOx nanoparticle toxicity using machine learning

Code files:
----------
1. modelling.R: code for the core machine learning 
2. vif.R: code for variance inflation factor analysis
3. normalise.R: code for normalisation
4. ApplicabilityDomain.R: code for applicability domain analysis
5. deployment.R: code for prediction of new instances

Data:
----
1. dataset.txt: dataset used in the study
2. model_\*\*\*.RDS: RDS objects of the machine learning models created (namely Logistic, RandomForest, SVM, Neural Nets)
3. normalised_train_data.Rdata: R object of the normalised train data used in the study.

Citation:
--------
__Nilesh Anantha Subramanian and Ashok Palaniappan__. NanoTox: Development of a Parsimonious In Silico Model for Toxicity Assessment of Metal-Oxide Nanoparticles Using Physicochemical Features. __ACS Omega__ Article ASAP __DOI__: [10.1021/acsomega.1c01076](https://pubs.acs.org/doi/10.1021/acsomega.1c01076)

Correspondence: apalania@scbt.sastra.edu (A.P.)

Copyright (c) 2020 the Authors. MIT License


