# SBEDE-open
A Bayesian Model for Portfolio Decisions based on Debiased and Regularized Expert Predictions

Selective Bayesian expert debiasing (SBEDE) portfolio model is developed for an investor who wants aggregate multiple stock analyst return predictions and estimates the predictive posterior distribution of future stock return.

The data set for the illustration of the SBEDE portfolio model include one example industry with real stock returns and artificial expert predictions. The SBEDE model is coded with Stan, and R scripts for fitting the models and optimizing the portfolio with an example industry are also present.

## Current files
The process flow with explanations: run_models.R

The SBEDE model coded with Stan: SBEDE.stan

A simplified SBEDE model, where all experts are assumed to be exchangeable: EE_SBEDE.stan

An example data set where expert predictions are simulated: example_data_sim_CommunicationsEquipment.xlsx

Supportive functions for portfolio optimization with the Kelly criteria: functions_optimization_example.RData

