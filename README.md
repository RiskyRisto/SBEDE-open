# SBEDE-open
A Bayesian Model for Portfolio Decisions based on Debiased and Regularized Expert Predictions

The selective Bayesian expert debiasing (SBEDE) portfolio model has been developed for an investor who wants aggregate multiple external experts' (e.g. stock analysts) return predictions and estimate the predictive posterior distribution of future stock return.

The data set for the illustration of the SBEDE portfolio model include one example industry with real stock returns and artificial expert predictions. The SBEDE model is coded with Stan, and R scripts for fitting the models, optimizing the portfolio with an example industry and conducting a simulation study are also present.

## Current files
### Analysis of the example industry
The process flow with explanations: run_models.R
The SBEDE model coded with Stan: SBEDE.stan
A simplified SBEDE model, where all experts are assumed to be exchangeable: EE_SBEDE.stan
An example data set where expert predictions are simulated: example_data_sim_LifeHealthInsurance.xlsx
Supportive functions for portfolio optimization with the Kelly criteria: functions_optimization_example.RData

### Simulation study
Excel file for defining parameters for simulating data: parameter_values_for_sim.xlsx
R codes for simulating data and fitting Bayesian models: script_loop_gen_data_fit_models.R
R codes for optimizing portfolios and calculating performance metrics: script_optimize_performance.R
Supportive functions for scripts above: functions_simulator.R
