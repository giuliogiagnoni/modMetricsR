# modMetricsR
Evaluate models with observed and predictions from the model.

The main function is from https://animalnutrition.org/software and https://github.com/National-Animal-Nutrition-Program/Code-Examples/tree/master/Model-Evaluation.

To install run: `r devtools::install_github("giuliogiagnoni/modMetricsR", force = TRUE, build_vignettes = TRUE)`

It includes the functions:
- `r opmetrics`
- `r metricsloop`
- `r metricsloopmix`

Where:
- `r opmetrics` returns model evaluation for 2 vectors (observed and predicted), with also possibility of plotting.
- `r metricsloop` returns model evaluation for a serie of specified model, all belonging to the same model type. 
- `r metricsloopmix` returns model evaluation for a serie of specified model, that can belong to different model type. 


To cite the package type `r citation("modMetricsR")`
