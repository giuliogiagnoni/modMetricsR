# modRMSE
Evaluate model with observed and predictions from model.

The main function is from https://animalnutrition.org/software and https://github.com/National-Animal-Nutrition-Program/Code-Examples/tree/master/Model-Evaluation.

To install run: devtools::install_github("giuliogiagnoni/modRMSE")

It includes the functions:
- RMSEvectors
- RMSEloop
- RMSEloopmix

Where:
- RMSEvectors returns model evaluation for 2 vectors.
- RMSEloop returns model evaluation for a serie of specified model, all belonging to the same model type. 
- RMSEloopmix returns model evaluation for a serie of specified model, that can belong to different model type. 
