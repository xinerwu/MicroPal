# Creates MAT models for environmental variables from the n=1968 database and 
# run predictions.
# Notes: to view source code eg: getAnywhere(reconPlot.predict.mat)
library(analogue)

dino <- read.delim('dino1968.txt',row.names=1)
envi <- read.delim('envi1968.txt',row.names=1)
core <- read.delim('dino1968.txt',row.names=1)
n.analogues <- 5
output_file_name <- 'test_predictions.csv'

# Optional: Specify environmental variables to be reconstructed
# eg.: env_id <- 2:18
# reconstruct all variables by default
env_id <- 1:ncol(envi)

# Manually apply log transformation 
# because log-distance isn't one of the built-in methods
modern <- log(dino+1)
fossil <- log(core+1)

# Set output directory
output_dir <- './MATrecons'
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
output_file <- file.path(output_dir, output_file_name)

# Check if models already exist
model_file <- 'MATmodels1968.RData'
if (file.exists(model_file)) {
  # Load the existing data
  load(model_file)
  print("Loaded existing MAT models.")
} else {
  # Fit MAT model
  mat_models <- vector("list",length=length(env_id))
  names(mat_models) <- colnames(envi)[env_id]
  # loop through each environmental variable
  c=1
  for (i in env_id) {
    env <- envi[, i]
    mat_model <- mat(modern,env,method="euclidean",k=n.analogues)
    mat_models[[c]] <- mat_model
    c=c+1
  }
  save(mat_models,names,file=model_file)
  print("Created MAT models.")
}
pred_list <- lapply(mat_models,function(model) predict(model,fossil)$predictions$model$predicted[n.analogues, ])
pred_matrix <- do.call(cbind,pred_list)
colnames(pred_matrix) <- names(mat_models)
write.csv(pred_matrix, file = output_file)
print(paste("Reconstructions saved to",output_file))
depths <- as.numeric(rownames(core))
par(mfrow = c(2, 2))
reconPlot(pred_list$Ssummer, depths = depths, ylab = "Summer SSS (psu)", xlab = "Samples")
reconPlot(pred_list$Tsummer, depths = depths, ylab = "Summer SST (degC)", xlab = "Samples")
reconPlot(pred_list$SeaIcemonths, depths = depths, ylab = "Sea ice cover (month/year)", xlab = "Samples")
reconPlot(pred_list$gCYC0217, depths = depths, ylab = "Annual primary productivity", xlab = "Samples")
print("Prepared plots for preview.")
