# Creates MAT models for environmental variables from the n=1968 database and 
# run predictions.
# Notes: to view source code eg: getAnywhere(reconPlot.predict.mat)
library(analogue)

# Import modern reference species data file
spec <- read.delim('dino1968.txt',row.names=1)
# Import modern reference environmental variables file
envi <- read.delim('envi1968.txt',row.names=1)
# Import fossil species data file
core <- read.delim('testcore.txt',row.names=1)
# Name for output files:
output_file_name <- 'testcore'

# Change the number of analogues if wanted
n.analogues <- 5
# Choose the distance method
method <- "euclidean"
# Are you using dinocyst?
isDinocyst <- TRUE

# No bootstrapping by default, we use LOO for cross validation
# But if you do want to bootstrap, set bootstrap <- TRUE
# use n.boot=100 for testing/debugging, 1000 for formal analysis/publication
bootstrap <- FALSE
n.boot <- 100 

# Reconstruct all variables (could take time, depends on the performance of your computer):
# target_env <- colnames(envi)
# Or specify environmental variables to be reconstructed:
target_env <- c('Ssummer','Swinter','Tsummer','Twinter','SeaIceC','SeaIcemonths','gCYC0217')

# Name of the file containing MAT models that will be used/created
model_file <- 'MATmodels1968.RData'

################# No modification necessary below this line ####################
######################## Unless you know what to do ############################

# Temporarily increase the print limit
old_limit <- getOption("max.print")
options(max.print = 1000000)

# Manually apply log transformation if using dinocyst
# because log-distance isn't one of the built-in methods
if (isDinocyst) {
  modern <- log(spec+1)
  fossil <- log(core+1)
  # Remove duplicates in the dinocyst training database
  dup <- c("1101","1102","1100","1105")
  modern <- modern[ !rownames(modern) %in% dup, ]
  envi <- envi[ !rownames(envi) %in% dup, ]
} else {
  modern <- spec
  fossil <- core
}

# As this seems to occur pretty often, safety check for duplicates
if (any(duplicated(modern))){
  warning("There are duplicates in your calibration dataset (identical species 
rows). Use CheckDuplicates.R to inspect these rows. For now we just 
remove the repeated rows to be able to move forward.", call. = FALSE)
  dup_rows <- duplicated(modern)
  modern <- modern[!dup_rows, ]
  envi <- envi[!dup_rows, ]
}

# Set output directory
output_dir <- './MATrecons'
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
pattern <- file.path(output_dir, output_file_name)
output_file <- paste0(pattern,".csv")

# Check if models already exist
answer <- utils::askYesNo("Use existing MAT models (Yes) or create new (No)?")
if (file.exists(model_file) && isTRUE(answer)) {
  # Load the existing data
  load(model_file)
  message("Loaded existing MAT models.")
} else {
  # Fit MAT model
  message("Creating MAT models...")
  n_env=length(target_env)
  mat_models <- vector("list",length=n_env)
  names(mat_models) <- target_env
  
  pdf("Summary_MATmodels.pdf")
  sink(file = "Validation_MATmodels.txt")
  cat("========================================\n")
  cat("Model Validation Results\n")
  cat("Analysis Started at:", format(Sys.time(),"%Y-%m-%d %H:%M:%S"), "\n")
  cat("========================================\n\n")
  
  # loop through each environmental variable
  c=1
  for (i in 1:n_env) {
    env <- envi[[target_env[i]]]
    cat(paste("Variable:", target_env[i]), "\n")
    mat_model <- mat(modern,env,method=method,k=n.analogues*2,weighted=TRUE)
    mat_models[[c]] <- mat_model
    c=c+1
    par(mfrow = c(2, 2),oma = c(0, 0, 2, 0))
    plot(mat_model,sub.caption = target_env[i],weighted = TRUE,k=n.analogues)
    cat("The RMSEP values are from the leave-one-out (LOO) cross-validation.\n")
    print(mat_model)
    if (bootstrap) {
    boot<-bootstrap(mat_model,k=n.analogues,weighted = TRUE,n.boot = n.boot)
    cat(paste("Bootstrap-derived RMSEP using",n.analogues,"analogues and",n.boot,"bootstrap cycles is: ",RMSEP(boot, type="standard"),"\n\n\n"))
    }
  }
  
  save(mat_models,names,file=model_file)
  dev.off()
  sink()
  message("Created MAT models.")
  message("Saved model summary diagrams and validation results.")
}

# Extract the analogues and their distance
message("Processing fossil assemblages...")
ana <- analog(modern,fossil,method = method)
ana_file <- paste0(pattern,"_analogues.txt")
sink(file = ana_file)
# get a quick summary
print(summary(ana))
# create a better arranged matrix for post-processing
dist_mat <- ana$analogs
samples <- colnames(dist_mat)
sites <- rownames(dist_mat)
top_sites <- matrix(NA, nrow = length(samples), ncol = n.analogues, 
                     dimnames = list(samples, 1:n.analogues))
top_dists <- matrix(NA, nrow = length(samples), ncol = n.analogues, 
                     dimnames = list(samples, 1:n.analogues))
# loop through each sample (column) to find the n closest sites
for(i in seq_along(samples)) {
  current_dists <- dist_mat[, i]
  closest_idx <- order(current_dists)[1:n.analogues]
  top_sites[i, ] <- sites[closest_idx]
  top_dists[i, ] <- current_dists[closest_idx]
}
cat("\n\n=== Top n Closest Sites (Names) ===\n")
print(top_sites)
cat("\n\n=== Top n Closest Sites (Distances) ===\n")
print(top_dists)
sink()
message(paste("Saved close modern analogues to",ana_file))

# Run predictions for fossil samples and export the results
pred_list <- lapply(mat_models,function(model) predict(model,fossil,k=n.analogues,weighted=TRUE)$predictions$model$predicted[n.analogues, ])
pred_matrix <- do.call(cbind,pred_list)
colnames(pred_matrix) <- names(mat_models)
write.csv(pred_matrix, file = output_file)
message(paste("Reconstructions saved to",output_file))

# Extract the range of analogue env values
get_env_ranges <- function(fossil_row_dists,k,env_matrix){
  sorted_dists <- sort(fossil_row_dists)
  top_names <- names(sorted_dists)[1:k]
  # 'drop = FALSE' ensures it remains a matrix even if we only have 1 variable
  subset_env <- env_matrix[top_names, ,drop=FALSE]
  ranges <- apply(subset_env,2,range,na.rm=TRUE)
  return(ranges)
}
dists <- ana$analogs
results_list <- apply(dists,2,get_env_ranges,k=n.analogues,env_matrix=envi[,target_env],simplify = FALSE)
ranges_df <- do.call(rbind,lapply(results_list,as.vector))
ranges_colnames <- paste0(rep(target_env,each=2),"_",c("Min","Max"))
colnames(ranges_df) <- ranges_colnames
rownames(ranges_df) <- colnames(dists)
ranges_file <- paste0(pattern,"_ranges.csv")
write.csv(ranges_df,file=ranges_file)

# Prepare plots
depths <- as.numeric(rownames(core))
pdf(paste0(pattern,"_preview.pdf"))
Stratiplot(pred_matrix,depths,varTypes='absolute',ylab="Age/depth")
dc <- minDC(ana)
plot(dc,depths,xlab="Age/depth")
dev.off()
message("Prepared plots for preview.")
message("Done!")

# Restore the original print limit
options(max.print = old_limit)
