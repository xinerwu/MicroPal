# Check for duplicates in the n=1968 database

spec <- read.delim('dino1968.txt',row.names=1)
envi <- read.delim('envi1968.txt',row.names=1)
coor <- read.delim('coor1968.txt',row.names=1)

# 1. Identify rows that are duplicates of others (forward and backward)
is_duplicate <- duplicated(spec) | duplicated(spec, fromLast = TRUE)

# 2. Check how many there are
cat("Number of duplicate samples found:", sum(is_duplicate), "\n")

# Extract the duplicate rows
dup_spec <- spec[is_duplicate, ]
dup_env <- envi[is_duplicate, ]
dup_coor <- coor[is_duplicate, ]

# Combine them into a temporary dataframe for inspection
# We add the row names as a column so we don't lose them during sorting
inspection_table <- data.frame(
  SampleID = rownames(dup_spec),dup_coor, dup_env)
inspection_table <- inspection_table[do.call(order, as.list(dup_spec)), ]
inspection_table2 <- data.frame(
  SampleID = rownames(dup_spec),dup_coor, dup_spec)
inspection_table2 <- inspection_table2[do.call(order, as.list(dup_spec)), ]

mysheets <- list("envi"=inspection_table,"spec"=inspection_table2)
openxlsx::write.xlsx(mysheets,file="duplicates.xlsx")
print("Results written to duplicates.xlsx")
