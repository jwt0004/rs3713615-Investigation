# Read input genotype data collected from 1000G Project VCF for rs3173615
# Edit path to input file to run
snp_genotype_by_population <- read.delim("~/Downloads/snp_genotype_by_population.sorted.tsv")

# reshape the data for processing
data <- reshape(snp_genotype_by_population, 
                     timevar = "rs3173615.Genotype", 
                     idvar = "Population", 
                     direction = "wide")
					 
# Change column names for df for easier processing					 
colnames(data) <- c("Population", "0_0", "0_1", "1_1")

# Select African populations and add category to df					 
african_populations <- c("YRI", "LWK", "GWD", "MSL", "ESN", "ASW", "ACB")
data$Category <- ifelse(data$Population %in% african_populations, "African", "Non-African")

# Recalculate genotype frequencies based on new category
african_counts <- colSums(data[data$Population %in% african_populations, c("0_0", "0_1", "1_1")])
non_african_counts <- colSums(data[!data$Population %in% african_populations, c("0_0", "0_1", "1_1")])

# Create new African/Non-African df
combined_data <- rbind(african_counts, non_african_counts)
rownames(combined_data) <- c("African", "Non-African")

# Run chisq test on new df
chisq.test(combined_data)