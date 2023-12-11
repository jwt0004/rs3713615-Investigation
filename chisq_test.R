library(ggplot2)
library(reshape2)

# Read input genotype data collected from 1000G Project VCF for rs3173615
# Edit path to input file to run
snp_genotype_by_population <- read.delim("~/Downloads/snp_genotype_by_population.sorted.tsv")
snp_genotype_by_gender <- read.delim("~/Downloads/snp_genotype_by_gender.sorted.tsv")

# table for gender chisq test
gender_table <- xtabs(Freq ~ Gender + rs3173615.Genotype, data = snp_genotype_by_gender)

# chisq test for genotypes by geneder
chisq.test(gender_table)

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

# Transpose data
combined_data <- t(combined_data)
combined_data <- as.data.frame(combined_data)

# Calculate proportions
sum_african <- sum(combined_data$African)
sum_non_african <- sum(combined_data$`Non-African`)
combined_data$African <- combined_data$African / sum_african
combined_data$`Non-African` <- combined_data$`Non-African` / sum_non_african

# Add genotype column
combined_data$Genotype <- rownames(combined_data)

combined_data_long <- melt(combined_data, id.vars = 'Genotype', variable.name = 'Population', value.name = 'Proportion')

proportion_plot <- ggplot(combined_data_long, aes(x = Population, y = Proportion, fill = Genotype)) + 
	geom_bar(stat = 'identity', position = 'dodge') + 
	scale_fill_manual(values = c("0_0" = "#56B4E9", "0_1" = "#E69F00", "1_1" = "#009E73"), 
	labels = c("0/0", "0/1", "1/1")) + ylab('Proportion') + xlab('Population') + 
	ggtitle('Proportions of rs3173615 Genotypes in African and Non-African Populations') + 
	theme_minimal()
	
ggsave("~/Downloads/african_population_proportions.png", plot = proportion_plot, width = 6, height = 4, dpi = 300)
