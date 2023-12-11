library(ggplot2)

snp_genotype_by_gender <- read.delim("~/Downloads/snp_genotype_by_gender.sorted.tsv")
snp_genotype_by_pop <- read.delim("~/Downloads/snp_genotype_by_population.sorted.tsv")

gender_plot <- ggplot(snp_genotype_by_gender, aes(x = rs3173615.Genotype, y = Freq, fill = Gender)) + 
	geom_bar(stat = "identity", position = position_dodge()) + 
	scale_fill_manual(values = c("female" = "#56B4E9", "male" = "#E69F00")) + 
	labs(x = "Genotype", y = "Counts", fill = "Gender", title = "rs3173615 Genotype Counts by Gender") +
	theme_minimal()
  
population_plot <- ggplot(snp_genotype_by_pop, aes(x = Population, y = Freq, fill = rs3173615.Genotype)) + 
	geom_bar(stat = "identity", position = position_dodge()) + 
	labs(x = "Population", y = "Counts", fill = "Genotype", 
	title = "rs3173615 Genotype Counts by Population") + 
	scale_fill_manual(values = c("0/0" = "#56B4E9", "0/1" = "#E69F00", "1/1" = "#009E73")) + 
	theme_minimal() + 
	theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
ggsave("~/Downloads/genotype_counts_by_gender.png", plot = gender_plot, width = 6, height = 4, dpi = 300)
ggsave("~/Downloads/genotype_counts_by_population.png", plot = population_plot, width = 6, height = 4, dpi = 300)
