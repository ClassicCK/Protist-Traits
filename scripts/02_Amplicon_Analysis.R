# SPRUCE protist amplicon analysis for June and September 2019
# Author: Alyssa Carrell
# Date: 6/21/22

# Microbial sequences were processed with QIIME 2 v 2021.2 platform. 
# Paired sequences were demultiplexed with the plugin demuz and quality filtered 
# (denoised, dereplicated, chimera filtered and pair-end merged) and processed 
# in Sequence Variants (SVs) with the dada2 plugin. Taxonomy was assigned using 
# the Pr2 (18S) database. Sequence variant based alpha diversity (Shannon) and 
# beta diversity (bray curtis distance) were calculated with the phyloseq package.

# Load required libraries
library(qiime2R)
library(phyloseq)
library(ggplot2)
library(ggpubr)
library(scales)
library(tidyverse)
library(viridis)
library(car)
library(rcompanion)
library(FSA)
library(vegan)
library(RColorBrewer)
library(ggh4x)
library(microViz)
library(microbiome)
library(pals)
library(data.table)

# Data import
# Import QIIME2 .qza files and create phyloseq object for 18S
mapping <- read.delim("data/raw/mapping.txt")
SVs18S <- read_qza("data/raw/protist-dada2table.qza")
taxonomy18S <- read_qza("data/raw/protist-taxonomy.qza")

tax18S<-taxonomy18S$data %>% 
  as.tibble() %>% 
  mutate(Taxon=gsub("D_[0-9]__", "", Taxon)) %>% 
  separate(Taxon, sep=";", c("Kingdom","Phylum","Class","Order","Family","Genus","Species"))

physeq18S <- phyloseq(
  otu_table(SVs18S$data, taxa_are_rows = T),
  tax_table(as.data.frame(tax18S) %>% 
              select(-Confidence) %>% 
              column_to_rownames("Feature.ID") %>% 
              as.matrix()), # moving the taxonomy to the way phyloseq wants it
  sample_data(mapping %>% 
                as.data.frame() %>% 
                column_to_rownames("SampleID"))
) %>% 
  subset_taxa(Kingdom == "Eukaryota")

physeq18S <- subset_samples(physeq18S, sample_names(physeq18S) != "P16_june") %>%
  rarefy_even_depth()

# Filter protists and prepare data
# List of protist
Protist <- c("Alveolata", "Stramenopiles", "Archaeplastida")

# List of non-protist and things that are too small
Bad_Order <- c("Gregarinomorphea", "Bacillariophyta", "Embryophyceae")

# Filter to include protist and remove non-protists
physeq18S <- subset_taxa(physeq18S, Phylum %in% Protist) %>%
  subset_taxa(!(Order %in% Bad_Order))

mycolors <- as.vector(brewer.paired(14))

# Transform to relative abundance
phyla18S <- physeq18S %>%
  tax_glom(taxrank = "Genus") %>%
  transform_sample_counts(function(x){x/sum(x)}) %>%
  psmelt() %>%
  arrange(Genus)

new_phyla18S <- aggregate(Abundance ~ Sample, data = phyla18S, sum)
new_phyla18S$Abundance <- 1 - new_phyla18S$Abundance
new_phyla18S$OTU <- "Other"
new_phyla18S$Genus <- "Other"
new_phyla18S$Family <- "Other"
new_phyla18S$Order <- "Other"
new_phyla18S$Class <- "Other"
new_phyla18S$Phylum <- "Other"
new_phyla18S$Kingdom <- "Other"

new_phyla18S2 <- merge(new_phyla18S, mapping, by.x = c("Sample"), by.y = c("SampleID"))
phyla18S2 <- merge(phyla18S, new_phyla18S2, all = TRUE)
phyla18S <- phyla18S2 %>%
  add_column(gene = "18S")

s_phyla18S <- phyla18S[phyla18S$sampling == "sep" & phyla18S$OTU != "Other",]

# Plot
sp_phyla_18S <- ggplot(s_phyla18S, aes(x = Sample, y = Abundance, fill = Order)) +     
  geom_bar(stat = "identity", color = "black") + 
  theme_bw() +     
  theme(axis.text = element_text(size = 8),        
        axis.title = element_blank(),        
        axis.text.x = element_blank(),        
        axis.ticks = element_blank(),        
        legend.text = element_text(size = 8)) +     
  facet_nested(.~temp + co2, scales = "free") +  
  scale_fill_manual(values = (mycolors), name = "Protist Family") +     
  xlab("Samples") + 
  ylab("Relative Abundance Phylum >2%") +    
  theme(strip.background = element_rect(fill = "white"),           
        strip.text = element_text(color = "black", size = 8, face = "bold")) +     
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))

# Net changes in abundance
# Calculate initial and final relative abundances for each Order
initial_abundances <- phyla18S %>%   
  filter(temp == "0") %>%   
  group_by(Order, co2) %>%   
  summarise(InitialAbundance = sum(Abundance))

final_abundances <- phyla18S %>%   
  filter(temp == "9") %>%   
  group_by(Order, co2) %>%   
  summarise(FinalAbundance = sum(Abundance))

# Compute net change for each Order
net_changes <- merge(initial_abundances, final_abundances, by = c("Order", "co2"))
net_changes$NetChange <- ((net_changes$FinalAbundance - net_changes$InitialAbundance)/net_changes$InitialAbundance)*100

# Create a data frame for the functional traits
functional_traits <- data.frame(
  Order = c("Chlorophyceae", "Chrysophyceae", "Colpodea", "Heterotrichea",
            "Litostomatea", "Nassophorea", "Oligohymenophorea", "Oomycota",
            "Phyllopharyngea", "Spirotrichea", "Trebouxiophyceae",
            "Ulvophyceae", "Zygnemophyceae"),
  NutritionalMode = c("Phototroph", "Mixotroph", "Heterotroph", "Heterotroph",
                      "Heterotroph", "Heterotroph", "Heterotroph", "Heterotroph",
                      "Heterotroph", "Heterotroph", "Phototroph",
                      "Phototroph", "Phototroph"),
  Movement = c("Flagellate", "Flagellate", "Ciliate", "Ciliate",
               "Ciliate", "Ciliate", "Ciliate", "Sporic",
               "Ciliate", "Ciliate", "Flagellate or Non-Motile",
               "Flagellate or Non-Motile", "Non-Motile"),
  TrophicLevel = c("Producer", "Producer", "Predator", "Predator",
                   "Predator", "Predator", "Predator or Scavenger", "Decomposer or Parasite",
                   "Predator", "Predator", "Producer",
                   "Producer", "Producer")
)

# Merge the net changes with functional traits
results_table <- merge(net_changes, functional_traits, by = "Order")

df <- data.frame(
  Order = c("Chlorophyceae", "Chrysophyceae", "Colpodea", "Heterotrichea",
            "Litostomatea", "Nassophorea", "Oligohymenophorea", "Oomycota",
            "Other", "Phyllopharyngea", "Spirotrichea", "Trebouxiophyceae",
            "Ulvophyceae", "Zygnemophyceae"),
  AverageSize = c(16, 27.5, 12.5, 37.5,
                  20, 23, 12, 42.5,
                  52.5, 18, 30, 62.5,
                  24, 32) # Example average sizes
)

# Define size classes based on the schema provided
size_class_boundaries <- data.frame(
  SizeClass = 1:5,
  LowerBound = c(12, 17.07, 20.68, 28.24, 59.74),
  UpperBound = c(17.07, 20.68, 28.24, 59.74, 526.43)
)

# Function to determine the size class
get_size_class <- function(size) {
  for (i in 1:nrow(size_class_boundaries)) {
    if (size >= size_class_boundaries$LowerBound[i] && size < size_class_boundaries$UpperBound[i]) {
      return(size_class_boundaries$SizeClass[i])
    }
  }
  return(NA) # Return NA if no class matches
}

# Apply the function to create a new column 'SizeClass'
df$SizeClass <- sapply(df$AverageSize, get_size_class)

results_table <- results_table %>%
  mutate(co2 = ifelse(co2 == "eCO2", "Elevated", "Ambient"))

results_table <- results_table[results_table$Order != "Other",]

plot_data <- results_table %>%
  select(Order, co2, NetChange, NutritionalMode, Movement, TrophicLevel) %>%
  pivot_longer(cols = c(NutritionalMode, Movement, TrophicLevel),
               names_to = "TraitType", values_to = "TraitValue")

trait_plot <- ggplot(plot_data, aes(x = Order, y = NetChange, fill = TraitValue)) +
  theme_cowplot() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10)) +
  labs(color = expression(paste(CO[2], "Treatment"))) +
  theme(legend.position = "right") +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~co2) +
  labs(x = "Order", y = "Net Change in Abundance", fill = "Trait") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save plot (uncomment and set figures path as needed)
ggsave(filename = paste0("figures/manuscript/FigureSTraits.pdf"),
       plot = trait_plot, width = 180, height = 100, units = c("mm"), dpi = 600)

# Size Changes
size_table <- merge(net_changes, df, by = "Order")

# Print results table (uncomment if stargazer is available)
stargazer(results_table, summary = FALSE, type = "latex",
          title = "Net Relative Abundance Change in Functional Groups across Temperature Treatments",
          label = "tab:compositionLinkages",
          table.placement = "H",
          digits = 3,
          float = FALSE)

# Protist community analysis
# Shannon diversity analysis
mycolors <- as.vector(brewer.paired(12))
set.seed(092)

diversity18S <- estimate_richness(physeq18S, measures = c("Observed", "Shannon"))
diversity18S <- setDT(diversity18S, keep.rownames = "SampleID")[]
diversity18S$SampleID <- str_replace(diversity18S$SampleID, "\\.", "-")
diversity18S$SampleID <- str_replace(diversity18S$SampleID, "\\.", "-")
diversity18S$SampleID <- str_replace(diversity18S$SampleID, "\\.", "-")
diversity18S$SampleID <- str_replace(diversity18S$SampleID, "\\.", "-")

alpha18S <- diversity18S %>%
  left_join(mapping) %>%
  add_column(gene = "18S")

p_div_18S <- ggplot(alpha18S, aes(x = temp, y = Shannon, color = sampling, shape = co2, linetype = co2)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_blank(),
        legend.text = element_text(size = 12)) + 
  scale_fill_manual(values = (mycolors), name = "Duration") +     
  xlab("Warming Treatment") + 
  ylab("Shannon Diversity") +    
  theme(strip.background = element_rect(fill = "white"),           
        strip.text = element_text(color = "black", size = 12, face = "bold")) +     
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))

# Display plots
print(sp_phyla_18S)  # Taxa summary at the phylum level
print(p_div_18S)     # Shannon diversity relationship with temperature treatment and CO2
