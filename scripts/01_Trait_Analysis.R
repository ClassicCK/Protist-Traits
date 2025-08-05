# SPRUCE protist trait analysis
# Author: Christopher Kilner
# Date: 4/5/22

# Colors for Reproducibility
purple <- "#7851A9"
gold <- "#a97851"

# Read in data
data.raw <- read.csv("data/raw/SPRUCE_2019.csv", check.names = F)

drop <- c(4, 16, 25, 31, 38, 39, 40, 41, 44, 45)
data.processed <- data.raw[,-drop]
colnames(data.processed) <- c("Plot", "Sample", "Temperature", "Area.ABD", "Area.Filled",
                              "Aspect.Ratio", "Average.Blue", "Average.Green", "Average.Red", "Circularity", 
                              "Circularity.Hu", "Compactness", "Convex.Perimeter", "Convexity", 
                              "Diameter.ABD", "Diameter.ESD", "Diameter.FD", "Elongation", 
                              "Feret.Angle.Max", "Feret.Angle.Min", "Fiber.Curl", "Fiber.Straightness", 
                              "Geodesic.Aspect.Ratio", "Geodesic.Length", 
                              "Geodesic.Thickness", "Intensity",  "Length", "Perimeter", "Ratio.Blue.Green", 
                              "Ratio.Red.Blue", "Ratio.Red.Green", "Roughness", "Sigma.Intensity", 
                              "Sum.Intensity", "Symmetry", "Transparency", "Volume.ABD", "Volume.ESD","Width")
data.processed$Volume <-  (4/3)*pi*((data.processed$Geodesic.Thickness/2)^2)*(data.processed$Geodesic.Length/2)
data.processed <- data.processed[data.processed$Geodesic.Aspect.Ratio != 1,]

#Add in Carbon Treatment

data.processed$Carbon.Dioxide <- "Ambient"

elev <- which(data.processed$Plot == list(4))
data.processed$Carbon.Dioxide[elev] <- "Elevated"

elev <- which(data.processed$Plot == c(10))
data.processed$Carbon.Dioxide[elev] <- "Elevated"

elev <- which(data.processed$Plot == c(11))
data.processed$Carbon.Dioxide[elev] <- "Elevated"

elev <- which(data.processed$Plot == c(16))
data.processed$Carbon.Dioxide[elev] <- "Elevated"

elev <- which(data.processed$Plot == c(19))
data.processed$Carbon.Dioxide[elev] <- "Elevated"

data.processed$Sample <- as.factor(data.processed$Sample)
data.processed$Carbon.Dioxide <- as.factor(data.processed$Carbon.Dioxide)

write.csv(data.processed, "data/processed/processed.csv")

### Statistical Evaluation of Data

# Following the initial pre-processing of the data, I ran a skewness test
# on the various traits, to ensure normal distribution and -- if
# non-normal -- determine the proper transformation of the data.
skewtable <- matrix(, nrow = 37, ncol = 2)
data.subset <- data.processed[,4:40]

for(i in 1:37){
  name <- colnames(data.subset[i])
  skew <- skewness(data.subset[,i])
  skewtable[i,1] <- name
  skewtable[i,2] <- skew
}

print(skewtable)

# Following this skewness test, I determined the optimal transformation
# through the information provided by the skewness test
  
#### Transformation Determinations
  
#  | Trait                 | Distribution | Zero | Transformation   |
#  |-----------------------|--------------|------|------------------|
#  | Width                 | right-tailed | No   | log              |
#  | Volume.ESD            | right-tailed | No   | log              |
#  | Volume.ABD            | right-tailed | No   | log              |
#  | Transparency          | right-tailed | No   | log              |
#  | Symmetry              | left-tailed  | Yes  | log              |
#  | Sum.Intensity         | right-tailed | No   | log              |
#  | Sigma Intensity       | normal       | No   |                  |
#  | Roughness             | right-tailed | No   | log              |
#  | Ratio.Red.Green       | normal       | No   |                  |
#  | Ratio.Red.Blue        | normal       | No   |                  |
#  | Ratio.Blue.Green      | normal       | No   |                  |
#  | Perimeter             | right-tailed | No   | log              |
#  | Length                | right-tailed | No   | log              |
#  | Intensity             | normal       | No   |                  |
#  | Geodesic.Thickness    | right-tailed | No   | log              |
#  | Geodesic.length       | right-tailed | No   | log              |
#  | Geodesic.Aspect.Ratio | ???          | No   |                  |
#  | Fiber.Straightness    | left-tailed  | No   | log              |
#  | Fiber.Curl            | right-tailed | No   | log              |
#  | Feret.Angle.Min       | circular     | Yes  | cotangent        |
#  | Feret.Angle.Max       | circular     | Yes  | cotangent        |
#  | Elongation            | right-tailed | No   | log              |
#  | Diameter.FD           | right-tailed | No   | log              |
#  | Diameter.ESD          | right-tailed | No   | log              |
#  | Diameter.ABD          | right-tailed | No   | log              |
#  | Convexity             | left-tailed  | No   | log              |
#  | Convex.Perimeter      | right-tailed | No   | log              |
#  | Compactness           | right-tailed | No   | log              |
#  | Circularity.Hu        | left-tailed  | No   | log              |
#  | Circularity           | left-tailed  | No   | log              |
#  | Average.Red           | normal       | No   |                  |
#  | Average.Green         | normal       | No   |                  |
#  | Average.Blue          | normal       | No   |                  |
#  | Aspect.Ratio          | bi-modal     | No   | abs(x - mean(x)) |
#  | Area.Filled           | right-tailed | No   | log              |
#  | Area.ABDS             | right-tailed | No   | log              |
#  | Volume                | right-tailed | No   | log              |
  
# The data was then transformed accordingly, and any outliers removed

data.transformed <- data.processed

data.transformed$Area.ABD <- log(data.processed$Area.ABD)
data.transformed$Area.Filled <- log(data.processed$Area.Filled)
data.transformed$Circularity <- log(data.processed$Circularity)
data.transformed$Circularity.Hu <- log(data.processed$Circularity.Hu)
data.transformed$Compactness <- log(data.processed$Compactness)
data.transformed$Convex.Perimeter <- log(data.processed$Convex.Perimeter)
data.transformed$Convexity <- log(data.processed$Convexity)
data.transformed$Diameter.ABD <- log(data.processed$Diameter.ABD)
data.transformed$Diameter.ESD <- log(data.processed$Diameter.ESD )
data.transformed$Diameter.FD <- log(data.processed$Diameter.FD)
data.transformed$Elongation <- log(data.processed$Elongation)
data.transformed$Feret.Angle.Max <- atan(data.processed$Feret.Angle.Max)
data.transformed$Feret.Angle.Min <- atan(data.processed$Feret.Angle.Min)
data.transformed$Fiber.Curl <- log(data.processed$Fiber.Curl + 0.0000001)
data.transformed$Fiber.Straightness <- log(data.processed$Fiber.Straightness)
data.transformed$Geodesic.Length <- log(data.processed$Geodesic.Length)
data.transformed$Geodesic.Thickness <- log(data.processed$Geodesic.Thickness)
data.transformed$Intensity <- log(data.processed$Intensity)
data.transformed$Length <- log(data.processed$Length)
data.transformed$Perimeter <- log(data.processed$Perimeter)
data.transformed$Roughness <- log(data.processed$Roughness)
data.transformed$Sum.Intensity <- log(data.processed$Sum.Intensity)
data.transformed$Symmetry <- log(data.processed$Symmetry)
data.transformed$Transparency <- log(data.processed$Transparency)
data.transformed$Volume.ABD <- log(data.processed$Volume.ABD )
data.transformed$Volume.ESD <- log(data.processed$Volume.ESD)
data.transformed$Width <- log(data.processed$Width)
data.transformed$Volume <- log(data.processed$Volume)

data.transformed[is.na(data.transformed)] <- 0

#### Evaluation of Correlation and Co-linearity of Data
# Following prior published analysis, we focused on a subset of traits

keep <- c("Plot", "Sample", "Temperature", 
          "Geodesic.Aspect.Ratio", "Geodesic.Length",
          "Ratio.Red.Green", "Sigma.Intensity", "Volume", "Carbon.Dioxide")

data.selected <- data.transformed[,keep]

drop.index.small <- c(1, 2, 3, 9)
drop.index.large <- c(1, 2, 3, 41)

p <- ggcorrmat(
  data     = data.selected[,-drop.index.small],
  cor.vars = c(Volume, Geodesic.Aspect.Ratio, Geodesic.Length, Ratio.Red.Green, Sigma.Intensity, Geodesic.Length),
  cor.vars.names = c(
    "Volume",
    "Aspect Ratio",
    "Red/Green Ratio",
    "Sigma Intensity",
    "Length"
  ),
  colors   = c("#002244","white","#FB4F14"),
  type         = "parametric", ## correlation method,
  ggcorrplot.args = list(
    tl.srt = 45,
    pch.col = "white",
    outline.color = "white", hc.order = TRUE,
    pch.cex = 4
  )
) + 
  theme(legend.position="right")

q <- ggcorrmat(
  data     = data.transformed[,-drop.index.large],
  cor.vars = c(Area.ABD:Volume),
  cor.vars.names = c("Area (ABD)",
                     "Area (Filled)",
                     "Aspect Ratio",
                     "Average Blue",
                     "Avergae Green",
                     "Average Red",
                     "Circularity",
                     "Circularity Hu",
                     "Compactness",
                     "Convex Perimeter",
                     "Convexity",
                     "Diameter (ABD)",
                     "Diameter (ESD)",
                     "Diameter (FD)",
                     "Elongation",
                     "Feret Angle Max.",
                     "Feret Angle Min.",
                     "Fiber Curl",
                     "Fiber Straightness",
                     "Geodesic Aspect Ratio",
                     "Geodesic Length",
                     "Geodesic Thickness",
                     "Intensity",
                     "Length",
                     "Perimeter",
                     "Blue/Green Ratio",
                     "Red/Blue Ratio",
                     "Red/Green Ratio",
                     "Roughness",
                     "Sigma Intensity",
                     "Sum Intensity",
                     "Symmetry",
                     "Transparency",
                     "Volume (ABD)",
                     "Volume (ESD)",
                     "Width",
                     "Volume"),
  colors   = c("#002244","white","#FB4F14"),
  type         = "pearson", ## correlation method,
  ggcorrplot.args = list(
    tl.srt = 45,
    pch.col = "white",
    outline.color = "white", hc.order = TRUE,
    pch.cex = 0,
    lab_size = 0
  )
) + theme(legend.position="none",
          axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5))

legend <- get_legend(
  p + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "right"))

FigureSTEMP <- plot_grid(q, p, labels = c("A", "B"), ncol = 1, rel_heights = c(0.7, 0.3))
FigureS1 <- plot_grid(FigureSTEMP, legend, nrow = 1, rel_widths = c(1, .05), align = "h", axis = "tb")

ggsave(filename= paste0("figures/manuscript/FigureS1.pdf"), 
       plot = FigureSTEMP, width = 180, height = 300, units=c("mm"), dpi=600) 

# Conversion of abundance by density
density <- read.csv("data/raw/SPRUCE_Density_2019.csv", check.names = F)

density$Particles...ml <- density$`Particles / ml`*6

set.seed(118)
density.lm <- lm(Count ~ 0 + Particles...ml, data = density)
print(summary(density.lm))

#### Printed Data Summary
# Call: lm(formula = Count \~ 0 + Particles...ml, data = density)
#
# Residuals: Min 1Q Median 3Q Max -14101.9 -1162.6 -492.4 -60.7 8383.6
#
# Coefficients: Estimate Std. Error t value Pr(\>\|t\|)\
# Particles...ml 0.58615 0.02654 22.08 5.22e-15 ***--- Signif. codes: 0
# '***' 0.001 '\*\*' 0.01 '\*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 4315 on 19 degrees of freedom Multiple
# R-squared: 0.9625, Adjusted R-squared: 0.9605 F-statistic: 487.7 on 1
# and 19 DF, p-value: 5.218e-15

#### Adjust count data on regression
data.selected$adj.count <- 1/density.lm$coefficients[1]
data.selected$adj.mass <- (data.selected$Volume*1e-12)/density.lm$coefficients[1]

data.upper <- data.selected
data.lower <- data.selected

density.lm$coefficients[1]/0.02654

data.upper$adj.count <- 1/(density.lm$coefficients[1] + 0.02654)
data.lower$adj.count <- 1/(density.lm$coefficients[1] + 0.02654)

#### Plot adjustments
adjusted <- density
adjusted$Transformed <- adjusted$Count/density.lm$coefficients[1]

colnames(adjusted)[6] <- "Density"

adjusted.adj <- adjusted[,c("Density", "Transformed")]
adjusted.ori <- adjusted[,c("Density", "Count")]

adjusted.adj$Transformation <- "Estimated"
adjusted.ori$Transformation <- "Original"

colnames(adjusted.adj)[2] <- "Count"

density.plot <- rbind(adjusted.adj, adjusted.ori)
density.plot$Transformation <- as.factor(density.plot$Transformation)

gad <- ggplot(data = density.plot, aes(y = Count, x = Density, color = Transformation)) +
  # Add Segment and Points
  scale_colour_manual(values = c("#005BBB", "#FFD500")) +
  stat_smooth(method = "glm", formula = y ~ 0 + x) +
  geom_point(size = 4, show.legend = FALSE, alpha = 0.5) +
  geom_abline(linetype = "dashed", colour = "Gray") +
  theme_cowplot(12)  +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  xlab("Density (particles/ml)") +
  ylab("Count") +
  ggtitle("Derivation of Counts") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= "figures/manuscript/FigureS2.pdf", 
       plot = gad, width = 180, height = 180, units=c("mm"), dpi=600) 

#### Cluster data by protist size
BIC <- mclustBIC(data.selected[,c("Geodesic.Length")])
saveRDS(BIC, file = "models/BIC.RDS")

# Bayesian Information Criterion Results
# BIC <- readRDS(file = "models/BIC.RDS") # Uncomment to reload data if needed
plot(BIC)
summary(BIC)

# Apply optimum clustering
clust.model <- Mclust(data.selected[,c("Geodesic.Length")], modelName = "V", G = 5)
summary(clust.model)
saveRDS(clust.model, file = "models/cluster.RDS")
# clust.model <- readRDS(file = "models/cluster.RDS") # Uncomment to reload data if needed

# Optimized clustered data
data.clustered <- cbind(data.selected, clust.model$classification)
colnames(data.clustered)[12] <- "Size.Class"

# Generate Figure S3
data.clustered$Size.Class <- factor(data.clustered$Size.Class, levels = c("5", "4", "3", "2", "1"))
names <- as_labeller(c('1' = "Size Class 1", '2' = "Size Class 2", '3' = "Size Class 3", '4' = "Size Class 4", '5' = "Size Class 5"))

bayes <- as.data.frame(BIC[,1:2])
bayes$x <- as.numeric(row.names(bayes))
long <- reshape2::melt(bayes, id.vars = c("x"))

colnames(long) <- c("Clusters", "Method", "BIC")

bayesplot <- ggplot(long, aes(x=Clusters, y=BIC, color = Method)) +
  geom_line() + 
  geom_point() +
  theme_cowplot(12) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  labs(fill= "Method") +
  ylab("BIC") +
  xlab("Clusters") + 
  scale_alpha(guide = 'none') +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                     labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))

clusts <- ggplot(data.clustered, aes(x=Geodesic.Length, fill=Size.Class, color = Size.Class)) + 
  geom_density(aes(y=after_stat(count), alpha = 0.95)) + 
  theme_cowplot(12) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  labs(fill= "Size Class") +
  scale_fill_viridis(discrete = T, option = "D") +
  scale_color_viridis(discrete = T, option = "D", guide = 'none') +
  ylab("Count") +
  xlab("ln Geodesic Length") + 
  scale_alpha(guide = 'none')

Figure_Tmp <- plot_grid(bayesplot, clusts, labels = c("A", "B"), label_size = 14, nrow=2)
ggsave(filename= "figures/manuscript/FigureS3.pdf", 
       plot = Figure_Tmp, width = 180, height = 120, units=c("mm"), dpi=600) 

#### Plots of elevated vs. non-elevated CO2 treatments by linear model
tmp <- aggregate(.~ Sample + Size.Class + Temperature + Carbon.Dioxide, data.clustered, mean)
tmp$Temperature <- as.character(tmp$Temperature)
tmp$Temperature <- as.numeric(tmp$Temperature)
tmp$Size.Class <- as.factor(tmp$Size.Class)
tmp$mean.mass <- tmp$adj.mass

tmp.reduced <- tmp[,c("Carbon.Dioxide", "Size.Class", "Temperature", "Geodesic.Aspect.Ratio", 
                      "Sigma.Intensity","Ratio.Red.Green", "Volume", "mean.mass")]


for(i in 4:8){
  name <- colnames(tmp.reduced[i])
  g <-  ggplot(data = tmp.reduced, aes(x=Temperature, y=tmp.reduced[,name], linetype=Carbon.Dioxide, color=Size.Class)) +
    stat_smooth(method="glm", se = F) +
    ggtitle("Mean Trait by Temperature") +
    theme_cowplot() +
    ylab(name) + 
    xlab("Temperature")
  
  ggsave(filename= paste0("figures/LM/LM.", name, ".png"), 
         plot = g, width = 25, height=25, units=c("cm"), dpi=300) 
}

tmp <- aggregate(.~ Sample + Size.Class + Temperature + Carbon.Dioxide, data.clustered, sum)
tmp$Temperature <- as.character(tmp$Temperature)
tmp$Temperature <- as.numeric(tmp$Temperature)
tmp$Size.Class <- as.factor(tmp$Size.Class)

tmp.reduced <- tmp[,c("Carbon.Dioxide", "Size.Class", "Temperature", "adj.count", 
                      "adj.mass")]

for(i in 4:5){
  name <- colnames(tmp.reduced[i])
  g <-  ggplot(data = tmp.reduced, aes(x=Temperature, y=tmp.reduced[,name], linetype=Carbon.Dioxide, color=Size.Class)) +
    stat_smooth(method="glm", se = F) +
    ggtitle("Mean Trait by Temperature") +
    theme_cowplot() +
    ylab(name) + 
    xlab("Temperature")
  
  ggsave(filename= paste0("figures/LM/LM.", name, ".png"), 
         plot = g, width = 25, height=25, units=c("cm"), dpi=300) 
}

# GLMs for Trait Data
data.summed <- aggregate(.~ Plot + Sample + Size.Class + Temperature + Carbon.Dioxide, data.clustered, sum)
data.clustered$Size.Class <- as.factor(data.clustered$Size.Class)
data.summed$Size.Class <- as.factor(data.summed$Size.Class)

GLM_Sigma.Intensity <- glm(Sigma.Intensity ~ (Temperature*Carbon.Dioxide*Size.Class), data = data.clustered)
GLM_Red.Green.Ratio <- glm(Ratio.Red.Green ~ (Temperature*Carbon.Dioxide*Size.Class), data = data.clustered)
GLM_Aspect.Ratio <- glm(Geodesic.Aspect.Ratio ~ (Temperature*Carbon.Dioxide*Size.Class), data = data.clustered)
GLM_Volume <- glm(Volume ~ (Temperature*Carbon.Dioxide*Size.Class), data = data.clustered)

#Calculate F-statistic and P-value
GLM_Sigma.Intensity.0 <- glm(Sigma.Intensity ~ 1, data = data.clustered)
Sigma.Intensity.Anova <- anova(GLM_Sigma.Intensity, GLM_Sigma.Intensity.0, test = "F")

GLM_Volume.0 <- glm(Volume ~ 1, data = data.clustered)
Volume.Anova <- anova(GLM_Volume, GLM_Volume.0, test = "F")

GLM_Aspect.Ratio.0 <- glm(Geodesic.Aspect.Ratio ~ 1, data = data.clustered)
Aspect.Ratio.Anova <- anova(GLM_Aspect.Ratio, GLM_Aspect.Ratio.0, test = "F")

GLM_Red.Green.0 <- glm(Ratio.Red.Green ~ 1, data = data.clustered)
Red.Green.Anova <- anova(GLM_Red.Green.Ratio, GLM_Red.Green.0, test = "F")

colnames(data.summed)[11] <- "Estimated Abundance"
colnames(data.summed)[12] <- "Community Biomass"

# Biomass models
biomass <- glm(`Community Biomass` ~ Temperature*Carbon.Dioxide*Size.Class, data = data.summed)
abundance <- glm(`Estimated Abundance` ~ Temperature*Carbon.Dioxide*Size.Class, data = data.summed)

biomass.0 <- glm(`Community Biomass` ~ 1, data = data.summed)
abundance.0 <- glm(`Estimated Abundance` ~ 1, data = data.summed)

biomass.anova <- anova(biomass, biomass.0, test = "F")
abundance.anova <- anova(abundance, abundance.0, test = "F")

# Bootstrapping GLMs
GLM_Sigma.Intensity <- glm(Sigma.Intensity ~ (Temperature*Carbon.Dioxide*Size.Class), data = data.clustered)
GLM_Red.Green.Ratio <- glm(Ratio.Red.Green ~ (Temperature*Carbon.Dioxide*Size.Class), data = data.clustered)
GLM_Aspect.Ratio <- glm(Geodesic.Aspect.Ratio ~ (Temperature*Carbon.Dioxide*Size.Class), data = data.clustered)
GLM_Volume <- glm(Volume ~ (Temperature*Carbon.Dioxide*Size.Class), data = data.clustered)

# Bootstrap models

# Perform bootstrapping
set.seed(118) # for reproducibility
boot.sigma.intensity <- Boot(GLM_Sigma.Intensity, R = 1000) # R is the number of bootstrap replications
boot.red.green.ratio <- Boot(GLM_Red.Green.Ratio, R = 1000) # R is the number of bootstrap replications
boot.aspect.ratio <- Boot(GLM_Aspect.Ratio, R = 1000) # R is the number of bootstrap replications
boot.volume <- Boot(GLM_Volume, R = 1000) # R is the number of bootstrap replications

# Plotting
hist.volume <- hist(boot.volume)

# Extract the bootstrapped coefficients from the boot object
boot_coefs <- as.data.frame(t(boot.volume$t))

# Apply the names to your bootstrap coefficients
names(boot_coefs) <- coef_names

# Tidy the bootstrapped coefficients for plotting
boot_coefs_tidy <- boot_coefs %>%
  pivot_longer(cols = everything(), names_to = "term", values_to = "estimate")

# Function to plot individual terms
plot_term <- function(term_name, boot_coefs_tidy, observed_coefs) {
  term_data <- boot_coefs_tidy
  conf_int <- quantile(term_data$estimate, probs = c(0.025, 0.975))
  
  ggplot(term_data, aes(x = estimate)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, colour = "black", fill = "white") +
    geom_density(alpha = .2, fill = "#FF6666") +
    geom_vline(xintercept = observed_coefs[term_name], colour = "blue", linetype = "dashed", size = 1) +
    geom_vline(xintercept = conf_int[1], colour = "black", linetype = "dotted", linewidth = 1) +
    geom_vline(xintercept = conf_int[2], colour = "black", linetype = "dotted", linewidth = 1) +
    stat_function(fun = dnorm, args = list(mean = mean(term_data$estimate), sd = sd(term_data$estimate)), colour = "purple", linewidth = 1) +
    labs(title = term_name, x = "Coefficient Value", y = "Density") +
    theme_minimal()
}

# Observed coefficients from the GLM model
observed_coefs <- coef(GLM_Volume)

# Create a list of plots for each term
plots_list <- lapply(names(observed_coefs), function(term_name) plot_term(term_name, boot_coefs_tidy, observed_coefs))

# Check the first plot
print(plots_list[[1]])

# Run GLM models for individual trait data
for(i in 1:5){
  tmp <- data.clustered[data.clustered$Size.Class == i,]
  
  # Run GLMs for Individual Traits Data
  m <- glm(Geodesic.Aspect.Ratio ~ (Temperature*Carbon.Dioxide), data = tmp)
  saveRDS(m, file = paste0("results/GLM_Aspect_Ratio_", i, ".RDS"))
  
  m <- glm(Sigma.Intensity ~ (Temperature*Carbon.Dioxide), data = tmp)
  saveRDS(m, file = paste0("results/GLM_Sigma_Intensity_", i, ".RDS"))
  
  m <- glm(Volume ~ (Temperature*Carbon.Dioxide), data = tmp)
  saveRDS(m, file = paste0("results/GLM_Volume_", i, ".RDS"))
  
  m <- glm(adj.mass ~ (Temperature*Carbon.Dioxide), data = tmp)
  saveRDS(m, file = paste0("results/GLM_Mean_Mass_", i, ".RDS"))
  
  m <- glm(Ratio.Red.Green ~ (Temperature*Carbon.Dioxide), data = tmp)
  saveRDS(m, file = paste0("results/GLM_RedGreenRatio_", i, ".RDS"))
  
  # Run GAMs for Community Traits Data
  tmp.summed <- data.summed[data.summed$Size.Class == i,]
  colnames(tmp.summed)[11] <- "adj.count"
  colnames(tmp.summed)[12] <- "adj.mass"
  
  m <- gam(adj.count ~ (Temperature*Carbon.Dioxide), data = tmp.summed)
  saveRDS(m, file = paste0("results/GAM_Abundance_", i, ".RDS"))
  
  m <- gam(adj.mass ~ (Temperature*Carbon.Dioxide), data = tmp.summed)
  saveRDS(m, file = paste0("results/GAM_Biomass_", i, ".RDS"))
}

# Generate Figure 2
data.summed$Size.Class <- factor(data.summed$Size.Class, levels = c("5", "4", "3", "2", "1"))

p <-ggplot(data.summed, aes(fill=Size.Class, y=`Estimated Abundance`, x=Temperature)) + 
  geom_density(position="fill", stat="identity" , size=.1, colour="white") +
  scale_fill_viridis(discrete = T, option = "D") +
  facet_wrap("Carbon.Dioxide") + 
  theme_cowplot(12) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 12), 
        legend.position="none", 
        strip.background=element_rect(colour="black",
                                      fill="white")) +
  labs(fill= "Size Class") +
  ylab("Estimated Relative Abundance") +
  scale_x_continuous(breaks = c(0, 2.25, 4.5, 6.75, 9),
                     labels = c("0", "+2.25", "+4.5", "+6.75", "+9")) +
  xlab("Temperature (\u00B0C)")

#ggsave(filename= paste0(figures, "/Manuscript/Figure2B.png"), 
#       plot = p, width = 180, height=9, units=c("mm"), dpi=1200) 

q <- ggplot(data.summed, aes(fill=Size.Class, y=`Estimated Abundance`, x=Temperature)) + 
  geom_area(size=.1, colour="white") +
  scale_fill_viridis(discrete = T, option = "D") +
  facet_wrap("Carbon.Dioxide") + 
  theme_cowplot(12) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10), 
        legend.position="none", 
        strip.background=element_rect(colour="black",
                                      fill="white")) +
  labs(fill= "Size Class") +
  ylab("ln Estimated Abundance") +
  scale_x_continuous(breaks = c(0, 2.25, 4.5, 6.75, 9),
                     labels = c("0", "+2.25", "+4.5", "+6.75", "+9"))
xlab("Temperature (\u00B0C)")

legend <- get_legend(
  p + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom"))

Figure_Tmp <- plot_grid(q, p, labels = c("A", "B"), label_size = 14)
Figure2 <- plot_grid(Figure_Tmp, legend, nrow = 2, rel_heights = c(1, .05), align = "v")

ggsave(filename= "figures/manuscript/Figure2.pdf", 
       plot = Figure2, width = 180, height=80, units=c("mm"), dpi=600) 

## Overall Calculated Slopes by Trait
#Standardize Traits before Regression
#Calculate mean for each predictor and subtract from each value (distributed around zero)
tmp <- data.clustered[,c("Size.Class", "Carbon.Dioxide", "Temperature", "Geodesic.Aspect.Ratio", 
                         "Sigma.Intensity", "Ratio.Red.Green", "Volume")]

tmp$Temperature <- as.character(tmp$Temperature)
tmp$Temperature <- as.numeric(tmp$Temperature)
tmp$Size.Class <- as.factor(tmp$Size.Class)
colnames(tmp)[4] <- "Aspect Ratio"
colnames(tmp)[5] <- "Sigma Intensity"
colnames(tmp)[6] <- "Red to Green Ratio"
colnames(tmp)[7] <- "Volume"
names <- as_labeller(c('1' = "Size Class 1", '2' = "Size Class 2", '3' = "Size Class 3", '4' = "Size Class 4", '5' = "Size Class 5"))

slope.matrix <- matrix(, nrow = 40, ncol = 4)
colnames(slope.matrix) <- c("Variable", "Size.Class", "CO2 Treatment", "Slope")

slope.matrix[1:10, 1] <- "Aspect Ratio"
slope.matrix[11:20, 1] <- "Sigma Intensity"
slope.matrix[21:30, 1] <- "Red to Green Ratio"
slope.matrix[31:40, 1] <- "Volume"

slope.matrix[1:5, 3] <- "Ambient"
slope.matrix[6:10, 3] <- "Elevated"
slope.matrix[11:15, 3] <- "Ambient"
slope.matrix[16:20, 3] <- "Elevated"
slope.matrix[21:25, 3] <- "Ambient"
slope.matrix[26:30, 3] <- "Elevated"
slope.matrix[31:35, 3] <- "Ambient"
slope.matrix[36:40, 3] <- "Elevated"

slope.matrix[,2] <- rep_len(1:5, length.out=40)

for(i in 4:7){
  if(i == 4){
    for(p in 1:5){
      my.slope <- glm(`Aspect Ratio` ~ Temperature, data = tmp[tmp$Size.Class == p & tmp$Carbon.Dioxide == "Ambient",])
      slope <- my.slope$coefficients[2]
      slope.matrix[p,4] <- slope
    }
    for(q in 1:5){
      my.slope <- glm(`Aspect Ratio` ~ Temperature, data = tmp[tmp$Size.Class == q & tmp$Carbon.Dioxide == "Elevated",])
      slope <- my.slope$coefficients[2]
      slope.matrix[q+5,4] <- slope
    }
  }
  if(i == 5){
    for(p in 1:5){
      my.slope <- glm(`Sigma Intensity` ~ Temperature, data = tmp[tmp$Size.Class == p & tmp$Carbon.Dioxide == "Ambient",])
      slope <- my.slope$coefficients[2]
      slope.matrix[p+10,4] <- slope
    }
    for(q in 1:5){
      my.slope <- glm(`Sigma Intensity` ~ Temperature, data = tmp[tmp$Size.Class == q & tmp$Carbon.Dioxide == "Elevated",])
      slope <- my.slope$coefficients[2]
      slope.matrix[q+15,4] <- slope
    }
  }
  if(i == 6){
    for(p in 1:5){
      my.slope <- glm(`Red to Green Ratio` ~ Temperature, data = tmp[tmp$Size.Class == p & tmp$Carbon.Dioxide == "Ambient",])
      slope <- my.slope$coefficients[2]
      slope.matrix[p+20,4] <- slope
    }
    for(q in 1:5){
      my.slope <- glm(`Red to Green Ratio` ~ Temperature, data = tmp[tmp$Size.Class == q & tmp$Carbon.Dioxide == "Elevated",])
      slope <- my.slope$coefficients[2]
      slope.matrix[q+25,4] <- slope
    }
  }
  if(i == 7){
    for(p in 1:5){
      my.slope <- glm(Volume ~ Temperature, data = tmp[tmp$Size.Class == p & tmp$Carbon.Dioxide == "Ambient",])
      slope <- my.slope$coefficients[2]
      slope.matrix[p+30,4] <- slope
    }
    for(q in 1:5){
      my.slope <- glm(Volume ~ Temperature, data = tmp[tmp$Size.Class == q & tmp$Carbon.Dioxide == "Elevated",])
      slope <- my.slope$coefficients[2]
      slope.matrix[q+35,4] <- slope
    }
  }
}

overall.slope.matrix <- slope.matrix
overall.slope.matrix <- overall.slope.matrix[,-4]
overall.slope.matrix <- as.data.frame(overall.slope.matrix)
overall.slope.matrix$Mean.Slope <- NA
overall.slope.matrix$SE <- NA

Amb <- glm(`Aspect Ratio` ~ Temperature, data = tmp[tmp$Carbon.Dioxide == "Ambient",])
Elv <- glm(`Aspect Ratio` ~ Temperature, data = tmp[tmp$Carbon.Dioxide == "Elevated",])

overall.slope.matrix[1:5,4] <- mean(as.numeric(slope.matrix[1:5,4]))
overall.slope.matrix[1:5,5] <- coef(summary(Amb))[, "Std. Error"][2] 
overall.slope.matrix[6:10,4] <- coef(summary(Elv))[, "Estimate"][2]
overall.slope.matrix[6:10,5] <- coef(summary(Elv))[, "Std. Error"][2] 

Amb <- glm(`Sigma Intensity` ~ Temperature, data = tmp[tmp$Carbon.Dioxide == "Ambient",])
Elv <- glm(`Sigma Intensity` ~ Temperature, data = tmp[tmp$Carbon.Dioxide == "Elevated",])

overall.slope.matrix[11:15,4] <- coef(summary(Amb))[, "Estimate"][2]
overall.slope.matrix[11:15,5] <- coef(summary(Amb))[, "Std. Error"][2] 
overall.slope.matrix[16:20,4] <- coef(summary(Elv))[, "Estimate"][2]
overall.slope.matrix[16:20,5] <- coef(summary(Elv))[, "Std. Error"][2]  

Amb <- glm(`Red to Green Ratio` ~ Temperature, data = tmp[tmp$Carbon.Dioxide == "Ambient",])
Elv <- glm(`Red to Green Ratio` ~ Temperature, data = tmp[tmp$Carbon.Dioxide == "Elevated",])

overall.slope.matrix[21:25,4] <- coef(summary(Amb))[, "Estimate"][2]
overall.slope.matrix[21:25,5] <- coef(summary(Amb))[, "Std. Error"][2] 
overall.slope.matrix[26:30,4] <- coef(summary(Elv))[, "Estimate"][2]
overall.slope.matrix[26:30,5] <- coef(summary(Elv))[, "Std. Error"][2] 

Amb <- glm(Volume ~ Temperature, data = tmp[tmp$Carbon.Dioxide == "Ambient",])
Elv <- glm(Volume ~ Temperature, data = tmp[tmp$Carbon.Dioxide == "Elevated",])

overall.slope.matrix[31:35,4] <- coef(summary(Amb))[, "Estimate"][2]
overall.slope.matrix[31:35,5] <- coef(summary(Amb))[, "Std. Error"][2] 
overall.slope.matrix[36:40,4] <- coef(summary(Elv))[, "Estimate"][2]
overall.slope.matrix[36:40,5] <- coef(summary(Elv))[, "Std. Error"][2] 

combo <- overall.slope.matrix[,4:5]

## Calculated Slopes by Size.Class

slopes <- as.data.frame(slope.matrix)
ambient <- slopes[slopes$`CO2 Treatment` == "Ambient",]
elevated <- slopes[slopes$`CO2 Treatment` == "Elevated",]

slopes$Slope <- as.numeric(slopes$Slope)

segment_info <- slopes
segment_info <- segment_info[segment_info$`CO2 Treatment` == "Ambient",]
segment_info <- segment_info[,1:2]
segment_info$start <- NA
segment_info$end <- NA
segment_info$difference <- NA

segment_info$start <- as.numeric(ambient$Slope)
segment_info$end <- as.numeric(elevated$Slope)

segment_info$difference <- abs(segment_info$start - segment_info$end)

diff <- segment_info[segment_info$Variable == "Aspect Ratio",]
diff$x_pos = diff$start + (diff$difference/2)

diff2 <- segment_info[segment_info$Variable != "Aspect Ratio",]
diff2$x_pos = diff2$start - (diff2$difference/2)

diff <- rbind(diff, diff2)

slopes <- cbind(slopes, combo)

# Figure S4
group_temperatures <- function(temp) {
  if (temp >= 0 && temp < 2.25) {
    return("0 - 2.25")
  } else if (temp >= 2.25 && temp < 4.5) {
    return("2.25 - 4.5")
  } else if (temp >= 4.5 && temp < 6.75) {
    return("4.5 - 6.75")
  } else if (temp >= 6.75 && temp < 9) {
    return("6.75 - 9")
  } else {
    return("9+")
  }
}

tmp$TemperatureGroup <- sapply(tmp$Temperature, group_temperatures)
data.summed$TemperatureGroup <- sapply(data.summed$Temperature, group_temperatures)

vol <-  ggplot(data = tmp, aes(x=Temperature, y=tmp[,7], color=Carbon.Dioxide)) +
  geom_boxplot(aes(group = interaction(TemperatureGroup, Carbon.Dioxide, width = 0.02)), position = position_dodge(width = 2), outlier.shape = NA) +
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  scale_fill_manual(values = c("#7851A9", "#a97851")) +
  stat_smooth(method="glm", se = T) +
  theme_cowplot(12) +
  ylab("Volume") + 
  xlab("Temperature") + 
  stat_cor(method = "pearson", size = 2) + 
  facet_wrap("Size.Class", nrow = 1, labeller = names) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_blank()) +
  labs(color= expression(paste(C0[2], "Treatment"))) +
  theme(legend.position="none")+
  scale_y_continuous(labels = label_number(accuracy = 0.01))

ar <-  ggplot(data = tmp, aes(x=Temperature, y=tmp[,4], color=Carbon.Dioxide)) +
  geom_boxplot(aes(group = interaction(TemperatureGroup, Carbon.Dioxide, width = 0.02)), position = position_dodge(width = 2), outlier.shape = NA) +
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  scale_fill_manual(values = c("#7851A9", "#a97851")) +
  stat_smooth(method="glm", se = T) +
  theme_cowplot(12) +
  ylab("Aspect Ratio") + 
  xlab("Temperature") + 
  stat_cor(method = "pearson", size = 2) + 
  facet_wrap("Size.Class", nrow = 1, labeller = names) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_blank()) +
  labs(color= expression(paste(C0[2], "Treatment"))) +
  theme(legend.position="none")+
  scale_y_continuous(labels = label_number(accuracy = 0.01))

si <-  ggplot(data = tmp, aes(x=Temperature, y=tmp[,5], color=Carbon.Dioxide)) +
  geom_boxplot(aes(group = interaction(TemperatureGroup, Carbon.Dioxide, width = 0.02)), position = position_dodge(width = 2), outlier.shape = NA) +
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  scale_fill_manual(values = c("#7851A9", "#a97851")) +
  stat_smooth(method="glm", se = T) +
  theme_cowplot(12) +
  ylab("Sigma Intensity") + 
  xlab("Temperature") + 
  stat_cor(method = "pearson", size = 2) + 
  facet_wrap("Size.Class", nrow = 1, labeller = names) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_blank()) +
  labs(color= expression(paste(C0[2], "Treatment"))) +
  theme(legend.position="none")+
  scale_y_continuous(labels = label_number(accuracy = 0.01))

rg <-  ggplot(data = tmp, aes(x=Temperature, y=tmp[,6], color=Carbon.Dioxide)) +
  geom_boxplot(aes(group = interaction(TemperatureGroup, Carbon.Dioxide, width = 0.02)), position = position_dodge(width = 2), outlier.shape = NA) +
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  scale_fill_manual(values = c("#7851A9", "#a97851")) +
  stat_smooth(method="glm", se = T) +
  theme_cowplot(12) +
  ylab("Red:Green Ratio") + 
  xlab("Temperature") + 
  stat_cor(method = "pearson", size = 2) + 
  facet_wrap("Size.Class", nrow = 1, labeller = names) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_blank()) +
  labs(color= expression(paste(C0[2], "Treatment"))) +
  theme(legend.position="none")+
  scale_y_continuous(labels = label_number(accuracy = 0.01))

legend <- get_legend(
  rg + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom", legend.title = element_text(size = 12), legend.text = element_text(size = 12)))

Figure_Tmp <- plot_grid(vol, ar, si, rg, nrow = 4, labels = c("A", "B", "C", "D"))
FigureS4 <- plot_grid(Figure_Tmp, legend, ncol = 1, rel_heights  = c(1, .05), align = "v", axis = "lr") 

ggsave(filename= "figures/manuscript/FigureS4.pdf", 
       plot = FigureS4, width = 180, height = 250, units=c("mm"), dpi=600) 


#Figure S5
ea <- ggplot(data = data.summed, aes(x=Temperature, y=log(data.summed[,11]), color=Carbon.Dioxide)) +
  geom_boxplot(aes(group = interaction(TemperatureGroup, Carbon.Dioxide, width = 0.02)), position = position_dodge(width = 2), outlier.shape = NA) +
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  scale_fill_manual(values = c("#7851A9", "#a97851")) +
  stat_smooth(method="gam", se = T, formula = y ~ s(x, bs = "cs", k=5)) +
  theme_cowplot(12) +
  ylab("Estimated Abundance") + 
  xlab("Temperature") + 
  stat_cor(method = "pearson", size = 2)+ 
  facet_wrap("Size.Class", nrow = 1) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_blank()) +
  labs(color= expression(paste(C0[2], "Treatment"))) +
  theme(legend.position="none")

bm <- ggplot(data = data.summed, aes(x=Temperature, y=log(data.summed[,12]), color=Carbon.Dioxide)) +
  geom_boxplot(aes(group = interaction(TemperatureGroup, Carbon.Dioxide, width = 0.02)), position = position_dodge(width = 2), outlier.shape = NA) +
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  scale_fill_manual(values = c("#7851A9", "#a97851")) +
  stat_smooth(method="gam", se = T, formula = y ~ s(x, bs = "cs", k=5)) +
  theme_cowplot(12) +
  ylab("Community Biomass") + 
  xlab("Temperature") + 
  stat_cor(method = "pearson", size = 2)+ 
  facet_wrap("Size.Class", nrow = 1) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_blank()) +
  labs(color= expression(paste(C0[2], "Treatment"))) +
  theme(legend.position="none")


legend <- get_legend(
  ea + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom", legend.title = element_text(size = 12), legend.text = element_text(size = 12)))

Figure_Tmp <- plot_grid(ea, bm, nrow = 2, labels = c("A", "B"))
FigureS5 <- plot_grid(Figure_Tmp, legend, ncol = 1, rel_heights  = c(1, .05), align = "v", axis = "lr") 

ggsave(filename= "figures/manuscript/FigureS5.pdf", 
       plot = FigureS5, width = 180, height = 180, units=c("mm"), dpi=600) 


# Dumb Bell Plots
lpa <- ggplot(data = slopes[slopes$Variable == "Aspect Ratio",]) +
  # Add Segment and Points
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  geom_segment(aes(x = Size.Class, xend = Size.Class, y = start, yend = end), size = 1, data = segment_info[segment_info$Variable == "Aspect Ratio",], colour = "grey70", alpha = 0.9) +
  geom_point(aes(x = Size.Class, y = Slope, color = `CO2 Treatment`), size = 4, show.legend = TRUE) +
  theme_cowplot(12) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_hline(data = slopes[slopes$Variable == "Aspect Ratio",], aes(yintercept = Mean.Slope, color = `CO2 Treatment`), linetype = "dotted", size = 1, alpha = 0.5)  +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), 
        legend.position="none") +
  labs(colour = expression(paste(C0[2], "Treatment"))) +
  xlab("Size Class") +
  ylab("Slope") +
  ggtitle("Aspect Ratio") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.02, 0.02)

lpb <- ggplot(data = slopes[slopes$Variable == "Red to Green Ratio",]) +
  # Add Segment and Points
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  geom_segment(aes(x = Size.Class, xend = Size.Class, y = start, yend = end), size = 1, data = segment_info[segment_info$Variable == "Red to Green Ratio",], colour = "grey70", alpha = 0.9) +
  geom_point(aes(x = Size.Class, y = Slope, color = `CO2 Treatment`), size = 4, show.legend = TRUE) +
  theme_cowplot(12) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_hline(data = slopes[slopes$Variable == "Red to Green Ratio",], aes(yintercept = Mean.Slope, color = `CO2 Treatment`), linetype = "dotted", size = 1, alpha = 0.5)  +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), 
        legend.position="none") +
  labs(colour = expression(paste(C0[2], "Treatment"))) +
  xlab("Size Class") +
  ylab("Slope") +
  ggtitle("Red to Green Ratio") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.002, 0.002)

lpc <- ggplot(data = slopes[slopes$Variable == "Sigma Intensity",]) +
  # Add Segment and Points
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  geom_segment(aes(x = Size.Class, xend = Size.Class, y = start, yend = end), size = 1, data = segment_info[segment_info$Variable == "Sigma Intensity",], colour = "grey70", alpha = 0.9) +
  geom_point(aes(x = Size.Class, y = Slope, color = `CO2 Treatment`), size = 4, show.legend = TRUE) +
  theme_cowplot(12) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_hline(data = slopes[slopes$Variable == "Sigma Intensity",], aes(yintercept = Mean.Slope, color = `CO2 Treatment`), linetype = "dotted", size = 1, alpha = 0.5)  +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), 
        legend.position="none") +
  labs(colour = expression(paste(C0[2], "Treatment"))) +
  xlab("Size Class") +
  ylab("Slope") +
  ggtitle("Sigma Intensity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-1.5, 1.5)

lpd <- ggplot(data = slopes[slopes$Variable == "Volume",]) +
  # Add Segment and Points
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  geom_segment(aes(x = Size.Class, xend = Size.Class, y = start, yend = end), size = 1, data = segment_info[segment_info$Variable == "Volume",], colour = "grey70", alpha = 0.9) +
  geom_point(aes(x = Size.Class, y = Slope, color = `CO2 Treatment`), size = 4, show.legend = TRUE) +
  theme_cowplot(12) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_hline(data = slopes[slopes$Variable == "Volume",], aes(yintercept = Mean.Slope, color = `CO2 Treatment`), linetype = "dotted", size = 1, alpha = 0.5)  +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), 
        legend.position="none") +
  labs(colour = expression(paste(C0[2], "Treatment"))) +
  xlab("Size Class") +
  ylab("Slope") +
  ggtitle("Volume") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.1, 0.1)

legend <- get_legend(
  lpd + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom", legend.title = element_text(size = 12), legend.text = element_text(size = 12)))

Figure_Tmp <- plot_grid(lpd, lpa, lpc, lpb, nrow = 2, labels = c("A", "B", "C", "D"))
Figure3 <- plot_grid(Figure_Tmp, legend, ncol = 1, rel_heights  = c(1, .1), align = "v", axis = "lr") 


ggsave(filename= "figures/manuscript/Figure3.png", 
       plot = Figure3, width = 180, height = 180, units=c("mm"), dpi=600) 

# Lollipop Plots Figure 3 with Standard Errors
segment_info$Size.Class <- as.numeric(segment_info$Size.Class)
slopes$Size.Class <- as.numeric(slopes$Size.Class)

lpa <- ggplot(data = slopes[slopes$Variable == "Aspect Ratio",]) +
  # Add Segment and Points
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  geom_segment(aes(x = Size.Class + 0.05, xend = Size.Class + 0.05, y = 0, yend = end), size = 1, data = segment_info[segment_info$Variable == "Aspect Ratio",], colour = "#a97851", alpha = 0.67) +
  geom_segment(aes(x = Size.Class - 0.05, xend = Size.Class - 0.05, y = 0, yend = start), size = 1, data = segment_info[segment_info$Variable == "Aspect Ratio",], colour = "#7851A9", alpha = 0.67) +
  geom_point(data = slopes[slopes$`CO2 Treatment` == "Ambient" & slopes$Variable == "Aspect Ratio",], aes(x = Size.Class - 0.05, y = Slope), color = '#7851A9', size = 4, show.legend = FALSE) +
  geom_point(data = slopes[slopes$`CO2 Treatment` != "Ambient" & slopes$Variable == "Aspect Ratio",], aes(x = Size.Class + 0.05, y = Slope), color = '#a97851', size = 4, show.legend = FALSE) +
  theme_cowplot(12) +
  geom_hline(yintercept = 0, alpha = 0.9) +
  geom_hline(data = slopes[slopes$Variable == "Aspect Ratio",], aes(yintercept = Mean.Slope, color = `CO2 Treatment`), linetype = "dotted", size = 1, alpha = 0.5)  +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10), 
        legend.position="none") +
  labs(colour = expression(paste(CO[2], "Treatment"))) +
  xlab("Size Class") +
  ylab(expression(Beta ~ " Temperature")) +
  ggtitle("Aspect Ratio") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(-0.020, 0.020), labels = label_number(accuracy = 0.001)) +
  geom_errorbar(data = slopes[slopes$`CO2 Treatment` == "Ambient" & slopes$Variable == "Aspect Ratio",], aes(y = Slope, x = Size.Class - 0.05, ymin = Slope-SE, ymax = Slope+SE, width = 1), colour = c("#7851A9")) +
  geom_errorbar(data = slopes[slopes$`CO2 Treatment` == "Elevated" & slopes$Variable == "Aspect Ratio",], aes(y = Slope, x = Size.Class + 0.05, ymin = Slope-SE, ymax = Slope+SE, width = 1), colour = c("#a97851"))

lpb <- ggplot(data = slopes[slopes$Variable == "Red to Green Ratio",]) +
  # Add Segment and Points
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  geom_segment(aes(x = Size.Class + 0.05, xend = Size.Class + 0.05, y = 0, yend = end), size = 1, data = segment_info[segment_info$Variable == "Red to Green Ratio",], colour = "#a97851", alpha = 0.67) +
  geom_segment(aes(x = Size.Class - 0.05, xend = Size.Class - 0.05, y = 0, yend = start), size = 1, data = segment_info[segment_info$Variable == "Red to Green Ratio",], colour = "#7851A9", alpha = 0.67) +
  geom_point(data = slopes[slopes$`CO2 Treatment` == "Ambient" & slopes$Variable == "Red to Green Ratio",], aes(x = Size.Class - 0.05, y = Slope), color = '#7851A9', size = 4, show.legend = FALSE) +
  geom_point(data = slopes[slopes$`CO2 Treatment` != "Ambient" & slopes$Variable == "Red to Green Ratio",], aes(x = Size.Class + 0.05, y = Slope), color = '#a97851', size = 4, show.legend = FALSE) +
  theme_cowplot(12) +
  geom_hline(yintercept = 0, alpha = 0.9) +
  geom_hline(data = slopes[slopes$Variable == "Red to Green Ratio",], aes(yintercept = Mean.Slope, color = `CO2 Treatment`), linetype = "dotted", size = 1, alpha = 0.5)  +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10), 
        legend.position="none") +
  labs(colour = expression(paste(C0[2], "Treatment"))) +
  xlab("Size Class") +
  ylab(expression(Beta ~ " Temperature")) +
  ggtitle("Red to Green Ratio") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(-0.002, 0.002), labels = label_number(accuracy = 0.001)) +
  geom_errorbar(data = slopes[slopes$`CO2 Treatment` == "Ambient" & slopes$Variable == "Red to Green Ratio",], aes(y = Slope, x = Size.Class - 0.05, ymin = Slope-SE, ymax = Slope+SE, width = 1), colour = c("#7851A9")) +
  geom_errorbar(data = slopes[slopes$`CO2 Treatment` == "Elevated" & slopes$Variable == "Red to Green Ratio",], aes(y = Slope, x = Size.Class + 0.05, ymin = Slope-SE, ymax = Slope+SE, width = 1), colour = c("#a97851"))

lpc <- ggplot(data = slopes[slopes$Variable == "Sigma Intensity",]) +
  # Add Segment and Points
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  geom_segment(aes(x = Size.Class + 0.05, xend = Size.Class + 0.05, y = 0, yend = end), size = 1, data = segment_info[segment_info$Variable == "Sigma Intensity",], colour = "#a97851", alpha = 0.67) +
  geom_segment(aes(x = Size.Class - 0.05, xend = Size.Class - 0.05, y = 0, yend = start), size = 1, data = segment_info[segment_info$Variable == "Sigma Intensity",], colour = "#7851A9", alpha = 0.67) +
  geom_point(data = slopes[slopes$`CO2 Treatment` == "Ambient" & slopes$Variable == "Sigma Intensity",], aes(x = Size.Class - 0.05, y = Slope), color = '#7851A9', size = 4, show.legend = FALSE) +
  geom_point(data = slopes[slopes$`CO2 Treatment` != "Ambient" & slopes$Variable == "Sigma Intensity",], aes(x = Size.Class + 0.05, y = Slope), color = '#a97851', size = 4, show.legend = FALSE) +
  theme_cowplot(12) +
  geom_hline(yintercept = 0, alpha = 0.9) +
  geom_hline(data = slopes[slopes$Variable == "Sigma Intensity",], aes(yintercept = Mean.Slope, color = `CO2 Treatment`), linetype = "dotted", size = 1, alpha = 0.5)  +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10), 
        legend.position="none") +
  labs(colour = expression(paste(C0[2], "Treatment"))) +
  xlab("Size Class") +
  ylab(expression(Beta ~ " Temperature")) +
  ggtitle("Sigma Intensity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(-1.5, 1.5), labels = label_number(accuracy = 0.001)) +
  geom_errorbar(data = slopes[slopes$`CO2 Treatment` == "Ambient" & slopes$Variable == "Sigma Intensity",], aes(y = Slope, x = Size.Class - 0.05, ymin = Slope-SE, ymax = Slope+SE, width = 1), colour = c("#7851A9")) +
  geom_errorbar(data = slopes[slopes$`CO2 Treatment` == "Elevated" & slopes$Variable == "Sigma Intensity",], aes(y = Slope, x = Size.Class + 0.05, ymin = Slope-SE, ymax = Slope+SE, width = 1), colour = c("#a97851"))

lpd <- ggplot(data = slopes[slopes$Variable == "Volume",]) +
  # Add Segment and Points
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  geom_segment(aes(x = Size.Class + 0.05, xend = Size.Class + 0.05, y = 0, yend = end), size = 1, data = segment_info[segment_info$Variable == "Volume",], colour = "#a97851", alpha = 0.67) +
  geom_segment(aes(x = Size.Class - 0.05, xend = Size.Class - 0.05, y = 0, yend = start), size = 1, data = segment_info[segment_info$Variable == "Volume",], colour = "#7851A9", alpha = 0.67) +
  geom_point(data = slopes[slopes$`CO2 Treatment` == "Ambient" & slopes$Variable == "Volume",], aes(x = Size.Class - 0.05, y = Slope), color = '#7851A9', size = 4, show.legend = TRUE) +
  geom_point(data = slopes[slopes$`CO2 Treatment` != "Ambient" & slopes$Variable == "Volume",], aes(x = Size.Class + 0.05, y = Slope), color = '#a97851', size = 4, show.legend = TRUE) +
  theme_cowplot(12) +
  geom_hline(yintercept = 0, alpha = 0.9) +
  geom_hline(data = slopes[slopes$Variable == "Volume",], aes(yintercept = Mean.Slope, color = `CO2 Treatment`), linetype = "dotted", size = 1, alpha = 0.5)  +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10),
        legend.position="none") +
  labs(colour = expression(paste(C0[2], "Treatment"))) +
  xlab("Size Class") +
  ylab(expression(Beta ~ " Temperature")) +
  ggtitle("Volume") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(-0.2, 0.2), labels = label_number(accuracy = 0.001)) +
  geom_errorbar(data = slopes[slopes$`CO2 Treatment` == "Ambient" & slopes$Variable == "Volume",], aes(y = Slope, x = Size.Class - 0.05, ymin = Slope-SE, ymax = Slope+SE, width = 1), colour = c("#7851A9")) +
  geom_errorbar(data = slopes[slopes$`CO2 Treatment` == "Elevated" & slopes$Variable == "Volume",], aes(y = Slope, x = Size.Class + 0.05, ymin = Slope-SE, ymax = Slope+SE, width = 1), colour = c("#a97851"))

legend <- get_legend(
  lpd + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom", legend.title = element_text(size = 12), legend.text = element_text(size = 12)))

Figure_Tmp <- plot_grid(lpd, lpa, lpc, lpb, nrow = 2, labels = c("A", "B", "C", "D"), label_size = 14)
Figure3 <- plot_grid(Figure_Tmp, legend, ncol = 1, rel_heights  = c(1, .1), align = "v", axis = "lr") 

ggsave(filename= "figures/manuscript/Figure3_SE.pdf", 
       plot = Figure3, width = 180, height = 180, units=c("mm"), dpi=600) 

#r supplemental of Distance between ambient and elevated in the segment plots
segment_info$percent <- ((abs(segment_info$end) - abs(segment_info$start))/abs(segment_info$end))*100

segment_difference <- ggplot(data = segment_info, aes(x = Size.Class, y = difference, color = Variable)) +
  stat_smooth(method = "glm", se = F) +
  geom_point(size = 4, show.legend = FALSE, alpha = 0.5) +
  theme_cowplot(12)  +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  xlab("Size Class") +
  ylab("Difference in Slopes") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= "figures/manuscript/FigureS15.pdf", 
       plot = segment_difference, width = 180, height = 180, units=c("mm"), dpi=600) 

# Coefficient Plots for glms
tmp <- data.clustered[,c("Size.Class", "Carbon.Dioxide", "Temperature", "Geodesic.Aspect.Ratio", 
                         "Sigma.Intensity", "Ratio.Red.Green", "Volume")]

tmp$Temperature <- as.character(tmp$Temperature)
tmp$Temperature <- as.numeric(tmp$Temperature)
tmp$Size.Class <- as.factor(tmp$Size.Class)

aspect.ratio <- glm(Geodesic.Aspect.Ratio ~ Temperature*Carbon.Dioxide*Size.Class, data = tmp)
mean.mass <- glm(Volume ~ Temperature*Carbon.Dioxide*Size.Class, data = tmp)
red.green <- glm(Ratio.Red.Green ~ Temperature*Carbon.Dioxide*Size.Class, data = tmp)
sigma.intensity <- glm(Sigma.Intensity ~ Temperature*Carbon.Dioxide*Size.Class, data = tmp)

colnames(data.summed)[11] <- "adj.count"
colnames(data.summed)[12] <- "adj.mass"

biomass <- glm(adj.mass ~ Temperature*Carbon.Dioxide*Size.Class, data = data.summed)
abundance <- glm(adj.count ~ Temperature*Carbon.Dioxide*Size.Class, data = data.summed)

#Aspect.Ratio
g <- ggcoef_model(aspect.ratio, variable_labels = c(Temperature = "Temp",
                                                    Carbon.Dioxide = "C02",
                                                    Size.Class = "Size Class"),
                  show_p_values = FALSE,
                  stripped_rows = FALSE,
                  interaction_sep = " x ",
                  facet_labeller = label_wrap_gen(12)) + 
  ggtitle("Aspect Ratio ~ Temperature x CO2 x Size Class")

ggsave(filename= "figures/manuscript/FigureS7.pdf", 
       plot = g, width = 180, height=120, units=c("mm"), dpi=300)

#Sigma.Intensity
g <- ggcoef_model(sigma.intensity, variable_labels = c(Temperature = "Temp",
                                                       Carbon.Dioxide = "C02",
                                                       Size.Class = "Size Class"),
                  show_p_values = FALSE,
                  stripped_rows = FALSE,
                  interaction_sep = " x ",
                  facet_labeller = label_wrap_gen(12)) + 
  ggtitle("Sigma Intensity ~ Temperature x CO2 x Size Class")

ggsave(filename= "figures/manuscript/FigureS8.pdf", 
       plot = g, width = 180, height=120, units=c("mm"), dpi=300)

#Volume
g <- ggcoef_model(mean.mass, variable_labels = c(Temperature = "Temp",
                                                 Carbon.Dioxide = "C02",
                                                 Size.Class = "Size Class"),
                  show_p_values = FALSE,
                  stripped_rows = FALSE,
                  interaction_sep = " x ",
                  facet_labeller = label_wrap_gen(12)) + 
  ggtitle("Volume ~ Temperature x CO2 x Size Class")

ggsave(filename= "figures/manuscript/FigureS6.pdf", 
       plot = g, width = 180, height=120, units=c("mm"), dpi=300)

#Red.Green.Ratio
g <- ggcoef_model(red.green, variable_labels = c(Temperature = "Temp",
                                                 Carbon.Dioxide = "C02",
                                                 Size.Class = "Size Class"),
                  show_p_values = FALSE,
                  stripped_rows = FALSE,
                  interaction_sep = " x ",
                  facet_labeller = label_wrap_gen(12)) + 
  ggtitle("Red/Green Ratio ~ Temperature x CO2 x Size Class")

ggsave(filename= "figures/manuscript/figureS9.pdf", 
       plot = g, width = 180, height=120, units=c("mm"), dpi=300)

# Model outputs as tables
aspect.ratio <- glm(Geodesic.Aspect.Ratio ~ Temperature*Carbon.Dioxide*Size.Class, data = tmp)
mean.mass <- glm(Volume ~ Temperature*Carbon.Dioxide*Size.Class, data = tmp)
red.green <- glm(Ratio.Red.Green ~ Temperature*Carbon.Dioxide*Size.Class, data = tmp)
sigma.intensity <- glm(Sigma.Intensity ~ Temperature*Carbon.Dioxide*Size.Class, data = tmp)

aspect.ratio %>%
  tbl_regression(
    exponentiate = TRUE, 
    pvalue_fun = ~style_pvalue(.x, digits = 2),
  ) %>% 
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

tmp <- data.selected

# Models with no size classes
aspect.ratio.full <- glm(Geodesic.Aspect.Ratio ~ Temperature*Carbon.Dioxide*Geodesic.Length, data = tmp)
mean.mass.full <- glm(adj.mass ~ Temperature*Carbon.Dioxide*Geodesic.Length, data = tmp)
red.green.full <- glm(Ratio.Red.Green ~ Temperature*Carbon.Dioxide*Geodesic.Length, data = tmp)
sigma.intensity.full <- glm(Sigma.Intensity ~ Temperature*Carbon.Dioxide*Geodesic.Length, data = tmp)

#Aspect.Ratio
g <- ggcoef_model(aspect.ratio.full, variable_labels = c(Temperature = "Temp",
                                                         Carbon.Dioxide = "C02"),
                  facet_labeller = label_wrap_gen(10))

ggsave(filename= "figures/manuscript/Beta.LM.Aspect.Ratio.Full.png", 
       plot = g, width = 25, height=25, units=c("cm"), dpi=300)

#Sigma.Intensity
g <- ggcoef_model(sigma.intensity.full, variable_labels = c(Temperature = "Temp",
                                                            Carbon.Dioxide = "C02"),
                  facet_labeller = label_wrap_gen(10))

ggsave(filename= "figures/manuscript/Manuscript/Beta.LM.Sigma.Intensity.Full.png", 
       plot = g, width = 25, height=25, units=c("cm"), dpi=300)

#Mean.Mass
g <- ggcoef_model(mean.mass.full, variable_labels = c(Temperature = "Temp",
                                                      Carbon.Dioxide = "C02"),
                  facet_labeller = label_wrap_gen(10))

ggsave(filename= "figures/manuscript/Beta.LM.Mean.Mass.Full.png", 
       plot = g, width = 25, height=25, units=c("cm"), dpi=300)

#Red.Green.Ratio
g <- ggcoef_model(red.green.full, variable_labels = c(Temperature = "Temp",
                                                      Carbon.Dioxide = "C02"),
                  facet_labeller = label_wrap_gen(10))

ggsave(filename= "figures/manuscript/Beta.LM.Red.Green.Ratio.Full.png", 
       plot = g, width = 25, height=25, units=c("cm"), dpi=300)

# Two-Dimensional Plot of Red/Green Ratio and Biomass Change by Size Class

# calculate relative changes for data
tmp <- aggregate(.~ Size.Class + Temperature + Carbon.Dioxide, data.clustered, mean)

tmp.reduced <- tmp[,c("Carbon.Dioxide", "Size.Class", "Temperature","Ratio.Red.Green", "Sigma.Intensity")]

for(i in 1:nrow(tmp.reduced)){
  if(i <= 5){
    tmp.reduced$ratio.change[i] <- 0
  }
  if(i > 5 & i <= 10){
    tmp.reduced$ratio.change[i] <- ((tmp.reduced$Ratio.Red.Green[i] - tmp.reduced$Ratio.Red.Green[i - 5])/tmp.reduced$Ratio.Red.Green[i - 5])
  }
  if(i > 10 & i <= 15){
    tmp.reduced$ratio.change[i] <- ((tmp.reduced$Ratio.Red.Green[i] - tmp.reduced$Ratio.Red.Green[i - 10])/tmp.reduced$Ratio.Red.Green[i - 10])
  }
  if(i > 15 & i <= 20){
    tmp.reduced$ratio.change[i] <- ((tmp.reduced$Ratio.Red.Green[i] - tmp.reduced$Ratio.Red.Green[i - 15])/tmp.reduced$Ratio.Red.Green[i - 15])
  }
  if(i > 20 & i <= 25){
    tmp.reduced$ratio.change[i] <- ((tmp.reduced$Ratio.Red.Green[i] - tmp.reduced$Ratio.Red.Green[i - 20])/tmp.reduced$Ratio.Red.Green[i - 20])
  }
  if(i > 25 & i <= 30){
    tmp.reduced$ratio.change[i] <- 0
  }
  if(i > 30 & i <= 35){
    tmp.reduced$ratio.change[i] <- ((tmp.reduced$Ratio.Red.Green[i] - tmp.reduced$Ratio.Red.Green[i - 5])/tmp.reduced$Ratio.Red.Green[i - 5])
  }
  if(i > 35 & i <= 40){
    tmp.reduced$ratio.change[i] <- ((tmp.reduced$Ratio.Red.Green[i] - tmp.reduced$Ratio.Red.Green[i - 10])/tmp.reduced$Ratio.Red.Green[i - 10])
  }
  if(i > 40 & i <= 45){
    tmp.reduced$ratio.change[i] <- ((tmp.reduced$Ratio.Red.Green[i] - tmp.reduced$Ratio.Red.Green[i - 15])/tmp.reduced$Ratio.Red.Green[i - 15])
  }
  if(i > 45 & i <= 50){
    tmp.reduced$ratio.change[i] <- ((tmp.reduced$Ratio.Red.Green[i] - tmp.reduced$Ratio.Red.Green[i - 20])/tmp.reduced$Ratio.Red.Green[i - 20])
  }
}

for(i in 1:nrow(tmp.reduced)){
  if(i <= 5){
    tmp.reduced$sigma.change[i] <- 0
  }
  if(i > 5 & i <= 10){
    tmp.reduced$sigma.change[i] <- ((tmp.reduced$Sigma.Intensity[i] - tmp.reduced$Sigma.Intensity[i - 5])/tmp.reduced$Sigma.Intensity[i - 5])
  }
  if(i > 10 & i <= 15){
    tmp.reduced$sigma.change[i] <- ((tmp.reduced$Sigma.Intensity[i] - tmp.reduced$Sigma.Intensity[i - 10])/tmp.reduced$Sigma.Intensity[i - 10])
  }
  if(i > 15 & i <= 20){
    tmp.reduced$sigma.change[i] <- ((tmp.reduced$Sigma.Intensity[i] - tmp.reduced$Sigma.Intensity[i - 15])/tmp.reduced$Sigma.Intensity[i - 15])
  }
  if(i > 20 & i <= 25){
    tmp.reduced$sigma.change[i] <- ((tmp.reduced$Sigma.Intensity[i] - tmp.reduced$Sigma.Intensity[i - 20])/tmp.reduced$Sigma.Intensity[i - 20])
  }
  if(i > 25 & i <= 30){
    tmp.reduced$sigma.change[i] <- 0
  }
  if(i > 30 & i <= 35){
    tmp.reduced$sigma.change[i] <- ((tmp.reduced$Sigma.Intensity[i] - tmp.reduced$Sigma.Intensity[i - 5])/tmp.reduced$Sigma.Intensity[i - 5])
  }
  if(i > 35 & i <= 40){
    tmp.reduced$sigma.change[i] <- ((tmp.reduced$Sigma.Intensity[i] - tmp.reduced$Sigma.Intensity[i - 10])/tmp.reduced$Sigma.Intensity[i - 10])
  }
  if(i > 40 & i <= 45){
    tmp.reduced$sigma.change[i] <- ((tmp.reduced$Sigma.Intensity[i] - tmp.reduced$Sigma.Intensity[i - 15])/tmp.reduced$Sigma.Intensity[i - 15])
  }
  if(i > 45 & i <= 50){
    tmp.reduced$sigma.change[i] <- ((tmp.reduced$Sigma.Intensity[i] - tmp.reduced$Sigma.Intensity[i - 20])/tmp.reduced$Sigma.Intensity[i - 20])
  }
}

tmp.sum <- aggregate(.~ Size.Class + Temperature + Carbon.Dioxide, data.clustered, sum)
tmp.sum$Temperature <- as.character(tmp.sum$Temperature)
tmp.sum$Temperature <- as.numeric(tmp.sum$Temperature)
tmp.sum$Size.Class <- as.factor(tmp.sum$Size.Class)

for(i in 1:nrow(tmp.sum)){
  if(i <= 5){
    tmp.sum$ratio.change[i] <- 0
  }
  if(i > 5 & i <= 10){
    tmp.sum$ratio.change[i] <- ((tmp.sum$adj.mass[i] - tmp.sum$adj.mass[i - 5])/tmp.sum$adj.mass[i - 5])
  }
  if(i > 10 & i <= 15){
    tmp.sum$ratio.change[i] <- ((tmp.sum$adj.mass[i] - tmp.sum$adj.mass[i - 10])/tmp.sum$adj.mass[i - 10])
  }
  if(i > 15 & i <= 20){
    tmp.sum$ratio.change[i] <- ((tmp.sum$adj.mass[i] - tmp.sum$adj.mass[i - 15])/tmp.sum$adj.mass[i - 15])
  }
  if(i > 20 & i <= 25){
    tmp.sum$ratio.change[i] <- ((tmp.sum$adj.mass[i] - tmp.sum$adj.mass[i - 20])/tmp.sum$adj.mass[i - 20])
  }
  if(i > 25 & i <= 30){
    tmp.sum$ratio.change[i] <- 0
  }
  if(i > 30 & i <= 35){
    tmp.sum$ratio.change[i] <- ((tmp.sum$adj.mass[i] - tmp.sum$adj.mass[i - 5])/tmp.sum$adj.mass[i - 5])
  }
  if(i > 35 & i <= 40){
    tmp.sum$ratio.change[i] <- ((tmp.sum$adj.mass[i] - tmp.sum$adj.mass[i - 10])/tmp.sum$adj.mass[i - 10])
  }
  if(i > 40 & i <= 45){
    tmp.sum$ratio.change[i] <- ((tmp.sum$adj.mass[i] - tmp.sum$adj.mass[i - 15])/tmp.sum$adj.mass[i - 15])
  }
  if(i > 45 & i <= 50){
    tmp.sum$ratio.change[i] <- ((tmp.sum$adj.mass[i] - tmp.sum$adj.mass[i - 20])/tmp.sum$adj.mass[i - 20])
  }
}

tmp.sum <- tmp.sum[,c("Size.Class", "Temperature", "Carbon.Dioxide", "ratio.change")]
tmp.reduced <- tmp.reduced[,c("Size.Class", "Temperature", "Carbon.Dioxide", "ratio.change", "sigma.change")]
colnames(tmp.sum)[4] <- "Biomass.Change"
colnames(tmp.reduced)[4] <- "Ratio.Change"
colnames(tmp.reduced)[5] <- "Sigma.Change"

tmp.change <- cbind(tmp.reduced, tmp.sum$Biomass.Change)

tmp.change.ambient <- tmp.change[tmp.change$Carbon.Dioxide == "Ambient",] 
tmp.change.ambient$Ratio.Normalized <- (tmp.change.ambient$Ratio.Change - 0)/(max(abs(tmp.change.ambient$Ratio.Change)) - 0)
tmp.change.ambient$Sigma.Normalized <- (tmp.change.ambient$Sigma.Change - 0)/(max(abs(tmp.change.ambient$Sigma.Change)) - 0)
tmp.change.ambient$Biomass.Normalized <- (tmp.change.ambient$`tmp.sum$Biomass.Change` - 0)/(max(abs(tmp.change.ambient$`tmp.sum$Biomass.Change`)) - 0)

tmp.change.elevated <- tmp.change[tmp.change$Carbon.Dioxide == "Elevated",] 
tmp.change.elevated$Ratio.Normalized <- (tmp.change.elevated$Ratio.Change - 0)/(max(abs(tmp.change.elevated$Ratio.Change)) - 0)
tmp.change.elevated$Sigma.Normalized <- (tmp.change.elevated$Sigma.Change - 0)/(max(abs(tmp.change.elevated$Sigma.Change)) - 0)
tmp.change.elevated$Biomass.Normalized <- (tmp.change.elevated$`tmp.sum$Biomass.Change` - 0)/(max(abs(tmp.change.elevated$`tmp.sum$Biomass.Change`)) - 0)

tmp.change <- rbind(tmp.change.ambient, tmp.change.elevated)

tmp.change <- tmp.change[,c("Size.Class", "Temperature", "Carbon.Dioxide", "Biomass.Normalized", "Ratio.Normalized", "Sigma.Normalized")]

# Figure 6
tmp.matrix <- tmp.change[tmp.change$Temperature == 9 | tmp.change$Temperature == 0, 1:3]

#Calculate with regressions predicted Trait Values
tmp.matrix$Biomass <- predict(biomass, tmp.matrix, type="response")
for(i in 1:nrow(tmp.matrix)){
  if(tmp.matrix$Biomass[i] <= 0){
    tmp.matrix$Biomass[i] <-  0
  }
}
tmp.matrix$Sigma.Intensity <- predict(GLM_Sigma.Intensity, tmp.matrix, type="response")
tmp.matrix$Red.Green.Ratio <- predict(GLM_Red.Green.Ratio, tmp.matrix, type="response")

tmp.stats <- tmp.matrix[tmp.matrix$Temperature == 9,]
tmp.stats$Biomass <- NA
tmp.stats$Sigma.Intensity <- NA
tmp.stats$Red.Green.Ratio <- NA

for(i in 1:nrow(tmp.stats)){
  if(i <= 5){
    tmp.stats[i,4] <- tmp.matrix[i+5, 4] - tmp.matrix[i, 4]
    tmp.stats[i,5] <- tmp.matrix[i+5, 5] - tmp.matrix[i, 5]
    tmp.stats[i,6] <- tmp.matrix[i+5, 6] - tmp.matrix[i, 6]
  }
  if(i > 5){
    tmp.stats[i,4] <- tmp.matrix[i+10, 4] - tmp.matrix[i+5, 4]
    tmp.stats[i,5] <- tmp.matrix[i+10, 5] - tmp.matrix[i+5, 5]
    tmp.stats[i,6] <- tmp.matrix[i+10, 6] - tmp.matrix[i+5, 6]
  }
}

tmp.normalized <- tmp.stats
tmp.normalized.ambient <- tmp.normalized[tmp.normalized$Carbon.Dioxide == "Ambient",] 
tmp.normalized.ambient$Ratio.Normalized <- (tmp.normalized.ambient$Red.Green.Ratio - 0)/(max(abs(tmp.normalized.ambient$Red.Green.Ratio)) - 0)
tmp.normalized.ambient$Sigma.Normalized <- (tmp.normalized.ambient$Sigma.Intensity - 0)/(max(abs(tmp.normalized.ambient$Sigma.Intensity)) - 0)
tmp.normalized.ambient$Biomass.Normalized <- (tmp.normalized.ambient$Biomass - 0)/(max(abs(tmp.normalized.ambient$Biomass)) - 0)

tmp.normalized.elevated <- tmp.normalized[tmp.normalized$Carbon.Dioxide == "Elevated",] 
tmp.normalized.elevated$Ratio.Normalized <- (tmp.normalized.elevated$Red.Green.Ratio - 0)/(max(abs(tmp.normalized.elevated$Red.Green.Ratio)) - 0)
tmp.normalized.elevated$Sigma.Normalized <- (tmp.normalized.elevated$Sigma.Intensity - 0)/(max(abs(tmp.normalized.elevated$Sigma.Intensity)) - 0)
tmp.normalized.elevated$Biomass.Normalized <- (tmp.normalized.elevated$Biomass - 0)/(max(abs(tmp.normalized.elevated$Biomass)) - 0)

tmp.change <- rbind(tmp.normalized.ambient, tmp.normalized.elevated)
tmp.change <- tmp.change[,-4]
tmp.change <- tmp.change[,-4]
tmp.change <- tmp.change[,-4]

tmp.change$Size.Class <- factor(tmp.change$Size.Class, levels = c("5", "4", "3", "2", "1"))
diff_plot <- ggplot(data = tmp.change, aes(x = Ratio.Normalized, y = Biomass.Normalized)) +
  geom_segment(data = tmp.change, aes(x = 0, y = 0, xend = Ratio.Normalized, yend = Biomass.Normalized, colour = Size.Class, linetype = Carbon.Dioxide), size = 0.75) +
  xlim(-1, 1) +
  ylim(-1, 1) +
  xlab(expression(paste("Normalized ", Delta, "Red/Green Ratio"))) +
  ylab(expression(paste("Normalized ", Delta, "Biomass"))) +
  theme_cowplot(7) + 
  geom_vline(xintercept = 0, size = 0.5, alpha = 0.3) +
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.3)  +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10)) +
  labs(colour= "Size Class") +
  labs(linetype = expression(paste(CO[2], "Treatment"))) +
  scale_color_viridis(discrete = T, option = "D")


ggsave(filename= "figures/manuscript/Figure5.pdf", 
       plot = diff_plot, width = 90, height=80, units=c("mm"), dpi=600) 

# Figure S10
#Three-axis ggplot figure x 4
tmp.difference <- as.data.frame(matrix(data = NA, nrow = 5, ncol = 2))
tmp.difference$V1 <- abs(slopes[slopes$Variable == "Volume" & slopes$`CO2 Treatment` == 
                                  "Ambient",4] - slopes[slopes$Variable == "Volume" & slopes$`CO2 Treatment` == 
                                                          "Elevated",4])
tmp.difference$V2 <- c(1, 2, 3, 4, 5)
vd<- ggplot(data = tmp.difference, aes(x = V2, y = V1)) +
  stat_smooth(method="glm", se = T, aes(color = "#005BBB")) + 
  stat_cor(method = "pearson", size = 4)+ 
  theme_cowplot(7) + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10), 
        legend.position="none") +
  xlab("Size Class") +
  ylab(expression("["~Beta ~ "Ambient - "~Beta ~"Elevated"~"]")) +
  ggtitle("Volume") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = label_number(accuracy = 0.001))

tmp.difference <- as.data.frame(matrix(data = NA, nrow = 5, ncol = 2))
tmp.difference$V1 <- abs(slopes[slopes$Variable == "Aspect Ratio" & slopes$`CO2 Treatment` == 
                                  "Ambient",4] - slopes[slopes$Variable == "Aspect Ratio" & slopes$`CO2 Treatment` == 
                                                          "Elevated",4])
tmp.difference$V2 <- c(1, 2, 3, 4, 5)
ard <- ggplot(data = tmp.difference, aes(x = V2, y = V1)) +
  stat_smooth(method="glm", se = T, aes(color = "#005BBB")) + 
  stat_cor(method = "pearson", size = 4)+
  theme_cowplot(7) + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10), 
        legend.position="none") +
  xlab("Size Class") +
  ylab(expression("["~Beta ~ "Ambient - "~Beta ~"Elevated"~"]")) +
  ggtitle("Aspect Ratio") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = label_number(accuracy = 0.001))

tmp.difference <- as.data.frame(matrix(data = NA, nrow = 5, ncol = 2))
tmp.difference$V1 <- abs(slopes[slopes$Variable == "Sigma Intensity" & slopes$`CO2 Treatment` == 
                                  "Ambient",4] - slopes[slopes$Variable == "Sigma Intensity" & slopes$`CO2 Treatment` == 
                                                          "Elevated",4])
tmp.difference$V2 <- c(1, 2, 3, 4, 5)
sid <- ggplot(data = tmp.difference, aes(x = V2, y = V1)) +
  stat_smooth(method="glm", se = T, aes(color = "#005BBB")) + 
  stat_cor(method = "pearson", size = 4)+
  theme_cowplot(7) + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10), 
        legend.position="none") +
  xlab("Size Class") +
  ylab(expression("["~Beta ~ "Ambient - "~Beta ~"Elevated"~"]")) +
  ggtitle("Sigma Intensity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = label_number(accuracy = 0.001))

tmp.difference <- as.data.frame(matrix(data = NA, nrow = 5, ncol = 2))
tmp.difference$V1 <- abs(slopes[slopes$Variable == "Red to Green Ratio" & slopes$`CO2 Treatment` == 
                                  "Ambient",4] - slopes[slopes$Variable == "Red to Green Ratio" & slopes$`CO2 Treatment` == 
                                                          "Elevated",4])
tmp.difference$V2 <- c(1, 2, 3, 4, 5)
rgd <- ggplot(data = tmp.difference, aes(x = V2, y = V1)) +
  stat_smooth(method="glm", se = T, aes(color = "#005BBB")) + 
  stat_cor(method = "pearson", size = 4)+
  theme_cowplot(7) + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10), 
        legend.position="none") +
  xlab("Size Class") +
  ylab(expression("["~Beta ~ "Ambient - "~Beta ~"Elevated"~"]")) +
  ggtitle("Red/Green Ratio") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = label_number(accuracy = 0.001))

Figure_Tmp <- plot_grid(vd, ard, sid, rgd, nrow = 2, labels = c("A", "B", "C", "D"), label_size = 14)

ggsave(filename= "figures/manuscript/FigureS11.pdf", 
       plot = Figure_Tmp, width = 180, height = 180, units=c("mm"), dpi=600) 

# testing log-log relationships
data.logs <- aggregate(. ~ Carbon.Dioxide + Size.Class, data = data.clustered, mean)
data.logs <- data.logs[,c("Carbon.Dioxide", "Size.Class", "Geodesic.Aspect.Ratio", "Ratio.Red.Green", "Sigma.Intensity", "Volume", "Geodesic.Length")]

# Old Way
colnames(data.logs) <- c("CO2 Treatment", "Size.Class", "Aspect Ratio", "Red to Green Ratio", "Sigma Intensity", "Volume", "Length")

valuecol <- "mean"
keycol <- "Variable"
gathercols <- c("Aspect Ratio", "Red to Green Ratio", "Sigma Intensity", "Volume")

data.long <- gather(data.logs, keycol, valuecol, gathercols)

colnames(data.long)[4] <- "Variable"
colnames(data.long)[5] <- "Value"

log.log <- merge(data.long, slopes, by = c("Variable", "CO2 Treatment", "Size.Class"))
log.log <- log.log[,1:6]

log.log$`Mean Volume` <- log.log$Length

drop <- c(10, 20, 30, 40)

log.log.tmp <- log.log[-drop,]

formula <- y ~ x

#Volume
log.log2 <- log.log.tmp[log.log.tmp$Variable == "Volume",]
log.log2.amb <- log.log.tmp[log.log.tmp$Variable == "Volume" & log.log.tmp$`CO2 Treatment` == "Ambient",]
log.log2.elv <-  log.log.tmp[log.log.tmp$Variable == "Volume" & log.log.tmp$`CO2 Treatment` == "Elevated",]

log.amb <- glm(log(abs(Slope)) ~ `Mean Volume`, data = log.log2.amb)
log.elv <- glm(log(abs(Slope)) ~ `Mean Volume`, data = log.log2.elv)

log.volume <- ggplot(data = log.log2, aes(y = log(abs(Slope)), x = `Mean Volume`, color = `CO2 Treatment`)) +
  # Add Segment and Points
  scale_colour_manual(values = c(purple, gold)) +
  stat_smooth(method="glm", se = T) +
  theme_cowplot(12)  +
  #geom_point(aes(x = `Mean Volume`, y = log(abs(Slope)), color = `CO2 Treatment`)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 24),
        legend.text = element_text(size = 20), 
        legend.position="bottom") +
  xlab("ln Mean Geodesic Length") +
  ylab(expression("ln Absolute" ~ beta ~ "Temperature")) +
  ggtitle("Volume") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_poly_eq(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~")),
               formula = formula, 
               parse = TRUE, size = 8) + 
  stat_fit_glance(method = 'lm', method.args = list(formula = formula),
                  geom = 'text', aes(label = paste("P= ", 
                                                   signif(..p.value.., digits = 3), sep = "")), size = 5)

ggsave(filename= "figures/manuscript/Figure4V.pdf", 
       plot = log.volume, width = 180, height = 180, units=c("mm"), dpi=600) 

#Aspect Ratio
log.log2 <- log.log.tmp[log.log.tmp$Variable == "Aspect Ratio",]
log.log2.amb <- log.log.tmp[log.log.tmp$Variable == "Aspect Ratio" & log.log.tmp$`CO2 Treatment` == "Ambient",]
log.log2.elv <-  log.log.tmp[log.log.tmp$Variable == "Aspect Ratio" & log.log.tmp$`CO2 Treatment` == "Elevated",]

log.amb <- glm(log(abs(Slope)) ~ `Mean Volume`, data = log.log2.amb)
log.elv <- glm(log(abs(Slope)) ~ `Mean Volume`, data = log.log2.elv)

log.ar <- ggplot(data = log.log2, aes(y = log(abs(Slope)), x = `Mean Volume`, color = `CO2 Treatment`)) +
  # Add Segment and Points
  scale_colour_manual(values = c(purple, gold)) +
  stat_smooth(method="glm", se = T) +
  theme_cowplot(12)  +
  #geom_point(aes(x = `Mean Volume`, y = log(abs(Slope)), color = `CO2 Treatment`)) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10), 
        legend.position="none") +
  xlab("ln Mean Geodesic Length") +
  ylab(expression("ln Absolute" ~ beta ~ "Temperature")) +
  ggtitle("Aspect Ratio") +
  theme(plot.title = element_text(hjust = 0.5))  +
  stat_poly_eq(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~")),
               formula = formula, 
               parse = TRUE, size = 3) + 
  stat_fit_glance(method = 'lm', method.args = list(formula = formula),
                  geom = 'text', aes(label = paste("P= ", 
                                                   signif(..p.value.., digits = 3), sep = "")), size = 3)

#Sigma Intensity
log.log2 <- log.log.tmp[log.log.tmp$Variable == "Sigma Intensity",]
log.log2.amb <- log.log.tmp[log.log.tmp$Variable == "Sigma Intensity" & log.log.tmp$`CO2 Treatment` == "Ambient",]
log.log2.elv <-  log.log.tmp[log.log.tmp$Variable == "Sigma Intensity" & log.log.tmp$`CO2 Treatment` == "Elevated",]

log.amb <- glm(log(abs(Slope)) ~ `Mean Volume`, data = log.log2.amb)
log.elv <- glm(log(abs(Slope)) ~ `Mean Volume`, data = log.log2.elv)

log.si <- ggplot(data = log.log2, aes(y = log(abs(Slope)), x = `Mean Volume`, color = `CO2 Treatment`)) +
  # Add Segment and Points
  scale_colour_manual(values = c(purple, gold)) +
  stat_smooth(method="glm", se = T) +
  theme_cowplot(12)  +
  #geom_point(aes(x = `Mean Volume`, y = log(abs(Slope)), color = `CO2 Treatment`)) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10), 
        legend.position="none") +
  xlab("ln Mean Geodesic Length") +
  ylab(expression("ln Absolute" ~ beta ~ "Temperature")) +
  ggtitle("Sigma Intensity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_poly_eq(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~")),
               formula = formula, 
               parse = TRUE, size = 3) + 
  stat_fit_glance(method = 'lm', method.args = list(formula = formula),
                  geom = 'text', aes(label = paste("P= ", 
                                                   signif(..p.value.., digits = 3), sep = "")), size = 3)

#Red to Green
log.log2 <- log.log.tmp[log.log.tmp$Variable == "Red to Green Ratio",]
log.log2.amb <- log.log.tmp[log.log.tmp$Variable == "Red to Green Ratio" & log.log.tmp$`CO2 Treatment` == "Ambient",]
log.log2.elv <-  log.log.tmp[log.log.tmp$Variable == "Red to Green Ratio" & log.log.tmp$`CO2 Treatment` == "Elevated",]

log.amb <- glm(log(abs(Slope)) ~ `Mean Volume`, data = log.log2.amb)
log.elv <- glm(log(abs(Slope)) ~ `Mean Volume`, data = log.log2.elv)

log.rg <- ggplot(data = log.log2, aes(y = log(abs(Slope)), x = `Mean Volume`, color = `CO2 Treatment`)) +
  # Add Segment and Points
  scale_colour_manual(values = c(purple, gold)) +
  stat_smooth(method="glm", se = T) +
  theme_cowplot(12)  +
  #geom_point(aes(x = `Mean Volume`, y = log(abs(Slope)), color = `CO2 Treatment`)) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10), 
        legend.position="none") +
  xlab("ln Mean Geodesic Length") +
  ylab(expression("ln Absolute" ~ beta ~ "Temperature")) +
  ggtitle("Red/Green Ratio") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_poly_eq(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~")),
               formula = formula, 
               parse = TRUE, size = 3) + 
  stat_fit_glance(method = 'lm', method.args = list(formula = formula),
                  geom = 'text', aes(label = paste("P= ", 
                                                   signif(..p.value.., digits = 3), sep = "")), size = 3)

legend <- get_legend(
  log.rg + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom", legend.title = element_text(size = 12), legend.text = element_text(size = 12)))

Figure_Tmp <- plot_grid(log.ar, log.si, log.rg, nrow = 1, labels = c("A", "B", "C"), label_size = 14)

FigureS10 <- plot_grid(Figure_Tmp, legend, ncol = 1, rel_heights  = c(1, .1), align = "v", axis = "lr") 

ggsave(filename= "figures/manuscript/FigureS10.pdf", 
       plot = FigureS10, width = 180, height = 90, units=c("mm"), dpi=600) 

# Calculating functional diversity
data.function <- data.clustered[,c("Plot", "Size.Class", "Geodesic.Aspect.Ratio",
                                   "Ratio.Red.Green", "Sigma.Intensity", "Volume")]

data.info <- unique(data.clustered[,c("Plot", "Temperature", "Carbon.Dioxide")])
colnames(data.info)[1] <- "site"

data.function <- aggregate(. ~ Size.Class, data.function, mean)
row.names(data.function) <- data.function$Size.Class
row.names(data.function) <- c("Species 5", "Species 4", "Species 3", "Species 2", "Species 1")
data.traits <- data.function[,3:6]
data.species <- data.summed[,c("Plot", "Size.Class", "adj.count")]
data.species <- aggregate(. ~ Size.Class + Plot, data.species, sum)

data.sites <- data.species %>% 
  pivot_wider(
    id_cols = "Plot",
    names_from = "Size.Class",
    values_from = "adj.count"
  )

colnames(data.sites) <- c("Site", "Species 5", "Species 4", "Species 3", "Species 2", "Species 1")
data.sites <- as.data.frame(data.sites)
row.names(data.sites) <- data.sites$Site
data.sites <- data.sites[,-1]

# Calculate functional diversity
fd.FRic <- fd_fric(data.traits, data.sites)
fd.FEve <- fd_feve(data.traits, data.sites)
fd.FDiv <- fd_fdiv(data.traits, data.sites)

richness <- cbind(fd.FRic, data.info)
evenness <- cbind(fd.FEve, data.info)
divergence <- cbind(fd.FDiv, data.info)

divergence <- divergence[,c("FDiv", "Temperature", "Carbon.Dioxide")]
evenness <- evenness[,c("FEve", "Temperature", "Carbon.Dioxide")]

# Define the Carbon Dioxide and Temperature treatments
carbon_dioxide_treatments <- c("Ambient", "Elevated")
temperature_treatments <- c(0, 2.25, 4.5, 6.75, 9)

# Create an empty data frame to store the results
results_df <- data.frame(
  Carbon_Dioxide = character(),
  Temperature = numeric(),
  Functional_Diversity = numeric()
)

# Create a progress bar
pb <- progress_bar$new(
  format = "[:bar] :percent | ETA: :eta",
  total = length(carbon_dioxide_treatments) * length(temperature_treatments)
)

# Iterate through Carbon Dioxide and Temperature treatments
for (co2 in carbon_dioxide_treatments) {
  for (temp in temperature_treatments) {
    # Update the progress bar
    pb$tick()
    
    # Subset data for the current combination of treatments
    subset_data <- data.clustered[data.clustered$Carbon.Dioxide == co2 & data.clustered$Temperature == temp, ]
    
    # Calculate functional diversity for the current combination
    data.function <- subset_data[, c("Size.Class", "Geodesic.Aspect.Ratio", "Ratio.Red.Green", "Sigma.Intensity", "Volume")]
    data.function <- aggregate(. ~ Size.Class, data.function, mean)
    data.function$Species <- as.factor(as.numeric(data.function$Size.Class))
    data.function <- data.function[, -1]
    fun.div <- dbFD(data.function)
    
    # Store the result in the data frame
    results_df <- rbind(results_df, data.frame(Carbon_Dioxide = co2, Temperature = temp, Functional_Diversity = fun.div))
  }
}

# Print the results data frame
print(results_df)

# Functional Diversity Plots
fdiv <- ggplot(results_df, aes(x = Temperature, y = Functional_Diversity.FDiv, color = Carbon_Dioxide, fill = Carbon_Dioxide)) +
  geom_point() +
  stat_smooth(method = "glm", se = F) +
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  scale_fill_manual(values = c("#7851A9", "#a97851")) +
  theme_cowplot() +
  ylab("Functional Divergence") +
  xlab("Temperature") + 
  stat_cor(method = "pearson", size = 4)+ 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_blank()) +
  labs(color= expression(paste(C0[2], "Treatment"))) +
  theme(legend.position="none")

feve <- ggplot(results_df, aes(x = Temperature, y = Functional_Diversity.FEve, color = Carbon_Dioxide, fill = Carbon_Dioxide)) +
  geom_point() +
  stat_smooth(method = "glm", se = F) +
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  scale_fill_manual(values = c("#7851A9", "#a97851")) +
  theme_cowplot() +
  ylab("Functional Evenness") +
  xlab("Temperature") + 
  stat_cor(method = "pearson", size = 4)+ 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_blank()) +
  labs(color= expression(paste(C0[2], "Treatment"))) +
  theme(legend.position="none")

fric <- ggplot(results_df, aes(x = Temperature, y = 100*Functional_Diversity.FRic, color = Carbon_Dioxide, fill = Carbon_Dioxide)) +
  geom_point() +
  stat_smooth(method = "glm", se = F) +
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  scale_fill_manual(values = c("#7851A9", "#a97851")) +
  theme_cowplot() +
  ylab("Functional Richness") +
  xlab("Temperature") + 
  stat_cor(method = "pearson", size = 4)+ 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_blank()) +
  labs(color= expression(paste(C0[2], "Treatment"))) +
  theme(legend.position="none")

fric1 <- ggplot(results_df, aes(x = Temperature, y = 100*Functional_Diversity.FRic, color = Carbon_Dioxide, fill = Carbon_Dioxide)) +
  geom_point() +
  stat_smooth(method = "glm", se = F) +
  scale_colour_manual(values = c("#7851A9", "#a97851")) +
  scale_fill_manual(values = c("#7851A9", "#a97851")) +
  theme_cowplot() +
  ylab("Functional Richness") +
  xlab("Temperature") + 
  stat_cor(method = "pearson", size = 4)+ 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_blank()) +
  labs(color= expression(paste(CO[2], "Treatment")))

legend <- get_legend(
  fric1 + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom", legend.title = element_text(size = 12), legend.text = element_text(size = 12)))

Figure_Tmp <- plot_grid(fric, feve, fdiv, nrow = 1, labels = c("A", "B", "C"), label_size = 14)
Figure6 <- plot_grid(Figure_Tmp, legend, ncol = 1, rel_heights  = c(1, .1), align = "v", axis = "lr") 

ggsave(filename= "figures/manuscript/Figure6.pdf", 
       plot = Figure6, width = 180, height = 100, units=c("mm"), dpi=600) 

# Structural Equation Modeling
# Note: You must now switch over and run 02_Amplicon_Analysis.R before proceeding

data.clustered.mean <- aggregate(. ~Size.Class + Temperature + Carbon.Dioxide, data.clustered, mean)
data.clustered.mean <- data.clustered.mean[,c("Size.Class", "Temperature", "Carbon.Dioxide", "Geodesic.Aspect.Ratio", "Ratio.Red.Green",
                                              "Sigma.Intensity", "Volume")]
data.summed.mean <- aggregate(. ~Plot + Size.Class, data.clustered, sum)
data.summed.mean <- data.summed.mean[,c("adj.count", "adj.mass")]
data.structural <- cbind(data.clustered.mean, data.summed.mean)
data.structural$biomass <- data.structural$adj.count * data.structural$adj.mass

#Keep alpha/shannon diversity metrics
keep <- c(2, 4, 6, 7, 9, 11, 13, 15, 17, 19)
community <- alpha18S[keep]

community<- community %>%
  mutate(co2 = ifelse(co2 == "eCO2", "Elevated", "Ambient"))

community <- community[,c("Observed", "Shannon", "temp", "co2")]
colnames(community) <- c("Observed", "Shannon", "Temperature", "Carbon.Dioxide")

data.biomass <- aggregate(biomass ~ Temperature + Carbon.Dioxide, data.structural, sum)

sem.data <- data.structural %>%
  left_join(community %>%
              select(Temperature, Carbon.Dioxide, Observed, Shannon),
            by = c("Temperature", "Carbon.Dioxide"))

# Print the first few rows of the standardized data
head(sem.data)

sem.data$adj.count <- log(sem.data$adj.count)

# Structural Equation Modeling

# Specify the SEM model
model <- '
  # Latent variables
  CommunityStructure =~ Observed + Shannon
  Trait =~ Geodesic.Aspect.Ratio + Ratio.Red.Green + Sigma.Intensity + Volume
  Abundance =~ adj.count

  # Hypothesized relationships
  CommunityStructure ~ Temperature * Carbon.Dioxide + Abundance
  Trait ~ Temperature * Carbon.Dioxide
  Abundance ~ Temperature * Carbon.Dioxide
'

# Fit the SEM model
fit <- sem(model, data = sem.data)

# Summary of the SEM model
summary(fit)

### Get rid of Latent Variables as explanatory within the Figure 
### r display SEM fit

# Create a path diagram
# Define labels with full terms
labels <- c(
  Obs = "Observed",
  Shn = "Shannon",
  G.A = "Geodesic.Aspect.Ratio",
  R.R = "Ratio.Red.Green",
  S.I = "Sigma.Intensity",
  Vlm = "Volume",
  ad. = "adj.count",
  Trt = "Trait",
  Abn = "Abundance",
  CmS = "CommunityStructure",
  C.D = "Carbon.Dioxide",
  Trt = "Temperature"
)

# Create a path diagram with full labels
path_diagram <- semPaths(fit, layout = "tree2", whatLabels = "std")#, nodeLabels = labels)

# Updated SEM
# Assuming 'sem.data' is your datase

# Fit the SEM model
fit <- sem(model, data = sem.data)

# SEM model fit indices to include in the plot
fitMeasures <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea")

# Create a path diagram
pathDiagram <- semPaths(fit, whatLabels = "est",
                        layout = "tree",
                        edge.color = c("black", "red"),
                        edge.label.cex = 0.8,
                        nodeLabels = c("Observed" = "Observed",
                                       "Shannon" = "Shannon Diversity Index",
                                       "Gedsc.Aspct.Rt" = "Geodesic Aspect Ratio",
                                       "Ratio.Red.Gren" = "Red-Green Ratio",
                                       "Sigma.Intensty" = "Sigma Intensity",
                                       "Volume" = "Volume",
                                       "adj.count" = "Adjusted Count",
                                       "CommunityStructure" = "Community Structure",
                                       "Trait" = "Trait",
                                       "Abundance" = "Abundance",
                                       "CO2*Temperature" = "CO2 x Temperature",
                                       "Carbon.Dioxide" = ),
                        residuals = FALSE,
                        sizeMan = 10,
                        sizeLat = 12,
                        nCharNodes = 0, # To allow full names
                        mar = c(5,5,5,5))

save_image(pathDiagram, file="figures/manuscript/SEM_Path_Diagram.png")

# Continued SEM Analysis
sem.data <- data.clustered

sem.data <- left_join(sem.data, community)
sem.data <-  left_join(sem.data, data.biomass)

for(i in 1:nrow(sem.data)){
  if(sem.data$Carbon.Dioxide[i] == "Ambient"){
    sem.data$Carbon.Dioxide[i] <- 450
  }
  else {
    sem.data$Carbon.Dioxide[i] <- 900
  }
}

sem.data$Carbon.Dioxide <- as.numeric(sem.data$Carbon.Dioxide)

# SEM model fit indices to include in the plot
fitMeasures <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea")

# Create a path diagram
# Assuming 'fit' is your fitted lavaan model object
pathDiagram <- semPaths(fit, whatLabels = "std", 
                        layout = "spring")

# Plotting SEM for Manuscrpit
variables_to_standardize <- c(
  "Geodesic.Aspect.Ratio",
  "Ratio.Red.Green",
  "Sigma.Intensity",
  "Volume",
  "biomass")

# Standardize the selected variables
sem.data <- sem.data %>%
  mutate(across(all_of(variables_to_standardize), ~scale(.)))

sem.aggregated <- aggregate(. ~Temperature + Carbon.Dioxide, sem.data, mean)

colnames(sem.aggregated) <- c("Temperature", "Carbon.Dioxide", "Plot", "Sample", "Geodesic.Aspect.Ratio", "Geodesic.Length", "Ratio.Red.Green", 
                              "Sigma.Intensity", "Volume", "adj.count", "adj.mass", "Size.Class", "Observed", "Shannon", "biomass")

model <- '
  # Latent variables
  CompositionalDiversity =~ Observed + Shannon
  FunctionalDiversity =~ Geodesic.Aspect.Ratio + Ratio.Red.Green + Sigma.Intensity + Volume

  # Regressions
  CompositionalDiversity ~ Temperature + Temperature:Carbon.Dioxide + Carbon.Dioxide
  FunctionalDiversity ~ Temperature + Temperature:Carbon.Dioxide + Carbon.Dioxide
    
  # Covariances
  FunctionalDiversity ~~ CompositionalDiversity
'

model2 <- '
  # Regressions
  Observed + Shannon ~ Temperature + Temperature:Carbon.Dioxide + Carbon.Dioxide
  biomass ~ Temperature + Temperature:Carbon.Dioxide + Carbon.Dioxide
  biomass ~ Observed + Shannon
  Volume + Geodesic.Aspect.Ratio + Sigma.Intensity  + Ratio.Red.Green ~ Temperature + Temperature:Carbon.Dioxide + Carbon.Dioxide
  Volume + Geodesic.Aspect.Ratio + Sigma.Intensity  + Ratio.Red.Green ~ Observed + Shannon + biomass
'

model_pruned <- '
  # Regressions
  Observed ~ Temperature + Temperature:Carbon.Dioxide + Carbon.Dioxide
  Shannon ~ Temperature + Temperature:Carbon.Dioxide + Carbon.Dioxide
  Volume ~ Temperature + Temperature:Carbon.Dioxide + Carbon.Dioxide
  Geodesic.Aspect.Ratio ~ Temperature + Temperature:Carbon.Dioxide + Carbon.Dioxide
  Ratio.Red.Green ~ Temperature + Temperature:Carbon.Dioxide + Carbon.Dioxide
  Volume ~ Observed + Shannon
  Geodesic.Aspect.Ratio ~ Observed + Shannon
  Sigma.Intensity ~ Shannon
  Ratio.Red.Green ~ Observed + Shannon
'

revised_model <- '
  # Regressions
  Observed ~ Temperature + Temperature:Carbon.Dioxide + Carbon.Dioxide
  Shannon ~ Temperature + Temperature:Carbon.Dioxide + Carbon.Dioxide
  Volume ~ Temperature + Carbon.Dioxide
  Ratio.Red.Green ~ Temperature + Carbon.Dioxide
  Geodesic.Aspect.Ratio ~ Temperature + Temperature:Carbon.Dioxide + Carbon.Dioxide

  # Covariances
  Shannon ~~ Observed
  Ratio.Red.Green ~~ Volume
'

# Fit the SEM model
fit <- lavaan::sem(revised_model, data = sem.aggregated)

# Get a summary with fit indices
summary(fit)

# Check modification indices for potential improvements
modIndices <- modificationIndices(fit)
head(modIndices, 10)  # Display the top 10 suggestions

# Analyze residuals
residuals <- resid(fit)

# Explore latent variable structure with Exploratory Factor Analysis
efa_results <- factanal(sem.aggregated[, c("Observed", "Shannon", "Geodesic.Aspect.Ratio", "Ratio.Red.Green", "Sigma.Intensity", "Volume")], factors = 2)
print(efa_results)

# Define custom color function
edgeColor <- function(fit, cutoff = 0.05) {
  est <- parameterEstimates(fit, standardized = TRUE)
  # Initialize the color vector with a default value to avoid NA
  color <- rep("grey", length(est$est)) # Default color
  # Assign colors based on p-values and the sign of the estimates
  color[est$pvalue < cutoff & est$est > 0] <- "#002244"
  color[est$pvalue < cutoff & est$est < 0] <- "#FB4F14"
  color[is.na(est$pvalue) & est$est < 0] <- "#FB4F14"
  color[is.na(est$pvalue) & est$est > 0] <- "#002244"
  names(color) <- est$op
  return(color)
}

edgeLty <- function(fit, cutoff = 0.05) {
  est <- parameterEstimates(fit, standardized = TRUE)
  # Initialize the lty vector with a default value to avoid NA
  lty <- rep(2, length(est$pvalue)) # Default line type (dashed)
  # Assign line types based on p-values
  lty[est$pvalue < cutoff] <- 1 # Solid line for significant estimates
  lty[is.na(est$pvalue) == T] <- 1
  lty <- as.character(lty)
  #names(lty) <- est$op
  return(lty)
}


# Define custom edge width function based on the standardized estimate
edgeWidth <- function(fit) {
  est <- parameterEstimates(fit, standardized = TRUE)
  width <- rep(1, nrow(est))  # Default width is 1
  est$pvalue[is.na(est$pvalue)] <- 1
  width[est$pvalue < 0.05] <- 1 + abs(est$std.all) * 10 # Increase width for significant paths
  names(width) <- est$op
  return(width)
}

# First, call the custom functions to get the styles
colors <- edgeColor(fit)
ltys <- edgeLty(fit)
widths <- edgeWidth(fit)
widths[is.na(widths)] <- 1

node_colors <- c("Observed" = "#FFB347", 
                 "Shannon" = "#FFB347", 
                 #"biomass" = "#FFB347", 
                 "Volume" = "#B565A7", 
                 "Geodesic.Aspect.Ratio" = "#B565A7", 
                 #"Sigma.Intensity" = "#B565A7",
                 "Ratio.Red.Green" = "#B565A7", 
                 "Temperature" = "#6497B1", 
                 "Temperature:Carbon.Dioxide" = "#6497B1",
                 "Carbon.Dioxide" = "#6497B1")

# Now create the path diagram with custom aesthetics
pathDiagram <- semPaths(fit, what = "par", whatLabels = "hide", 
                        layout = "tree2", 
                        edge.color = colors,
                        edge.width = widths,
                        lty = ltys,
                        color = list(lat = node_colors, man = node_colors),
                        nodeLabels = c("Observed" = "Total \nRichness",
                                       "Shannon" = "Shannon \nDiversity",
                                       #"biomass" = "Biomass",
                                       "Volume" = "Cell \nVolume",
                                       "Geodesic.Aspect.Ratio" = "Cell \nShape",
                                       #"Sigma.Intensity" = "Cell \nContents",
                                       "Ratio.Red.Green" = "Cell \nColor",
                                       "Temperature" = "Temperature",
                                       "Tmprtr:Crbn.Dx" =  "Interaction",
                                       "Carbon.Dioxide" = "Carbon \nDioxide"),
                        nCharNodes = 0, # To allow full names
                        sizeMan = 8, # Manageable node size
                        sizeLat = 15, # Latent variable node size
                        mar = c(10,4,10,4),
                        intercepts = FALSE,
                        residuals = FALSE,
                        optimizeLatRes = TRUE,
                        exoCov = T,
                        weighted = F,
                        cardinal = F,
                        curve = 1.5)

# Get the parameter estimates
estimates <- parameterEstimates(fit)

# You can also specify particular fit indices you are interested in
cfi <- fitMeasures(fit, "cfi")
tli <- fitMeasures(fit, "tli")
rmsea <- fitMeasures(fit, "rmsea")
srmr <- fitMeasures(fit, "srmr")
chi <-  fitMeasures(fit, "chisq")
fitMeasures(fit)

# Print specific fit indices
print(cfi)
print(tli)
print(rmsea)
print(srmr)

# Select the columns you want to include in the table
estimates_table <- estimates[, c("lhs", "op", "rhs", "est", "se", "z", "pvalue")]

# Use stargazer to create a LaTeX table
stargazer(estimates_table, summary = FALSE, type = "latex",
          title = "SEM Parameter Estimates",
          label = "tab:semEstimates",
          table.placement = "H",
          digits = 3,
          float = FALSE)

# sem with top lines
# Extract parameter estimates
param_estimates <- parameterEstimates(fit)

# Sort by p-value and select the top 10
top_estimates <- param_estimates[order(abs(param_estimates$est)), ][1:18,]
top_estimates$pvalue[is.na(top_estimates$pvalue)] <- 0

# Get the names of the edges corresponding to the top parameter estimates
top_edges_names <- rownames(top_estimates)

# Modify the edge color function to color only the top 10 edges
edgeColor <- function(fit, cutoff = 0.05, top_edges) {
  est <- parameterEstimates(fit, standardized = TRUE)
  est$pvalue[is.na(est$pvalue)] <- 0
  color <- rep("transparent", length(est$est), alpha = 0) # Set default color to grey
  # Loop over the parameter estimates to assign colors
  for (i in 1:nrow(est)) {
    if (rownames(est)[i] %in% top_edges) {
      if (est$pvalue[i] < cutoff && est$est[i] > 0) {
        color[i] <- "#002244" # Blue for positive significant estimates
      } else if (est$pvalue[i] < cutoff && est$est[i] < 0) {
        color[i] <- "#FB4F14" # Orange for negative significant estimates
      }
    }
  }
  names(color) <- rownames(est)
  return(color)
}

# Modify the edge line type function for top 10 edges
edgeLty <- function(fit, cutoff = 0.05, top_edges) {
  est <- parameterEstimates(fit, standardized = TRUE)
  est$pvalue[is.na(est$pvalue)] <- 0
  lty <- rep(2, length(est$est)) # Default line type (dashed)
  for (i in 1:nrow(est)) {
    if (rownames(est)[i] %in% top_edges && est$pvalue[i] < cutoff) {
      lty[i] <- 1 # Solid line for top significant estimates
    }
  }
  names(lty) <- rownames(est)
  return(lty)
}

# Modify the edge width function for top 10 edges
edgeWidth <- function(fit, top_edges) {
  est <- parameterEstimates(fit, standardized = TRUE)
  est$pvalue[is.na(est$pvalue)] <- 0
  width <- rep(1, length(est$est)) # Default width
  for (i in 1:nrow(est)) {
    if (rownames(est)[i] %in% top_edges) {
      # Increase width for top significant paths
      width[i] <- 1 + abs(est$std.all[i]) * 10
    }
  }
  names(width) <- rownames(est)
  return(width)
}

# Apply the functions to get the styles for edges
colors <- edgeColor(fit, cutoff = 0.05, top_edges = top_edges_names)
ltys <- edgeLty(fit, cutoff = 0.05, top_edges = top_edges_names)
widths <- edgeWidth(fit, top_edges = top_edges_names)

node_colors <- c("Observed" = "#FFB347", 
                 "Shannon" = "#FFB347", 
                 #"biomass" = "#FFB347", 
                 "Volume" = "#B565A7", 
                 "Geodesic.Aspect.Ratio" = "#B565A7", 
                 #"Sigma.Intensity" = "#B565A7",
                 "Ratio.Red.Green" = "#B565A7", 
                 "Temperature" = "#6497B1", 
                 "Temperature:Carbon.Dioxide" = "#6497B1",
                 "Carbon.Dioxide" = "#6497B1")

# Now create the path diagram with custom aesthetics
pdf("figures/SEM/path_diagram.pdf")
pathDiagram <- semPaths(fit, what = "par", whatLabels = "hide", 
                        layout = "tree2", 
                        edge.color = colors,
                        edge.width = widths,
                        lty = ltys,
                        color = list(lat = node_colors, man = node_colors),
                        nodeLabels = c("Observed" = "Total \nRichness",
                                       "Shannon" = "Shannon \nDiversity",
                                       #"biomass" = "Biomass",
                                       "Volume" = "Cell \nVolume",
                                       "Geodesic.Aspect.Ratio" = "Cell \nShape",
                                       #"Sigma.Intensity" = "Cell \nContents",
                                       "Ratio.Red.Green" = "Cell \nColor",
                                       "Temperature" = "Temperature",
                                       "Tmprtr:Crbn.Dx" =  "Interaction",
                                       "Carbon.Dioxide" = "Carbon \nDioxide"),
                        nCharNodes = 0, # To allow full names
                        sizeMan = 8, # Manageable node size
                        sizeLat = 15, # Latent variable node size
                        mar = c(10,4,10,4),
                        intercepts = FALSE,
                        residuals = FALSE,
                        optimizeLatRes = TRUE,
                        exoCov = T,
                        weighted = F,
                        cardinal = F,
                        curve = 1.5)