---
editor_options: 
  markdown: 
    wrap: 72
---

# Temperature and CO₂ Interactively Drive Shifts in Peatland Protist Communities

[![DOI](https://img.shields.io/badge/DOI-10.1111%2Fgcb.17203-blue)](https://doi.org/10.1111/gcb.17203)
[![Journal](https://img.shields.io/badge/Journal-Global%20Change%20Biology-green)](https://onlinelibrary.wiley.com/journal/gcb)
[![R](https://img.shields.io/badge/R-%3E%3D4.0.0-blue)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Data](https://img.shields.io/badge/Data-Available-brightgreen)](https://doi.org/10.5061/dryad.kprr4xhbx)

## 📖 About

This repository contains the complete analytical pipeline for
investigating how temperature and CO₂ interactively affect protist
communities in boreal peatlands. The study uses data from the SPRUCE
(Spruce and Peatland Responses Under Changing Environments) long-term
ecosystem warming experiment to demonstrate significant climate-induced
shifts in the compositional and functional structure of peatland protist
communities.

### 🔬 Key Findings

-   **Interactive Effects**: Temperature and CO₂ have significant
    interactive effects on protist community structure
-   **Size-Dependent Responses**: Environmental responses are contingent
    on organismal size, with larger protists showing amplified responses
-   **Functional Trait Reversals**: Warming effects on functional
    composition are reversed by elevated CO₂
-   **Taxonomic vs. Functional Divergence**: Communities converge
    taxonomically but diverge functionally under climate change

## 📊 Published Paper

**Kilner, C.L., Carrell, A.A., Wieczynski, D.J., et al.** (2024).
Temperature and CO₂ interactively drive shifts in the compositional and
functional structure of peatland protist communities. *Global Change
Biology*, 30, e17203. <https://doi.org/10.1111/gcb.17203>

## 🗂️ Repository Structure

```         
├── README.md                    # This file
├── LICENSE                      # MIT License
├── .gitignore                  # Git ignore rules
├── Protist-Traits.Rproj        # RStudio project file
│
├── 📁 scripts/                 # Main analysis scripts
│   ├── 00_setup.R              # Package installation and setup
│   ├── 01_Trait_Analysis.R     # Primary trait analysis pipeline
│   ├── 02_Amplicon_Analysis.R  # 18S rRNA amplicon sequencing analysis
│   └── 03_BootStrap_Plot_Code.R # Bootstrap visualization code
│
├── 📁 data/                    # Raw and processed data
│   ├── raw/                    # Original data files
│   │   ├── SPRUCE_2019.csv     # Main protist trait data (77.8 MB)
│   │   ├── SPRUCE_Density_2019.csv # Density calibration data
│   │   ├── mapping.txt         # Sample metadata
│   │   ├── protist-taxonomy.qza # QIIME2 taxonomy file
│   │   └── protist-dada2table.qza # QIIME2 feature table
│   └── processed/              # Processed/transformed data
│       └── processed.csv       # Cleaned trait data
│
├── 📁 results/                 # Model outputs and statistical results
│   ├── GLM_*.RDS              # Generalized Linear Model objects
│   ├── GAM_*.RDS              # Generalized Additive Model objects
│   └── cluster.RDS            # Size class clustering results
│
├── 📁 figures/                 # Generated plots and visualizations
│   ├── manuscript/            # Publication-ready figures
│   ├── LM/                    # Linear model diagnostic plots
│   ├── bootstrap/             # Bootstrap analysis plots
│   └── SEM/                   # Structural equation model diagrams
│
├── 📁 models/                  # Saved model objects
│   ├── BIC.RDS               # Bayesian Information Criterion results
│   └── cluster.RDS           # Clustering model results
│
└── 📁 docs/                   # Documentation and supplementary materials
    ├── manuscript.pdf         # Published GCB Manuscript
    └── supplement.pdf         # Published Supplementary Materials
```

## 🚀 Getting Started

### Prerequisites

-   **R** (≥ 4.0.0) - [Download here](https://www.r-project.org/)
-   **RStudio** (recommended) - [Download
    here](https://www.rstudio.com/)
-   **Git** - [Download here](https://git-scm.com/)

### Installation

1.  **Clone the repository**

    ``` bash
    git clone https://github.com/ClassicCK/Protist-Traits.git
    cd Protist-Traits
    ```

2.  **Open the RStudio project**

    ``` r
    # Open Protist-Traits.Rproj in RStudio
    ```

3.  **Install required packages**

    ``` r
    source("scripts/00_setup.R")
    ```

### 📦 Required R Packages

The analysis pipeline uses numerous R packages, automatically installed
by `00_setup.R`:

**Core Analysis:** - `tidyverse`, `dplyr`, `ggplot2` - Data manipulation
and visualization - `mclust` - Gaussian mixture modeling for size
classification - `lavaan`, `semPlot` - Structural equation modeling -
`vegan`, `phyloseq` - Community ecology analysis

**Statistical Modeling:** - `lme4`, `mgcv` - Mixed effects and GAM
modeling - `car`, `moments` - Statistical diagnostics and
transformations - `FD`, `fundiversity` - Functional diversity metrics

**Specialized:** - `qiime2R` - QIIME2 data import - `FlowCam` analysis
tools - Protist trait quantification - `microViz`, `microbiome` -
Microbial community analysis

## 🔬 Analysis Pipeline

### 1. Data Preprocessing (`01_Trait_Analysis.R`)

**Key Steps:** - Import and clean FlowCam protist trait data (37
morphological/optical traits) - Apply appropriate transformations for
non-normal distributions - Perform size-based clustering using Gaussian
mixture models - Calculate functional trait metrics (volume, aspect
ratio, cellular contents, resource acquisition)

**Size Classification:**

``` r
# Five size classes based on geodesic length
Size Class 1: 12.43-17.07 μm
Size Class 2: 17.07-20.68 μm  
Size Class 3: 20.68-28.24 μm
Size Class 4: 28.24-59.74 μm
Size Class 5: 59.74-526.43 μm
```

### 2. Statistical Analysis

**Generalized Linear Models (GLMs):** - Three-way interactions:
`Trait ~ Temperature × CO₂ × Size.Class` - Bootstrap validation with
1000 replicates - Functional trait responses across environmental
gradients

**Structural Equation Modeling (SEM):** - Direct and indirect
environmental effects - Community composition → functional trait
pathways - Model comparison and selection using fit indices

### 3. Amplicon Sequencing Analysis (`02_Amplicon_Analysis.R`)

**18S rRNA Gene Processing:** - QIIME2 pipeline for sequence quality
control - DADA2 denoising and feature detection\
- PR2 database taxonomic assignment - Phyloseq-based community
analysis - Alpha/beta diversity calculations

### 4. Functional Diversity Analysis

**Metrics Calculated:** - **Functional Richness (FRic)** - Volume of
trait space occupied - **Functional Evenness (FEve)** - Evenness of
trait distribution - **Functional Dispersion (FDiv)** - Mean distance to
centroid

## 📈 Key Results

### Temperature-Size Rule

-   **Ambient CO₂**: Protists get smaller, less round, more
    metabolically active with warming
-   **Elevated CO₂**: Complete reversal of temperature effects
-   **Size dependency**: Larger protists show amplified responses (up to
    25× stronger)

### Community Shifts

-   **80-fold variation** in protist densities across treatments
-   **Size class redistribution** under elevated CO₂
-   **Taxonomic convergence** but **functional divergence** between CO₂
    treatments

### Functional Traits Response

``` r
# Example results interpretation
Volume: Decreases with temperature (ambient CO₂) | Increases with temperature (elevated CO₂)
Shape: Less round with warming (ambient CO₂) | Rounder with warming (elevated CO₂)  
Contents: More active with warming (ambient CO₂) | Less active with warming (elevated CO₂)
Metabolism: More heterotrophic then autotrophic | Consistently more heterotrophic
```

## 📊 Figure Generation

All publication figures can be reproduced by running the analysis
scripts:

-   **Figure 1**: Experimental design and hypotheses
-   **Figure 2**: Abundance shifts across treatments\
-   **Figure 3**: Functional trait responses by size class
-   **Figure 4**: Taxonomic composition changes
-   **Figure 5**: Structural equation model results
-   **Figure 6**: Functional diversity metrics
-   **Supplementary Figures**: Correlation matrices, clustering,
    bootstrap results

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
For major changes, please open an issue first to discuss what you would
like to change.

### Development Workflow

1.  Fork the repository
2.  Create a feature branch (`git checkout -b feature/AmazingFeature`)
3.  Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4.  Push to the branch (`git push origin feature/AmazingFeature`)
5.  Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the
[LICENSE](LICENSE) file for details.

## 📚 Citation

``` bibtex
@article{kilner2024temperature,
  title={Temperature and CO₂ interactively drive shifts in the compositional and functional structure of peatland protist communities},
  author={Kilner, Christopher L and Carrell, Alyssa A and Wieczynski, Daniel J and Votzke, Samantha and DeWitt, Katrina and Yammine, Andrea and Shaw, Jonathan and Pelletier, Dale A and Weston, David J and Gibert, Jean P},
  journal={Global Change Biology},
  volume={30},
  number={3},
  pages={e17203},
  year={2024},
  publisher={Wiley Online Library},
  doi={10.1111/gcb.17203}
}
```

## 👥 Authors

-   **Christopher L. Kilner** - Lead author, trait analysis -
    [science\@christopher.eco](mailto:science@christopher.eco)
-   **Alyssa A. Carrell** - Amplicon sequencing analysis
-   **Daniel J. Wieczynski** - Statistical modeling\
-   **Jean P. Gibert** - Senior author, study design -
    [jean.gibert\@duke.edu](mailto:jean.gibert@duke.edu)

*Full author list and affiliations available in the published
manuscript.*

## 🔗 Related Resources

-   **SPRUCE Experiment**: <https://mnspruce.ornl.gov/>
-   **Data Repository**: <https://doi.org/10.5061/dryad.kprr4xhbx>
-   **FlowCam Documentation**: [Yokogawa Fluid
    Imaging](https://www.fluidimaging.com/)
-   **QIIME2**: <https://qiime2.org/>

## 🐛 Issues & Support

If you encounter any problems or have questions about the analysis:

1.  Check the
    [Issues](https://github.com/your-username/SPRUCE-Protist-Traits/issues)
    page
2.  Create a new issue with:
    -   Clear description of the problem
    -   Steps to reproduce
    -   Your R session info (`sessionInfo()`)
    -   Any error messages

## 🙏 Acknowledgments

-   SPRUCE experiment team at Oak Ridge National Laboratory
-   Duke University Department of Biology
-   Funding: DOE Office of Science, Simons Foundation, NSF

------------------------------------------------------------------------

> **Climate change impacts on peatland microbes matter!** Peatlands
> store 25% of terrestrial carbon despite covering \<3% of Earth's
> surface. Understanding microbial responses to warming and elevated CO₂
> is crucial for predicting ecosystem-scale carbon cycling under future
> climate scenarios.
