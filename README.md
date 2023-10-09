# boostPM_experiments

This repository provides program code to run the boosting algorithm proposed in Awaya and Ma (2021) under the settings of the numerical experiments. 

The detailed information is provided in

Awaya, N., & Ma, L. (2021). Unsupervised tree boosting for learning probability distributions. arXiv preprint arXiv:2101.11083.

To run the program, the R package `boostPM` needs to be installed  using `devtools` as follows:

```
library(devtools)
install_github("nawaya040/boostPM")
```

The following R files are included:

1. `Density_estimation.R`: density estimation for multi-variate data sets (Section 3.1 and 3.2)
2. `Variable.R`: evaluation of the variable importance (Section 3.3)
3. `Simulation.R`: functions to simulate data sets

For the benchmark data sets used in Section 3.2, they are pre-processed following these two papers:

POWER, GAS, HEPMASS, MINIBOONE \
Paper: Papamakarios, G., Pavlakou, T., & Murray, I. (2017). Masked autoregressive flow for density estimation. Advances in neural information processing systems, 30. \
Repository: https://github.com/gpapamak/maf

AReM, CASP, BANK \
Paper: Liu, Q., Xu, J., Jiang, R., & Wong, W. H. (2021). Density estimation using deep generative neural networks. Proceedings of the National Academy of Sciences, 118(15), e2101344118. \
Repository: https://zenodo.org/record/4559067
