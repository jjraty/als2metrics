# als2metrics: An R package for the extraction of metrics from an airborne LiDAR point cloud

# Installation

Install the *als2metrics* package from GitHub using the R package *remotes*:

``` r
remotes::install_github("jjraty/als2metrics", ref = "main")
```

# An Example run of the als2metrics function

``` r
als2metrics(pointcloud = "lidar_data.txt",            # A Path of an airborne LiDAR data file
            first = TRUE,                             # Compute first echo metrics
            last = TRUE,                              # Compute last echo metrics
            intermediate = TRUE,                      # Compute intermediate echo metrics
            all = TRUE,                               # Compute all echo metrics
            ecat_prop = TRUE,                         # Compute proportions of echo categories
            basic_stats = TRUE,                       # Compute basic statistics
            quantiles = seq(0.05, 0.95, 0.05),        # Vector of percentiles
            densities = TRUE,                         # Compute densities
            densities_ft = c(0.5, 2, 5, 10, 15, 20),  # Vector of heights in fixed height densities
            intensity_stats = TRUE,                   # Compute intensity statistics
            intensity_q = TRUE,                       # Compute intensity quantiles
            cutoff = 0.0,                             # Cutoff threshold
            min_echo_n = 10,                          # Minimum number of echoes
            output = "lidar_metrics.txt"              # Output file having ALS metrics
)
```

**pointcloud:** Specify an airborne LiDAR data file (text file), Format: plot_cell_id; x; y; z; dz, i; echotype; flightline; terraclass; GPS-time (delimeter: space). First seven columns must be in the aforementioned order, additional columns are optional. String.

**first:** Output for FIRST echoes (first + only). Logical.

**last:** Output for LAST echoes (last + only). Logical.

**intermediate:** Output INTERMEDIATE echoes (intermediate). Logical.

**all:** Output ALL echoes (first, last, only and intermediate). Logical.

**ecat_prop:** Calculates mean and standard deviation of heights and the proportion of echoes categories. Logical.

**basic_stats:** Calculates mean, std, med, min, max, skew, kurt. Skew and kurt computed using the functions of the moments package. Logical.

**quantiles:** Set a vector of percentiles, e.g using seq(...) function. Percentiles are calculated using quantile() -function (using default type=7). Vector.

**densities:** Calculates densities, i.e. echo proportion under or equal to the determined height value. Logical.

**densities_ft:** The height values fixed height densities are computed. Vector.

**intensity_stats:** Calculates mean_int, std_int, med_int, min_int, max_int, skew_int, kurt_int. Logical.

**intensity_q:** Calculate quantiles in the same manner as for dZ values (quantiles must be defined). Logical.

**cutoff:** Cuts off all echoes smaller or equal to the given threshold value. Numeric.

**min_ehcho_n:** Minimum number of echoes to compute metrics. Numeric.

**output:** Output path with a file name. Output file format is .txt. String.

# Description of an output file

There is one output file (.txt). The number of columns depends on your settings.

## Prefixes used in the output file:

f = first + only echoes

l = last + only echoes

a = first + last + only + intermediate echoes

i = intermediate echoes

int = intensity

## Output column names:

| Abbreviation         | Description                          |
|----------------------|--------------------------------------|
| plot_cell_id         | Plot or cell id                      |
| hmax                 | Maximum                              |
| hmin                 | Minimum                              |
| hstd                 | Standard deviation                   |
| hmed                 | Median                               |
| hmean                | Mean                                 |
| abs_n                | Absolute number of echoes            |
| hskew                | Skewness                             |
| hkurt                | Kurtosis                             |
| intmax               | Maximum intensity                    |
| intmin               | Minimum intensity                    |
| intstd               | Standard deviation of intensity      |
| intmed               | Median of intensity                  |
| intmean              | Mean of intensity                    |
| intskew              | Skewness of intensity                |
| intkurt              | Kurtosis of intensity                |
| h5/h10/.../h95       | Height percentiles e.g. H5,H10...H95 |
| int5/int10/.../int95 | Intensity percentiles                |
| d0.5/d2/d5/.../d20   | Height densities                     |
| echo_prob            | proportions of echo categories       |


# An Example run of the ai2metrics function

``` r
ai2metrics(
  pointcloud = "lidar_ai_data.txt",
  cutoff = 0,
  min_echo_n = 10,
  output = "ai_metrics.txt",
  verbose = TRUE
)
```
## Output column names:

| Abbreviation         | Description                          |
|----------------------|--------------------------------------|
| plot_cell_id         | Plot or cell id                      |
| R/G/B/Nmax           | Maximum                              |
| R/G/B/Nmin           | Minimum                              |
| R/G/B/Nstd           | Standard deviation                   |
| X_Ymrat              | Ratio between means of X and Y bands |
| X_Ystdrat            | Ratio between stds of X and Y bands  |



# Authors
Original als2metrics and ai2metrics functions: Eetu Kotivuori, Mikko Kukkonen, Janne Räty & Petteri Packalen, 2017.
Later modified and extended into the format of an R package: Janne Räty, 2023.