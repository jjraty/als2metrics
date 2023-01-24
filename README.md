# als2metrics: An R package for the extraction of metrics from an airborne LiDAR point cloud

Eetu Kotivuori, Mikko Kukkonen, Janne Räty & Petteri Packalen, 2017

# Installation

Install the *als2metrics* package from GitHub using the R package *remotes*:

``` r
remotes::install_github("jjraty/als2metrics", ref = "main")
```

# An Example run and the descriptions of arguments

``` r
als2metrics(ALSFILE = "lidar_data.txt",            # A Path of an airborne LiDAR data file
            FIRST = TRUE,                          # Compute first echo metrics
            LAST = TRUE,                           # Compute last echo metrics
            INTERMEDIATE = TRUE,                   # Compute intermediate echo metrics
            ALL_ECHOES = TRUE,                     # Compute all echo metrics
            PROP_MEAN_ETYP = TRUE,                 # Compute proportions of echo categories
            BASIC_STATISTICS = TRUE,                        # Compute basic statistics
            PERCENTILE_SCALE = seq(0.05, 0.95, 0.05),       # Vector of percentiles
            DENSITIES = TRUE,                               # Compute densities
            DENSITIES_FIXED_TRESHOLD = c(0.5, 2, 5, 10, 15, 20),    #  Vector of heights in fixed height densities
            INTENSITY_STATISTICS = TRUE,                            # Compute intensity statistics
            INTENSITY_PERCENTILES = TRUE,                           # Compute intensity percentiles
            CUTOFF = 0.0,                             # Cutoff threshold
            MIN_ECHO_N = 10,                          # Minimum number of echoes
            output = "lidar_metrics.txt"              # Output file having ALS metrics
)
```

**ALSFILE:** Specify an airborne LiDAR data file (text file), Format: plot_cell_id; x; y; z; dz, i; echotype; flightline; terraclass; GPS-time (delimeter: space). First seven columns must be in the aforementioned order, additional columns are optional. String.

**FIRST:** Output for FIRST echoes (first + only). Logical.

**LAST:** Output for LAST echoes (last + only). Logical.

**INTERMEDIATE:** Output INTERMEDIATE echoes (intermediate). Logical.

**ALL_ECHOES:** Output ALL echoes (first, last, only and intermediate). Logical.

**PROP_MEAN_ETYP:** Calculates mean and standard deviation of heights and the proportion of echoes categories. Logical.

**BASIC_STATISTICS:** Calculates mean, std, med, min, max, skew, kurt. Logical.

**PERCENTILE_SCALE:** Set a vector of percentiles, e.g using seq(...) function. Percentiles are calculated using quantile() -function (using default type=7). Vector.

**DENSITIES:** Calculates densities, i.e. echo proportion under or equal to the determined height value. Logical.

**DENSITIES_FIXED_TRESHOLD:** The height values fixed height densities are computed. Vector.

**INTENSITY_STATISTICS:** Calculates mean_int, std_int, med_int, min_int, max_int, skew_int, kurt_int. Logical.

**INTENSITY_PERCENTILES:** Calculate perecentiles in the same manner as for dZ values ('Compute percentiles' must be set to TRUE). Logical.

**CUTOFF:** Cuts off all echoes smaller or equal to the given threshold value. Numeric.

**MIN_ECHO_N:** Minimum number of echoes to compute metrics. Numeric.

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
