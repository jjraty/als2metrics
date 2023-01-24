# als2metrics: An R package for the computation of metrics from airborne LiDAR data

Kotivuori, Kukkonen,  Räty & Packalen, 2017

# Installation
Install the *als2metrics* from GitHub github using the R package *remotes*:

```r
remotes::install_github("jjraty/als2metrics", ref = "main")
```

# An Example run and the descriptions of arguments
```r
als2metrics( "test_plot_data.txt",     # ALS data file
             T,                        # Compute first echo metrics
             T,                        # Compute last echo metrics
             T,                        # Compute intermediate echo metrics
             T,                        # Compute all echo metrics
             T,                        # Compute proportions of echo categories
             T,                        # Compute basic statistics
             seq(0.05,0.95,0.05),      # Vector of percentiles
             T,                        # Compute densities
             c(0.5,2,5,10,15,20),      # Vector of heights in fixed height densities
             T,                        # Compute intensity statistics
             T,                        # Compute intensity percentiles
             0.0,                      # Cutoff threshold
             10,                       # Minimum number of echoes
             "als_metrics.txt"         # Output file having ALS metrics
)
```

*ALS data file*: Specify an ALS file (text file), Format: plot_cell_id; x; y; z; dz, i; echotype; flightline; terraclass; GPS-time   (delimeter: space). First seven columns must be in the abovementioned order, additional columns are optional.

*Compute first echo metrics*: Output for FIRST echoes (first + only)                         

*Compute last echo metrics*: Output for LAST echoes  (last + only)

*Compute intermediate echo metrics*: Output INTERMEDIATE echoes (intermediate)

*Compute all echo metrics*: Output ALL echoes (first, last, only and intermediate)

*Compute proportions of echo categories*: Calculates mean and standard deviation of heights and the proportion of echoes categories

*Compute basic statistics*: Calculates mean, std, med, min, max, skew, kurt

*Vector of percentiles*: Set a vector of percentiles, e.g using seq(...) function. Percentiles are calculated using quantile() -function (using default type=7)

*Compute densities*: Calculates densities, i.e. echo proportion under or equal to the determined height value

*Vector of heights in fixed height densities*: The height values fixed height densities are computed 

*Compute intensity statistics*: Calculates mean_int, std_int, med_int, min_int, max_int, skew_int, kurt_int 

*Compute intensity percentiles*: Calculate perecentiles in the same manner as for dZ values ('Compute percentiles' must be set to T)

*Cutoff threshold*: Cuts off all echoes smaller or equal to the given threshold value.

*Minimum number of echoes*: Minimum number of echoes to compute metrics.


# Description of output

There is one output file (.txt). The number of columns depends on your settings.

## Prefixes:

f = first + only
l = last + only
a = first + last + only + intermediate
i = intermediate

int = intensity                                                                       

## Output column names:

plot_cell_id = Plot or cell id

hmax = Maximum 
hmin = Minimum 
hstd = Standard deviation 
hmed = Median 
hmean = Mean
abs_n = Absolute number of echoes 	
hskew = Skewness 
hkurt = Kurtosis

intmax = Maximum intensity
intmin = Minimum intensity
intstd = Standard deviation of intensity
intmed = Median of intensity
intmean = Mean of intensity
intskew = Skewness of intensity
intkurt =	Kurtosis of intensity
	
h5/h10/.../h95 = Height percentiles e.g. H5,H10...H95       
int5/int10/.../int95 = Intensity percentiles                      
d0.5/d2/d5/.../d20 = Height densities                           

echo_prob = proportions of echo categories

                                                                                           

             
