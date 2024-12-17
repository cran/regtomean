# Package 'regtomean' - Regression Toward The Mean

**Type**: Package  
**Title**: Regression Toward the Mean  
**Version**: 1.2    
**License**: MIT + file LICENSE  
**Encoding**: UTF-8  
**LazyData**: true  
**RoxygenNote**: 7.1.1  
**Depends**: R (>= 3.4.0)  
**NeedsCompilation**: no  
**Repository**: CRAN  
**Date/Publication**: 2022-10-26 13:52:37 UTC  

## Description

In repeated measures studies with extreme large or small values, it is common for the subjects' measurements on average to be closer to the mean of the basic population. Interpreting possible changes in the mean in such situations can lead to biased results since the values were not randomly selected, they come from truncated sampling. This method allows estimating the range of means where treatment effects are likely to occur when regression toward the mean is present.

Ostermann, T., Willich, Stefan N. & Luedtke, Rainer. (2008). Regression toward the mean - a detection method for unknown population mean based on Mee and Chua's algorithm. BMC Medical Research Methodology. 

## Authors

- Daniela Recchia [aut, cre]
- Thomas Ostermann [ctb]
- Julian Stein [ctb]

## Maintainer

Daniela Recchia <daniela.rodriguesrecchia@uni-wh.de>

## Acknowledgments
We would like to acknowledge Lena Roth and Nico Steckhan for the package's initial updates (Q3 2024) and continued supervision and guidance. Both have contributed to discussing and integrating these methods into the package, ensuring they are up-to-date and contextually relevant.

## Installation

To install this package, use:

```R
install.packages("regtomean")
```

## Dependencies

- formattable
- effsize
- mefa
- plyr
- plotrix
- sjPlot
- sjmisc
- sjlabelled
- ggplot2
- dplyr

# Table of Contents

0. [language_test](#language_test)
1. [cordata](#cordata)
2. [replicate_data](#replicate_data)
3. [meechua_reg](#meechua_reg)
4. [meechua_eff.CI](#meechua_effci)
5. [plot_mu](#plot_mu)
6. [plot_t](#plot_t)
7. [meechua_plot](#meechua_plot)






## language_test

### Description:

A dataset with scores from 8 students who failed a high school test and could not get their diploma. They repeated the exam and got new scores.

### Usage:

```R
data("language_test")
```

### Format:

A data frame with 8 observations on the following 9 variables:

- `Student`: a numeric vector
- `Before`: a numeric vector
- `After`: a numeric vector
- `Total N`: a numeric vector
- `Cross`: a numeric vector
- `Pre-treatment Mean`: a numeric vector
- `Pre-treatment Std`: a numeric vector
- `Post-treatment Mean`: a numeric vector
- `Post-treatment Std`: a numeric vector

### Source:

McClave, J.T; Dietrich, F.H.: "Statistics"; New York, Dellen Publishing; 1988.


## cordata

### Description:

This function calculates the correlation for the data and Cohen’s d effect sizes, both based on pooled and on treatment standard deviations.

### Usage:

```R
cordata(Before, After, data)
```

### Arguments:

- `Before`: a numeric vector giving the data values for the first (before) measure.
- `After`: a numeric vector giving the data values for the second (after) measure.
- `data`: an optional data frame containing the variables in the formula. By default, the variables are taken from the environment (formula).

### Details:

This function computes the correlation between both measures as also both effect sizes based on Cohen’s d statistic.

The inputs must be numeric.

### Value:

Returns a table containing the correlation, effect size pooled, and effect size based on treatment.

### Author(s):

Daniela Recchia, Thomas Ostermann.

### References:

Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). New York: Academic Press.

### See Also:

`cohen.d`, `cor`

### Examples:

```R
cordata("Before", "After", data=language_test)
```










## replicate_data

### Description

This function replicates 100 times the before and after values giving a start and end reference.

### Usage

```r
replicate_data(start, end, Before, After, data)
```

### Arguments

- `start`: a start value for µ.
- `end`: an end value for µ.
- `Before`: a numeric vector giving the data values for the first (before) measure.
- `After`: a numeric vector giving the data values for the second (after) measure.
- `data`: an optional data frame containing the before and after variables in the formula. By default, the variables are taken from the environment (formula).

### Details

In order to overcome the limitation of Mee and Chua’s test regarding the population mean µ, a replication of the data is performed.

After replicating the data, the unknown population mean µ is systematically estimated over a range of values. Further estimations will be based on this new dataset.

### Value

Returns a data frame we could call `mee_chua` containing the values for µ, before, and after.

### Author(s)

Daniela Recchia, Thomas Ostermann.

### References

Ostermann, T., Willich, Stefan N. & Luedtke, Rainer. (2008). Regression toward the mean - a detection method for unknown population mean based on Mee and Chua’s algorithm. BMC Medical Research Methodology.

Galton, F. (1886). Regression towards mediocrity in hereditary stature. Journal of the Anthropological Institute (15: 246-263).

### See Also

`rep`

### Examples

```R
replicate_data(0, 100, "Before", "After", data=language_test)
```










## meechua_reg

### Description

This function fits linear models for a subset of data frames.

### Usage

```R
meechua_reg(x)
```


### Arguments

- `x`: Data to be used in the regression.

### Details

The data used for the regression must be sorted by mu.

A set of linear models will be estimated and model coefficients are saved and stored in `mod_coef`.

The estimated standard error for the after measure is also stored in `se_after` to be used further in other functions.

### Value

A table containing the estimations for each mu. Global variables `models`, `mod_coef`, `se_after` are stored for further analysis. The models are saved in an object called `mee_chua`, which is not automatically printed but is saved in the environment.

### Author(s)

Daniela Recchia, Thomas Ostermann.

### References

Ostermann, T., Willich, Stefan N. & Luedtke, Rainer. (2008). Regression toward the mean - a detection method for unknown population mean based on Mee and Chua’s algorithm. BMC Medical Research Methodology.

### See Also

`lm`, `dlply`

### Examples

```R
## get the values ##
mee_chua <- replicate_data(0, 100, "Before", "After", data=language_test)
meechua_reg(mee_chua)
```










## meechua_eff.CI


### Description

This function calculates and plots treatment and regression effects of both before and after measures as also its p-values.

### Usage

```R
meechua_eff.CI(x, n, se_after)
```


### Arguments

- `x`: a data frame containing the results from `meechua_reg`. It is stored as `mod_coef`.
- `n`: the original sample size (number of observations) from data.
- `se_after`: the estimated standard error from `meechua_reg`. It is stored as `se_after`.

### Details

After performing the `meechua_reg`, the model coefficients `mod_coef` and the global variable `se_after` are used as input in this function to estimate treatment and regression effects.

### Value

Two plots are performed: the first "Treatment Effect and p-value" and the second "Confidence Intervals" for µ.

### Author(s)

Daniela Recchia, Thomas Ostermann

### References

Ostermann, T., Willich, Stefan N. & Luedtke, Rainer. (2008). Regression toward the mean - a detection method for unknown population mean based on Mee and Chua’s algorithm. BMC Medical Research Methodology.

### See Also

[meechua_reg](#meechua_reg)

### Examples

```r
# First perform replicate_data and meechua_reg
replicate_data(0, 100, "Before", "After", data=language_test)
meechua_reg(mee_chua)

# Model coefficients (mod_coef) and se_after are stored in the environment
# as a result from the function meechua_reg
meechua_eff.CI(mod_coef, 8, se_after)
```










## plot_mu

### Description

Based on the data before and after the intervention and the regression models of the function meechua_reg, this function plots for a given range of µ the t-statistics and p-values of one sided tests, wether the intervention is having an significant impact on the measurements accounting for regression to the mean.

### Interpretation

For each µ the t-statistic and p-value correspond to the one sided test, if the intercept of the regression model from `meechua_reg` is significantly different from µ in the specified direction. Respecting the assumptions of the method, this is equivalent to the intervention having an significant impact accounting for regression to the mean. If for a concrete µ the p-value is below the specified threshold -visible as a blue dashed line- the impact of the intervention is significant under the assumption that µ is the real population mean.

### Usage

```R
plot_mu(x, n, se_after, lower = F, alpha = 0.05)
```

### Arguments

| Argument    | Description                                                                 |
|-------------|-----------------------------------------------------------------------------|
| `x`         | A data frame containing the results from `meechua_reg`. It is stored as `mod_coef`. |
| `n`         | The original sample size (number of observations) of the data.                |
| `se_after`  | The estimated standard error from `meechua_reg`. It is stored as `se_after`.|
| `lower` | Boolean value specifying the direction of the one sided tests. For `lower = F` (the default) it is testing, wether the intervention is increasing the measurements, for `lower = T`, wether the second measurements are lower than expected. |
| `alpha` | Specifies the significance threshold for the p-values of corresponding one sided tests. The default is `alpha = 0.05`. |

### Output

Plot for a range of µ the p-values and t-values of the corresponding tests against µ and prints some relevant values:

The value of µ, for which the treatment effect is the most statistically significant, and the corresponding t-statistic and p-value.
The highest and lowest µ, for which the treatment impact is significant.

Those variables will be returned as a list as well.

### Author(s)

Julian Stein

### References

Ostermann, T., Willich, Stefan N. & Luedtke, Rainer. (2008). Regression toward the mean - a detection method for unknown population mean based on Mee and Chua's algorithm. BMC Medical Research Methodology.

### See Also

[meechua_reg](#meechua_reg)

### Example

```R
# First perform replicate_data and meechua_reg
replicate_data(0, 100, "Before", "After", data=language_test)
meechua_reg(mee_chua)

#mod_coef and se_after are stored in the environment. The parameters lower = F and alpha = 0.05 can be omitted
plot_mu(mod_coef, 8, se_after)

#Alternative usage: Testing for decreased values due to the intervention with significance threshold alpha = 0.1
plot_mu(mod_coef, 8, se_after, lower=T, alpha = 0.1)
```









## plot_t

### Description

Similar to `plot_mu`, this function plots for a given range of µ the t-statistics and p-values of one sided tests, wether the intervention is having an significant impact on the measurements accounting for regression to the mean. The difference is, that this function is only based on some statistics of the samples before and after the treatment, like the mean, standard deviation and covariance/correlation.

### Interpretation

For each µ the t-statistic and p-value correspond to the one sided test, if the intervention has an significant impact on the second measurements accounting for regression to the mean. If for a concrete µ the p-value is below the specified threshold -visible as a blue dashed line- the impact of the intervention is significant under the assumption that µ is the real population mean.

### Usage

```R
plot_t(mu_start, mu_end, n, y1_mean, y2_mean, y1_std, y2_std, cov, lower = F, alpha = 0.05, r_insteadof_cov = F)
```

### Arguments

| Argument    | Description                                                                 |
|-------------|-----------------------------------------------------------------------------|
| `mu_start`         | Lower end for the range of µ to be considered. |
| `mu_end`         | Upper end for the range of µ to be considered. |
| `n`         | The number of observations.                |
| `y1_mean`  | Mean of the first measurement.|
| `y2_mean`  | Mean of the second measurement.|
| `y1_std`  | Standard deviation of the first measurement.|
| `y2_std`  | Standard deviation of the second measurement.|
| `cov`  | Covariance between the first and second measurements. If `r_insteadof_cov = T` this argument represents the correlation instead.|
| `lower` | Boolean value specifying the direction of the one sided tests. For `lower = F` (the default) it is testing, wether the intervention is increasing the measurements, for `lower = T`, wether the second measurements are lower than expected. |
| `alpha` | Specifies the significance threshold for the p-values of corresponding one sided tests. The default is `alpha = 0.05`. |
| `r_insteadof_cov` | Boolean value for the alternative usage of correlation instead of covariance. If `r_insteadof_cov = T`, the input `cov` is interpreted as the correlation.

### Output

Plot for a range of µ the p-values and t-values of the corresponding tests against µ and prints some relevant values:

The value of µ, for which the treatment effect is the most statistically significant, and the corresponding t-statistic and p-value.
The highest and lowest µ, for which the treatment impact is significant.

Those variables will be returned as a list as well.

### Author(s)

Julian Stein

### References

Ostermann, T., Willich, Stefan N. & Luedtke, Rainer. (2008). Regression toward the mean - a detection method for unknown population mean based on Mee and Chua's algorithm. BMC Medical Research Methodology.

### See Also

[plot_mu](#plot_mu)

### Example

```R
#Using the parameters corresponding to the example of the function plot_mu

plot_t(mu_start = 0, mu_end = 100, n = 8 , y1_mean = 57.375, y2_mean = 60.375, y1_std = 7.0, y2_std = 8.8, cov = 54.268)
```








## meechua_plot

### Description

This function plots all 4 diagnostics plots for each linear regression model: "Residuals vs Fitted", "Normal Q-Q", "Scale-Location" and "Residuals vs Leverage".

### Usage

```r
meechua_plot(x)
```

### Arguments

- `x`: List containing the estimated linear models from `meechua_reg`. It is stored as `models`.

### Details

For each model from `models`, 4 diagnostic plots are performed. For the first model, the numbers 1 to 4 should be given, for the second model numbers from 5 to 8, and so on.

### Value

Diagnostics plots for the set of models from `meechua_reg`.

### Author(s)

Daniela Recchia, Thomas Ostermann.

### References

Ostermann, T., Willich, Stefan N. & Luedtke, Rainer. (2008). Regression toward the mean - a detection method for unknown population mean based on Mee and Chua’s algorithm. BMC Medical Research Methodology.

### See Also

`plot.lm`, [meechua_reg](#meechua_reg)

### Examples

```R
# models are an output from meechua_reg
replicate_data(0, 100, "Before", "After", data=language_test)
meechua_reg(mee_chua)

# models are the output from meechua_reg saved in the environment after running the function
meechua_plot(models)
```