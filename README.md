# Multi Lineal Regression using PCA

__In Developing__

This project contain six script to do Multiple (Linear) Regression to predict the wheat production based on Spectral Reflectance data.

The first four script contain the code to manipulate the dataset and prepare the data to apply a Regression Algorithm. The fifth script show a Scatterplot Matrix of first three Principal Component. And the last one is the Regression Algorithm.

## Manipulate Dataset

The first script (1. FindMissingValues) review all DataSet to find missing values. You could use the function `findNA()` to do that. Also the variable `NA_values` contains all missing values identified by their positions (i,j). Therefore you should show `NA_values` and take decisions about results. In our case, we delete all rows that contains missing values.

The second script (2. NormalizedDataset) normalize dataset in range of 0 to 1. This is important before apply any algorithm. In our case a dataset non-normalized affects the algorithms that apply later.

## PCA

The Third script (3. PCA) are used to reduce the dimensionality and also to find outliers. If you want find outliers, you must run script 3 and later script 4 iteratively until have no more outliers. As a result of this process you will have a "clean" dataset.

```
do {
  run 3. PCA.R;
  run 4. Outliers.R;
}
while (have no more outliers);
```

Unfortunately, this process isn't automatic, therefore you must support on graphics, take decisions and change main program according you need.

## Scatterplot Matrix

The fifth script (5. Scatterplot Matrix) show the first 3 Principal Components after apply PCA Algorithm. This show us important information about behavior dataset that is impossible see with many dimensions.

## Regression

__In Progress__
