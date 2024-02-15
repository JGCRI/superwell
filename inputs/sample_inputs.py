# This script is used to sample inputs from all inputs such that the distributions of both the sampled inputs and
# the original inputs are the same
#
# Hassan Niazi, Nov 2023


# check my current directory
import os
os.getcwd()

# Import the required libraries
import pandas as pd
import numpy as np
from scipy.stats import lognorm, norm
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.mixture import GaussianMixture


# functions
# function to sample from a log-normal distribution given the sample mean and standard deviation
def sample_lognormal(data, n_samples=100):
    # Calculate the log-space mean and standard deviation
    log_data = np.log(data)
    shape = log_data.std()
    scale = np.exp(log_data.mean())
    # Sample from the log-normal distribution
    return lognorm.rvs(s=shape, scale=scale, size=n_samples)

# function to sample from a normal distribution given the sample mean and standard deviation
def sample_normal(data, n_samples=100):
    mean = data.mean()
    std = data.std()
    return norm.rvs(loc=mean, scale=std, size=n_samples)

# Adjusting the sampling method for 'Depth_to_water' to exclude zero values
def adjusted_sample_lognormal(data, n_samples=100):
    # Exclude zero or negative values
    positive_data = data[data > 0]
    return sample_lognormal(positive_data, n_samples=n_samples)


########### loading the inputs #############
# load input data
inputs_dir = '../inputs'
inputs_raw = pd.read_csv(os.path.join(inputs_dir, 'inputs.csv'))
inputs_raw.head()

# analyzing the distribution of input variables
sample_vars = ["Porosity", "Permeability", "Aquifer_thickness", "Depth_to_water", "Grid_area"]
summary_stats_input = inputs_raw[sample_vars].describe()
summary_stats_input

# Plotting histograms for each numerical variable
fig, axes = plt.subplots(len(sample_vars), 1, figsize=(10, 15))

for i, var in enumerate(sample_vars):
    # color the histograms blue
    inputs_raw[var].hist(ax=axes[i], bins=50, alpha=0.9, color='blue')
    axes[i].set_title(var)

plt.tight_layout()
plt.show()

fig.savefig(os.path.join(inputs_dir, 'inputs_histograms.png'))


########### sampling the inputs #############
# variables to be sampled
nsamples = 100
sampled_data = {} # initialize a dictionary to hold the sampled data

# add placeholder columns GridCellID, Continent, Country, GCAM_basin_ID, Basin_long_name to sampled_data
for var in ['GridCellID', 'Continent', 'Country', 'GCAM_basin_ID', 'Basin_long_name']:
    # add dummy values for the placeholder columns
    if var == 'GridCellID':
        sampled_data[var] = np.arange(1, nsamples+1)
    elif var in ['Continent', 'Country', 'Basin_long_name']:  # Modified this line
        sampled_data[var] = ['USA'] * nsamples  # 'USA' for all entries in these columns
    elif var == 'GCAM_basin_ID':
        sampled_data[var] = np.arange(1, nsamples+1)

# Analyzing the distribution of "WHYClass"
whyclass_distribution = inputs_raw['WHYClass'].value_counts(normalize=True)

# Sample values for 'WHYClass' based on the observed frequencies
sampled_data['WHYClass'] = np.random.choice(inputs_raw['WHYClass'].unique(), size=nsamples, p=whyclass_distribution)

# Sample values for 'Porosity' directly from observed values (relatively uniform distribution)
sampled_data['Porosity'] = np.random.choice(inputs_raw['Porosity'], size=nsamples)

# Sample values for 'Permeability' assuming a normal distribution
sampled_data['Permeability'] = sample_normal(inputs_raw['Permeability'])

# Sample for 'Grid_area' using the bootstrap method
sampled_data['Grid_area'] = np.random.choice(inputs_raw['Grid_area'], size=nsamples, replace=True)


# Sample values for the remaining variables assuming a log-normal distribution
for variable in ["Aquifer_thickness", "Depth_to_water"]:
    if variable == "Depth_to_water":
        # Exclude zero values for 'Depth_to_water'
        sampled_data[variable] = adjusted_sample_lognormal(inputs_raw[variable])
    else:
        sampled_data[variable] = sample_lognormal(inputs_raw[variable])

########### saving and plotting the sampled inputs #############
# Convert the sampled data to a DataFrame
sampled_df = pd.DataFrame(sampled_data)

# Save the sampled data to a CSV file
sampled_data_file_path = os.path.join(inputs_dir, f'sampled_data_{nsamples}.csv')
sampled_df.to_csv(sampled_data_file_path, index=False)


# Plotting the distributions and calculating summary statistics
fig, axes = plt.subplots(len(sample_vars), 1, figsize=(10, 20))

for i, var in enumerate(sample_vars):
    # Raw and sampled data histograms
    sns.histplot(inputs_raw[var], bins=50, ax=axes[i], kde=False, color='blue', stat='density', label='Raw Inputs histogram')
    sns.histplot(sampled_df[var], bins=50, ax=axes[i], kde=False, color='orange', stat='density', alpha=0.5,
                 label='Sampled Inputs histogram')
    # Raw and sampled data KDEs
    sns.kdeplot(inputs_raw[var], ax=axes[i], color='darkblue', label='Raw Inputs KDE')
    sns.kdeplot(sampled_df[var], ax=axes[i], color='orange', label='Sampled Inputs KDE')

    axes[i].set_title(f'Distribution of {var} - Raw vs Sampled')
    axes[i].legend()

plt.tight_layout()
plt.show()

fig.savefig(os.path.join(inputs_dir, f'sampled_data_{nsamples}.png'))

# Summary statistics for both raw and sampled data
summary_statistics = {
    'Raw Data': inputs_raw[sample_vars].describe(),
    'Sampled Data': sampled_df[sample_vars].describe()
}

summary_statistics

