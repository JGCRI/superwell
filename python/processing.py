# Process and plot superwell outputs

import geopandas as gpd
import matplotlib.pyplot as plt

# %% Plot distributions of results

# Read inputs
results = pd.read_csv(output_path + "\\" + output_name, low_memory=False)
input = gpd.read_file('../inputs/geoprocessed/All_merged.shp')

### plot inputs
# Create the map
fig, ax = plt.subplots(figsize=(16, 9))

# Plot the porosity map without borders
input.plot(column='MEAN_Poros', ax=ax, edgecolor='None')

# Remove axis ticks and labels
ax.set_xticks([])
ax.set_yticks([])
ax.axis('off')

# Show and save the plot as an image
plt.show()
plt.savefig('../processing/gridded_map_py.png', bbox_inches='tight', pad_inches=0)

### plot outputs
first_year_unit_cost = results.where(results.year_number == 1)
first_year_unit_cost = first_year_unit_cost.dropna(thresh=5)

plt.scatter(first_year_unit_cost.hydraulic_conductivity * 86400, first_year_unit_cost.unit_cost_per_acreft)
plt.xscale('log')
plt.yscale('log')
plt.xlabel('Aquifer K (m/d)')
plt.ylabel('$ per acre-foot')
plt.show()

plt.scatter(first_year_unit_cost.well_yield * 60 * 264.17, first_year_unit_cost.unit_cost_per_acreft)
plt.xscale('log')
plt.yscale('log')
plt.xlabel('Well pumping rate (gpm)')
plt.ylabel('$ per acre-foot')
plt.show()

plt.scatter(first_year_unit_cost.areal_extent * 0.000247105, first_year_unit_cost.unit_cost_per_acreft)
plt.xscale('log')
plt.yscale('log')
plt.xlabel('Acres per well')
plt.ylabel('$ per acre-foot')
plt.show()


