import pandas as pd
import tidypolars as tp
from tidypolars import col, desc
import geopandas as gpd
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable

# add utils file
import utils

# read in data
census = pd.read_excel('Data/Census2021-health-la.xlsx', skiprows=[0, 1, 2, 3])
census_tp = tp.from_pandas(census)

# get local authority map shapefiles
la_map = gpd.read_file("Data/Local_Authority_Districts_December_2021/LAD_DEC_2021_GB_BGC.shp")

# calculate percentage that have good/very good
good_health = (
    census_tp
    .mutate(Percentage = col('Very good health \n(age-standardised \nproportions)') + col('Good health \n(age-standardised \nproportions)'))
    .select('Area code', 'Percentage')
)
good_health = good_health.to_pandas()

# combine
ew_map = la_map.merge(good_health, left_on='LAD21CD', right_on='Area code')

# highest / lowest percentage
ew_map.query('Percentage == Percentage.max()')
ew_map.query('Percentage == Percentage.min()')
round(ew_map['Percentage'].mean(), 1)

# plot
plt.figure()
fig, ax = plt.subplots(1, 1)
divider = make_axes_locatable(ax)
cax = divider.append_axes("bottom", size="5%", pad=0.5)
ew_map.plot(ax=ax,
        legend=True,
        edgecolor="black",
        linewidth = 0.2,
        legend_kwds={"orientation": "horizontal"},
        column="Percentage",
        cax=cax,
        cmap='viridis', vmin = 70, vmax=90)
ax.axis('off')
cb_ax = fig.axes[1]
cb_ax.set_title('Percentage reporting good or very good health', fontsize=6)
cb_ax.tick_params(labelsize=5)
plt.text(0.04, 0.08, r"$\bf{Data}$" + r": Office for National Statistics (ONS). General health, England and Wales: Census 2021.", transform=fig.transFigure, fontsize=5)
plt.text(0.04, 0.06, r"$\bf{Graphic}$" + r": Nicola Rennie", transform=fig.transFigure, fontsize=5)
plt.text(0.04, 0.94, "Percentage of people reporting good or very good general\nhealth in the 2021 Census", transform=fig.transFigure, fontsize=8, weight='bold')
plt.suptitle("Census 2021 respondents were asked to rate their general health as being either very\ngood, good, fair, bad, or very bad. The map below shows the (age-standardised)\npercentage of respondents in each local authority who rated their health as being good\nor very good. Blaenau Gwent had the lowest percentage of people reporting good or\nvery good health at 74.2%. The average across all local authorities is 81.8%.", fontsize=6, x=0.04, y=.92, horizontalalignment='left', verticalalignment='top')
plt.ticklabel_format(useOffset=False, style='plain')
add_logo("twitter.png", "nrennie35", 0.04, 0.035, 0.02, 500, 300)
add_logo("github.png", "nrennie", 0.04, 0.015, 0.02, 800, 400)
plt.show()

# save
fig = plt.gcf()
fig.set_size_inches(4, 6)
fig.savefig('Images/general-health-2021-census-b.png', dpi=300)
