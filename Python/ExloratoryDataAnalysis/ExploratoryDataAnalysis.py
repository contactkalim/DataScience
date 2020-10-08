import pandas as pd
import numpy as np

import matplotlib.pyplot as plt
from matplotlib import cm
from mpl_toolkits.basemap import Basemap


#Please change file path as appropriate
df_ind_cities = pd.read_csv ("../Assignment_Python/Indian_cities.csv")
#df_ind_cities = pd.read_csv ("C:/Users/Shadab/Documents/Python/Assignment_Python/Indian_cities.csv")
print(df_ind_cities.info())
print(df_ind_cities.describe())

# We cans ee top 10 cities with most population
print("The Top 10 Cities Descending Order of Total Population")
top_pop_cities = df_ind_cities.sort_values(by='population_total',ascending=False)
top10_pop_cities=top_pop_cities.head(10)
top10_pop_cities

#We can see maximum cities from UP, WB
plt.figure(figsize=(15,12))
states = df_ind_cities.groupby('state_name')['name_of_city'].count().sort_values(ascending=True)
states.plot(kind="barh", fontsize = 20, color = "green")
plt.grid(b=True, which='both', color='Black',linestyle='-')
plt.xlabel('No of cities in provided dataset', fontsize = 20)
plt.show()
    
#Haryana is notorious for female infanticide, lets see if data supoorts this
#The graph does show Haryana at the bottom

plt.figure(figsize=(15,12))
states = df_ind_cities.groupby('state_name')['child_sex_ratio'].mean().sort_values(ascending=True)
states.plot(kind="barh", fontsize = 20, color = "green")
plt.grid(b=True, which='both', color='Black',linestyle='-')
plt.xlabel('No of girls per 1000 boys', fontsize = 20)
plt.show ()

plt.figure(figsize=(15,12))
states = df_ind_cities.groupby('state_name')['sex_ratio'].mean().sort_values(ascending=True)
states.plot(kind="barh", fontsize = 20, color = "green")
plt.grid(b=True, which='both', color='Black',linestyle='-')
plt.xlabel('Sex Ratio', fontsize = 20)
plt.show ()


# We can see 2 Aurangabad one in Bihar, one in Maharashtra. Max cities from UP, no Null values
print (df_ind_cities.describe(include=['O']))

#We can see States with highest urban population as Maharashtra, UP ans AP (as data is City wise where city implies urban area)
plt.figure(figsize=(15,12))
states = df_ind_cities.groupby('state_name')['population_total'].sum().sort_values(ascending=True)
states.plot(kind="barh", fontsize = 20, color = "green")
plt.grid(b=True, which='both', color='Black',linestyle='-')
plt.xlabel('Urban Population', fontsize = 20)
plt.show ()

#We can see States with highest male urban population as Maharashtra

plt.figure(figsize=(15,12))
states = df_ind_cities.groupby('state_name')['population_male'].sum().sort_values(ascending=True)
states.plot(kind="barh", fontsize = 20, color = "green")
plt.grid(b=True, which='both', color='Black',linestyle='-')
plt.xlabel('Male Population', fontsize = 20)
plt.show ()


def plot_map(sizes, colorbarValue):

    plt.figure(figsize=(19,20))
    f, ax = plt.subplots(figsize=(19, 20))

    # Setting up Basemap
    map = Basemap(width=5000000, height=3500000, resolution='l', projection='aea', llcrnrlon=69,
                  llcrnrlat=6, urcrnrlon=99, urcrnrlat=36, lon_0=78, lat_0=20, ax=ax)
                  
    map.drawmapboundary()
    map.drawcountries()
    map.drawcoastlines()    
    
    x, y = map(np.array(df_ind_cities["longitude"]), np.array(df_ind_cities["latitude"]))
    cs = map.scatter(x, y, s=sizes, marker="o", c=sizes, cmap=cm.Dark2, alpha=0.5)
    cbar = map.colorbar(cs, location='right',pad="5%")
    cbar.ax.set_yticklabels(colorbarValue)
    
    plt.show()

# Plotting the states with highest male population on the map
df_ind_cities['latitude'] = df_ind_cities['location'].apply(lambda x: x.split(',')[0])
df_ind_cities['longitude'] = df_ind_cities['location'].apply(lambda x: x.split(',')[1])

    
population_sizes = df_ind_cities["population_total"].apply(lambda x: int(x / 5000))
colorbarValue = np.linspace(df_ind_cities["population_total"].min(), df_ind_cities["population_total"].max(), 
                            num=10)
colorbarValue = colorbarValue.astype(int)

plot_map(population_sizes, colorbarValue)

#We can see states with highest female population

plt.figure(figsize=(16,16))
states = df_ind_cities.groupby('state_name')['population_female'].sum().sort_values(ascending=True)
states.plot(kind="barh", fontsize = 20, color = "green")
plt.grid(b=True, which='both', color='Black',linestyle='-')
plt.xlabel('Female Poulation', fontsize = 20)
plt.show ()

#We can see States with highest children population (0-6)

plt.figure(figsize=(16,16))
states = df_ind_cities.groupby('state_name')['0-6_population_total'].sum().sort_values(ascending=True)
states.plot(kind="barh", fontsize = 20, color = "green")
plt.grid(b=True, which='both', color='Black',linestyle='-')
plt.xlabel('Children Population', fontsize = 20)
plt.show ()

population_sizes = df_ind_cities["0-6_population_total"].apply(lambda x: int(x / 5000))
colorbarValue = np.linspace(df_ind_cities["0-6_population_total"].min(), df_ind_cities["0-6_population_total"].max(), 
                            num=10)
colorbarValue = colorbarValue.astype(int)

plot_map(population_sizes, colorbarValue)

#Analysing the effective Literacy rates
#As expected Kerala is in top 3
state_literacy_effective = df_ind_cities[["state_name",
                                          "effective_literacy_rate_total",
                                          "effective_literacy_rate_male",
                                          "effective_literacy_rate_female"]].groupby("state_name").agg({"effective_literacy_rate_total":np.average,
                                                                                                "effective_literacy_rate_male":np.average,
                                                                                                "effective_literacy_rate_female":np.average})
state_literacy_effective.sort_values("effective_literacy_rate_total", ascending=True).plot(kind="barh",
                      grid=True,
                      figsize=(30,29),
                      alpha = 0.6,
                      width=0.6,
                      stacked = False,
                      edgecolor="g",
                      fontsize = 20)
plt.grid(b=True, which='both', color='lightGreen',linestyle='-')
plt.show ()

#Analysing graduates accross states
#Kerala and Meghalaya are the only states that have more number of female graduates than 
# male graduates
state_graduates  = df_ind_cities[["state_name",
                                  "total_graduates",
                                  "male_graduates",
                                  "female_graduates"]].groupby("state_name").agg({"total_graduates":np.average,
                                                                                  "male_graduates":np.average,
                                                                                  "female_graduates":np.average})
# Plotting the bar chart 
state_graduates.sort_values("total_graduates", ascending=True).plot(kind="barh",
                      grid=True,
                      figsize=(30,29),
                      alpha = 0.6,
                      width=0.6,
                      stacked = False,
                      edgecolor="g",
                      fontsize = 20)
plt.grid(b=True, which='both', color='lightGreen',linestyle='-')
plt.show ()
