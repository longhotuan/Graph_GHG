# import libraries
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib as mpl
import matplotlib.pyplot as plt
from pandas import plotting
from scipy import stats
plt.style.use("ggplot")
import warnings
warnings.filterwarnings("ignore")
from scipy import stats
from matplotlib.pyplot import figure
import dfply

#### import data ####
import os

# print(os.listdir("C:/Users/htunlong/OneDrive - UGent/Research/Postdoc/Papers/GHG paper/Graph_GHG/Figures/GHG_py"))
# os. chdir("C:/Users/htunlong/OneDrive - UGent/Research/Postdoc/Papers/GHG paper/Graph_GHG/Figures/GHG_py")
data = pd.read_csv("River.csv")
data >> head(5)
data.head(5)
data.tail(5)
data.columns
data.describe()
data.info()
data['River'] = data['River'].astype('category')
data['Shading'] = data['Shading'].astype('category')
data['Pool_class'] = data['Pool_class'].astype('category')

len(data['River'].unique()
    
data >> select('River', 'T_w') >> filter_by(X.T_w < 20, X.River == 'Cuenca')

# make a histogram

for i in data['River'].unique():
    m = plt.hist(data[data["River"] == i].Dis_N2O_cor, bins = 30)
    plt.legend()
    plt.xlabel(r'$N_{2}O (mg.l^{-1})$')
    plt.ylabel('Frequency')
    
# make a boxplot

data2 = data.iloc[:, 6:13]
font = {'family' : 'normal',
        'weight' : 'bold',
        'size'   : 22}

mpl.rc('font', **font)  
  
for i in data2.columns:    
    plt.figure(num=None, figsize=(20, 20), dpi=300, facecolor='w', edgecolor='k')
    sns.boxplot(x = data['River'], y = i, data = data)
    plt.savefig('boxplot_{0}.jpg'.format(i))
    plt.close()
    
    
    
