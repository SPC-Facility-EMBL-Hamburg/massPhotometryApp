# For each from scipy.optimize import curve_fit
import pandas as pd
import numpy  as np
import h5py
import matplotlib.pyplot as plt

file = '/home/os/Downloads/007_MFP1.h5'
file = '/home/os/Downloads/006_PPXD11nM.h5'


data = data = h5py.File(file, 'r')

data_keys = data.keys() 

# For each dataset plot an histogram
for key in data_keys:

    try:

        values = np.array(data[key][:]).squeeze()
        values = values[~np.isnan(values)]
        plt.hist(values, bins=50, alpha=0.5, label=key)
        plt.xlabel('Values')
        plt.ylabel('Frequency')
        plt.title('Histogram of Datasets in HDF5 File')
        plt.legend()
        # Export as PNG
        plt.savefig(f'histogram_{key}.png')
        plt.close()
    except Exception as e:
        pass