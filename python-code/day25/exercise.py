import os
import numpy as np
import pandas as pd

os.chdir(r'day25/data')
df = pd.read_csv('hacker_ness.csv')
df.head()
df.tail()

cols = df.columns[0]  # Getting all the columns
s = pd.Series(cols)
print(s)

df.shape
df['title'] = df[df['title'] != 'python']
df['title'] = df[df['title'] != 'JavaScript']
print(df)
