import pandas as pd
import numpy as np

'''Pandas'''
# Creating Pandas Series with default index
nums = [1, 2, 3, 4, 5]
s = pd.Series(nums)
print(s)

# Creating Pandas Series with custom index
nums = [1, 2, 3, 4, 5]
s = pd.Series(nums, index=[1, 2, 3, 4, 5])
print(s)

# Creating Pandas Series from a dictionary
dct = {'name': 'Asabeneh', 'country': 'Finland', 'city': 'Helsinki'}
s = pd.Series(dct)
print(s)

# Creating a constant pandas series
s = pd.Series(10, index=[1, 2, 3])
print(s)

# Creating a pandas series using linspace
s = pd.Series(np.linspace(5, 20, 10))
print(s)

'''DataFrames'''
# Creating DataFrames from list of lists
data = [
    ['Asabeneh', 'Finland', 'Helsink'],
    ['David', 'UK', 'London'],
    ['John', 'Sweden', 'Stockholm']
]
df = pd.DataFrame(data, columns=['Names', 'Country', 'City'])
print(df)

# Creating DataFrame using Dictionary
data = {'Name': ['Asabeneh', 'David', 'John'], 'Country': [
    'Finland', 'UK', 'Sweden'], 'City': ['Helsiki', 'London', 'Stockholm']}
df = pd.DataFrame(data)
print(df)

# Creating DataFrams from list of dictionaries
data = [
    {'Name': 'Asabeneh', 'Country': 'Finland', 'City': 'Helsinki'},
    {'Name': 'David', 'Country': 'UK', 'City': 'London'},
    {'Name': 'John', 'Country': 'Sweden', 'City': 'Stockholm'}]
df = pd.DataFrame(data)
print(df)

'''Reading CSV File using pandas'''
df = pd.read_csv('./data/weight-height.csv')
df.head()   # Reading only the first 5 rows
df.tail()   # Reading only the last 5 rows
df.shape    # Presenting the numbers of rows and columns
df.columns  # Getting all the columns
heights = df['Height']  # Getting specif colums using the column key
heights.describe()  # Getting statisical information about height data

'''Modifying DataFrame'''
# Create a DataFrame
data = [
    {"Name": "Asabeneh", "Country": "Finland", "City": "Helsinki"},
    {"Name": "David", "Country": "UK", "City": "London"},
    {"Name": "John", "Country": "Sweden", "City": "Stockholm"}]
df = pd.DataFrame(data)
print(df)

# Add new column
weights = [74, 78, 69]
df['Weight'] = weights
birth_year = ['1769', '1985', '1990']
df['Birth Year'] = birth_year
print(df)

# Modify column values
df['Height'] = df['Height'] * 0.01
def calculate_bmi():
    weights = df['Weight']
    heights = df['Height']
    bmi = []
    for w, h in zip(weights, heights):
        b = w/(h*h)
        bmi.append(b)
    return bmi
bmi = calculate_bmi()
df['BMI'] = bmi
print(df)

# Format DataFrame column
df['BMI'] = round(df['BMI'], 1)
print(df)

'''Checking data types of Column values'''
# Data types of column values
df.Weight.dtype     # dtype('int64')
df['Birth Year'].dtype      # dtype('O')
df['Birth Year'] = df['Birth Year'].astype('int')   # change data type
df['Birth Year'].dtype   # dtype('int64')

# Boolean Indexing
df['Ages'] = df[df['Ages'] > 120]
