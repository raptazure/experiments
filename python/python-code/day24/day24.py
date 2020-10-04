import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats

'''NumPy'''
# Check Numpy
print('numpy:', np.__version__)  # Checking the version of the numpy package
print(dir(np))     # Checking the available methods

# Creating numpy array using
python_list = [1, 2, 3, 4, 5]
two_dimensional_list = [[0, 1, 2], [3, 4, 5], [6, 7, 8]]
python_tuple = (1, 2, 3, 4, 5)
print(type(python_list))  # <class 'list'>
print(type(python_tuple))  # <class 'tuple'>

numpy_array_from_list = np.array(python_list)   # Creating int numpy arrays
print(type(numpy_array_from_list))   # <class 'numpy.ndarray'>
print(numpy_array_from_list)  # array([1, 2, 3, 4, 5])

numpy_array_from_list2 = np.array(python_list, dtype=float)  # Creating float numpy arrays
print(type(numpy_array_from_list2))   # <class 'numpy.ndarray'>
print(numpy_array_from_list2)  # array([1., 2., 3., 4., 5.])

numpy_bool_array = np.array([0, 1, -1, 0, 0], dtype=bool)   # Creating boolean numpy arrays
print(type(numpy_bool_array))   # <class 'numpy.ndarray'>
print(numpy_bool_array)  # array([False,  True,  True, False, False])

numpy_two_dimensional_list = np.array(two_dimensional_list) # Creating multidimensional array using numpy
print(type(numpy_two_dimensional_list))    # <class 'numpy.ndarray'>
print(numpy_two_dimensional_list)

numpy_array_from_tuple = np.array(python_tuple) # Creating numpy array from tuple
print(type(numpy_array_from_tuple))  # <class 'numpy.ndarray'>
print(numpy_array_from_tuple)  # [1 2 3 4 5]

np_to_list = numpy_array_from_list.tolist()  # Converting numpy array to list
print(type(np_to_list))    # <class 'list'>
print('one dimensional array:', numpy_array_from_list.tolist())     # one dimensional array: [1, 2, 3, 4, 5]
print('two dimensional array:', numpy_two_dimensional_list.tolist())    # two dimensional array:  [[0, 1, 2], [3, 4, 5], [6, 7, 8]]

# Attributes of numpy array
nums = np.array([1, 2, 3, 4, 5])
numpy_two_dimensional_list = np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
three_by_four_array = np.array([[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11]])
print('shape of nums: ', nums.shape)    # shape of nums: (5,)
print('shape of numpy_two_dimensional_list: ',numpy_two_dimensional_list.shape)     # shape of numpy_two_dimensional_list: (3, 3)
print('shape of three_by_four_array: ', three_by_four_array.shape)  # shape of three_by_four_array: (3, 4)

int_lists = [-3, -2, -1, 0, 1, 2, 3]
int_array = np.array(int_lists)
float_array = np.array(int_lists, dtype=float)
print('Data type of int_array: ', int_array.dtype)  # Data type of int_array: int64
print('Data type of float_array: ', float_array.dtype)  # Data type of float_array: float64
print('The size of numpy_array_from_list: ', numpy_array_from_list.size)    # The size of numpy_array_from_list: 5
print('The size of numpy_two_dimensional_list:',numpy_two_dimensional_list.size)  # The size of two_dimensional_list: 3


'''Mathematical Operation using numpy'''
# Basic Mathematical Operation
original_array = [1, 2, 3, 4, 5]
numpy_array_from_list = np.array(original_array)
print('original array: ', numpy_array_from_list)    # original array:  [1 2 3 4 5]
res = numpy_array_from_list + 10  # Addition
print(res)   # [11 12 13 14 15]
res = numpy_array_from_list - 10    # Subtraction
print(res)   # [-9 -8 -7 -6 -5]
res = numpy_array_from_list * 10  # Multiplication
print(res)   # [10 20 30 40 50]
res = numpy_array_from_list / 10     # Division
print(res)   # [0.1 0.2 0.3 0.4 0.5]
res = numpy_array_from_list % 3      # Modulus
print(res)   # [1 2 0 1 2]
res = numpy_array_from_list // 10    # Floor Division
print(res)   # [0 0 0 0 0]
res = numpy_array_from_list ** 2    # Exponential
print(res)   # [ 1  4  9 16 25]

# Converting data types
numpy_arr = np.array([1, 2, 3, 4], dtype='float')    # int to float
print(numpy_arr)    # [1. 2. 3. 4.]
numpy_arr = np.array([1., 2., 3., 4.], dtype='int')   # float to int
print(numpy_arr)   # [1 2 3 4]
numpy_arr = np.array([-3, -2, 0, 1, 2, 3], dtype='bool')  # int to boolean
print(numpy_arr)    # [ True,  True, False,  True,  True,  True]
numpy_arr.astype('str')  # int to str
print(numpy_arr)    # [ True,  True, False,  True,  True,  True]

# Getting items from a numpy array
two_dimension_array = np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
first_row = two_dimension_array[0]
second_row = two_dimension_array[1]
third_row = two_dimension_array[2]
print('First row:', first_row)  # First row: [1 2 3]
print('Second row:', second_row)    # Second row: [4 5 6]
print('Third row: ', third_row)  # Third row:  [7 8 9]
first_column = two_dimension_array[:, 0]
second_column = two_dimension_array[:, 1]
third_column = two_dimension_array[:, 2]
print('First column:', first_column)    # First column: [1 4 7]
print('Second column:', second_column)  # Second column: [2 5 8]
print('Third column: ', third_column)   # Third column:  [3 6 9]

# Basic Array Operation
two_dimension_array = np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
print(two_dimension_array[::])  # Select the whole array
print(two_dimension_array[0:2, 0:2])    # Slice array (first two rows and columns)
print(two_dimension_array[::-1, ::1])  # Reverse the row but the columns remain
two_dimension_array[1, 1] = 55
print(two_dimension_array)  # Represent missing valus
numpy_zeros = np.zeros((3, 3), dtype=int, order='C')
print(numpy_zeros)  # Create zeros array
numpy_ones = np.ones((3, 3), dtype=int, order='C')
print(numpy_ones)   # Create ones array

original_shape = np.array([(1, 2, 3), (4, 5, 6)])
print(original_shape)
reshaped = original_shape.reshape(3, 2)
print(reshaped)     # Reshape the array
flattened = original_shape.flatten()
print(flattened)    # Flatten the array

np_list_one = np.array([1, 2, 3])
np_list_two = np.array([4, 5, 6])
print(np.hstack((np_list_one, np_list_two)))    # Horizontal Append
print(np.vstack((np_list_one, np_list_two)))    # Vertical Append

random_float = np.random.random()
print(random_float)     # Generate a random float number
random_floats = np.random.random(5)
print(random_floats)  # Generate random float numbers in an array
random_int = np.random.randint(0, 11)
print(random_int)   # Generating a random integer between 0 and 10
random_int = np.random.randint(2, 10, size=4)
print(random_int)   # Generating random integers between 2 and 9 in an array
random_int = np.random.randint(2, 10, size=(3, 3))
print(random_int)   # Generating random integers between 2 and 9 in an array
normal_array = np.random.normal(79, 15, 80)
print(normal_array)  # Generating Normal distribution random numbers in array

'''Numpy and Statistics'''
# Numpy numpy.arange()
whole_numbers = np.arange(0, 20, 1)
print(whole_numbers)
odd_numbers = np.arange(1, 20, 2)
print(odd_numbers)

# Creating sequence of numbers
sequence_line = np.linspace(1.0, 5.0, num=10)
print(sequence_line)
sequence_log = np.logspace(2, 4.0, num=4)
print(sequence_log)
sequence_complex = np.array([1, 2, 3], dtype=np.complex128)
print(sequence_complex)

# NumPy Statistical Functions
two_dimension_array = np.array([[3, 7, 6], [4, 2, 5], [1, 9, 8]])
print('min: ', np.min(two_dimension_array))
print('max: ', np.max(two_dimension_array))
print('mean: ', np.mean(two_dimension_array))
print('median: ', np.median(two_dimension_array))
print('Varience: ', np.var(two_dimension_array))
print('sd: ', np.std(two_dimension_array))
print('Percentile: ', np.percentile(two_dimension_array, 50))
print(two_dimension_array)
print('Column with minimum: ', np.amin(two_dimension_array, axis=0))
print('Column with maximum: ', np.amax(two_dimension_array, axis=0))
print('Row with minimum: ', np.amin(two_dimension_array, axis=1))
print('Row with maximum: ', np.amax(two_dimension_array, axis=1))

# Create repeating sequences
a = [1, 2, 3]
print('Tile:   ', np.tile(a, 2))    # Tile:    [1 2 3 1 2 3]
print('Repeat: ', np.repeat(a, 2))  # Repeat:  [1 1 2 2 3 3]

# Generate random numbers
print(np.random.random())
print(np.random.random(size=[2, 3]))
print(np.random.choice(['a', 'e', 'i', 'o', 'u'], size=10))
print(np.random.rand(2, 2))
print(np.random.randn(2, 2))
print(np.random.randint(0, 10, size=[5, 3]))
print(np.random.normal(5, 0.5, 1000))
