# Set is a collection of unordered and unindexed distinct elements.

''' Creating a set '''
st = {}
st = set()
fruits = {'banana', 'orange', 'mango', 'lemon'}

''' Adding items to a list '''
# Single item
fruits.add('apple')
print(fruits)

# Multiple items
st = {'item1', 'item2', 'item3', 'item4'}
st.update(['item5','item6','item7'])
print(st)

''' Removing item from a list '''
st.remove('item2')
print(st)
fruits.pop()
print(fruits)

''' Deleting a set '''
del st
try: print(st)
except BaseException as e: print(e, ">> go on")

''' Converting list to set '''
lst = ['item1', 'item2', 'item3', 'item4', 'item1']
st = set(lst)    # {'item2', 'item4', 'item1', 'item3'}

''' Joining sets '''
# union - return a new set
st1 = {'item1', 'item2', 'item3', 'item4'}
st2 = {'item5', 'item6', 'item7', 'item8'}
st3 = st1.union(st2)
print(st3)

# update - insert another set
fruits = {'banana', 'orange', 'mango', 'lemon'}
vegetables = {'Tomato', 'Potato', 'Cabbage','Onion', 'Carrot'}
fruits.update(vegetables)
print(fruits)

''' Finding intersection items '''
st1 = {'item1', 'item2', 'item3', 'item4'}
st2 = {'item3', 'item2'}
print(st1.intersection(st2))    # {'item3', 'item2'}

''' Checking subset and super set '''
print(st2.issubset(st1))        # True
print(st1.issuperset(st2))      # True

''' Checking difference between two sets '''
python = {'p', 'y', 't', 'o','n'}
dragon = {'d', 'r', 'a', 'g', 'o','n'}
print(python.difference(dragon))     # {'p', 'y', 't'}        (A\B)
print(dragon.difference(python))     # {'d', 'r', 'a', 'g'}   (B\A)

''' Finding Symmetric difference between two sets '''
print(python.symmetric_difference(dragon))  # {'r', 't', 'p', 'y', 'g', 'a', 'd'}  (A\B)U(B\A)

''' Joint or disjoint '''
st1 = {'item1', 'item2', 'item3', 'item4'}
st2 = {'item2', 'item3'}
print(st2.isdisjoint(st1))   # False - no common item