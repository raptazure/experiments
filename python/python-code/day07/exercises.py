it_companies = {'Facebook', 'Google', 'Microsoft', 'Apple', 'IBM', 'Oracle', 'Amazon'}
A = {19, 22, 24, 20, 25, 26}
B = {19, 22, 20, 25, 26, 24, 28, 27}
age = [22, 19, 24, 25, 26, 24, 25, 24]

print(len(it_companies))
it_companies.add("Twitter")
it_companies.update(['xx', 'xxx'])
it_companies.remove('xx')
it_companies.remove('xxx')
it_companies.discard('Twitter')
print(it_companies)

C = A.union(B)
print(C)
print(A.intersection(B))
print(A.issubset(B))
print(A.isdisjoint(B))
print(A.symmetric_difference(B))
del A
try: print(A)
except BaseException as e: print(e, ">> go on")
age = set(age)
sen = "I am a teacher and I love to inspire and teach people"
words = set(sen)
print(len(words) - 1)
