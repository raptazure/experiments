import re

'''Regular Expression'''
# Match
txt = 'I love to teach python or javaScript'
match = re.match('I love to teach', txt, re.I)
print(match)  # <re.Match object; span=(0, 15), match='I love to teach'>
span = match.span()  # Use span to get the starting and ending position
print(span)     # (0, 15)
start, end = span    # Find the start and stop position from the span
print(start, end)  # 0, 15
substring = txt[start:end]
print(substring)       # I love to teach

# Search
txt = '''Python is the most beautiful language that a human begin has ever created.
I recommend python for a first programming language'''
match = re.search('first', txt, re.I)
print(match)  # <re.Match object; span=(100, 105), match='first'>
span = match.span()    # Use span to get the starting and ending position
print(span)     # (100, 105)
start, end = span    # Find the start and stop position from the span
print(start, end)  # 100 105
substring = txt[start:end]
print(substring)       # first

# Searching all matches using findall
txt = '''Python is the most beautiful language that a human begin has ever created.
I recommend python for a first programming language'''
matches = re.findall('python', txt, re.I)
print(matches)  # ['Python', 'python']
matches = re.findall('Python|python', txt)
print(matches)  # ['Python', 'python']
matches = re.findall('[Pp]ython', txt)
print(matches)  # ['Python', 'python']

# Replacing a substring
txt = '''Python is the most beautiful language that a human begin has ever created.
I recommend python for a first programming language.'''
match_replaced = re.sub('Python|python', 'JavaScript', txt, re.I)
print(match_replaced)
match_replaced = re.sub('[Pp]ython', 'JavaScript', txt, re.I)
print(match_replaced)

'''Spliting text using RegEx split'''
txt = '''A,B,C,D'''
print(re.split(',', txt))  # ['A', 'B', 'C', 'D']

'''Writing RegEx pattern'''
# Square Bracket([])
regex_pattern = r'[Aa]pple|[Bb]anana'  # this square bracket mean either A or a
txt = 'Apple and banana are fruits. An old cliche says an apple a day a doctor way has been replaced by a banana a day keeps the doctor far far away. '
matches = re.findall(regex_pattern, txt)
print(matches)  # ['Apple', 'banana', 'apple', 'banana']

# Escape character(\)
regex_pattern = r'\d'  # d is a special character which means digits
txt = 'This regular expression example was made in December 6,  2019.'
matches = re.findall(regex_pattern, txt)
print(matches)  # ['6', '2', '0', '1', '9']

# One or more times(+)
regex_pattern = r'\d+'  # d+ mean one or more times
txt = 'This regular expression example was made in December 6,  2019.'
matches = re.findall(regex_pattern, txt)
print(matches)  # ['6', '2019']

# Period(.)
txt = '''Apple and banana are fruits'''
regex_pattern = r'[a].+'  # . any character, + any character one or more times
matches = re.findall(regex_pattern, txt)
print(matches)  # ['and banana are fruits']

# Zero or more times(*)
regex_pattern = r'[a].*'  # . any character, + any character one or more times
txt = '''Apple and banana are fruits'''
matches = re.findall(regex_pattern, txt)
print(matches)  # ['and banana are fruits']

# Zero or one times(?)
txt = '''I am not sure if there is a convention how to write the word e-mail.
Some people write it email others may write it as Email or E-mail.'''
regex_pattern = r'[Ee]-?mail'  # ? means optional
matches = re.findall(regex_pattern, txt)
print(matches)  # ['e-mail', 'email', 'Email', 'E-mail']

# Starts with(^)
txt = 'This regular expression example was made in December 6,  2019.'
regex_pattern = r'^This'  # ^ means starts with
matches = re.findall(regex_pattern, txt)
print(matches)  # ['This']

# Negation(^)
txt = 'This regular expression example was made in December 6,  2019.'
# ^ in set character means negation, not A to Z, not a to z, no space
regex_pattern = r'[^A-Za-z ]+'
matches = re.findall(regex_pattern, txt)
print(matches)  # ['6,', '2019.']

#Quantifier in RegEx
txt = 'This regular expression example was made in December 6 ,  2019.'
regex_pattern = r'\d{1,4}'   # 1 to 4
matches = re.findall(regex_pattern, txt)
print(matches)  # ['6', '2019']
