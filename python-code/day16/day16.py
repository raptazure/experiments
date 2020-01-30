from datetime import timedelta
from datetime import time
from datetime import date
from datetime import datetime

''' Getting the datetime information '''
now = datetime.now()
print(now)
day = now.day
month = now.month
year = now.year
hour = now.hour
minute = now.minute
second = now.second
timestamp = now.timestamp()
print(day, month, year, hour, minute)
print('timestamp', timestamp)
print(f'{day}/{month}/{year}, {hour}:{minute}')

''' Time formating '''
t = now.strftime("%H:%M:%S")
print("time:", t)

# mm/dd/YY H:M:S format
time_one = now.strftime("%m/%d/%Y, %H:%M:%S")
print("time one:", time_one)

# dd/mm/YY H:M:S format
time_two = now.strftime("%d/%m/%Y, %H:%M:%S")
print("time two:", time_two)

''' String to time using strptime '''
date_string = "5 December, 2019"
print("date_string =", date_string)
date_object = datetime.strptime(date_string, "%d %B, %Y")
print("date_object =", date_object)

''' Use date from datetime '''
new_year = date(2020, 1, 1)
print(new_year)
today = new_year.today()
print('Current time is', today)
print("Current year:", today.year)
print("Current month:", today.month)
print("Current day:", today.day)

''' Time object to represent time '''
a = time()
print("a =", a)

# time(hour, minute and second)
b = time(10, 30, 50)
print("b =", b)

# time(hour, minute and second)
c = time(hour=10, minute=30, second=50)
print("c =", c)

# time(hour, minute, second, microsecond)
d = time(10, 30, 50, 200555)
print("d =", d)

''' Difference between two datetime '''
today = date(year=2019, month=12, day=5)
new_year = date(year=2020, month=1, day=1)
time_left_for_newyear = new_year - today
# Time left for new year:  27 days, 0:00:00
print('Time left for new year: ', time_left_for_newyear)

t1 = datetime(year=2019, month=12, day=5, hour=0, minute=59, second=0)
t2 = datetime(year=2020, month=1, day=1, hour=0, minute=0, second=0)
diff = t2 - t1
# Time left for new year: 26 days, 23: 01: 00
print('Time left for new year:', diff)

''' Difference between two dates and times using timedelata '''
t1 = timedelta(weeks=12, days=10, hours=4, seconds=20)
t2 = timedelta(days=7, hours=5, minutes=3, seconds=30)
t3 = t1 - t2
print("t3 =", t3)
