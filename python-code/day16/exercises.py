from datetime import datetime
from datetime import date

now = datetime.now()
day = now.day
month = now.month
year = now.year
hour = now.hour
minute = now.minute
second = now.second
print(f'{month}/{day}/{year}, {hour}:{minute}:{second}')

date_string = "5 December, 2019"
date_object = datetime.strptime(date_string, "%d %B, %Y")
print(date_object)

today = datetime(year=2020, month=1, day=30, hour=19, minute=51)
new_year = datetime(year=2021, month=1, day=1)
time_left_for_newyear = new_year - today
print('Time left for new year: ', time_left_for_newyear)

unix_time = datetime(year=1970, month=1, day=1)
time_diff = today - unix_time
print(time_diff)
