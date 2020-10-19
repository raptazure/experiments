# ~T[14:49:17.031039]
Time.utc_now()
t = ~T[14:49:17.031039]
IO.puts(t.hour)
IO.puts(t.minute)

# ~D[2020-10-19]
Date.utc_today()
{:ok, date} = Date.new(2020, 10, 19)
IO.puts(Date.day_of_week(date))
IO.puts(Date.leap_year?(date))

# ~N[2018-10-01 00:00:44]
NaiveDateTime.add(~N[2018-10-01 00:00:14], 30)
# {:ok, #DateTime<2016-05-24 13:26:08.003Z>}
DateTime.from_naive(~N[2016-05-24 13:26:08.003], "Etc/UTC")

DateTime.now("Europe/Copenhagen", Tzdata.TimeZoneDatabase)
