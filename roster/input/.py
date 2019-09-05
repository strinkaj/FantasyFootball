
from ff_espn_api import League
import csv

league_id = 139368
year = 2019
swid = '{F0131450-8769-4C15-8632-A180B8DE2B54}'
espn_s2 = 'AEB6vEM8nQjtW3JcDG45m3sHbHe6gKiIXCtHfAZKBQGTajnVwqaHkevL25KF9Pv39b9PckyYeSv7SVIOTiSw9ZG9OEdcp02Z40GhAYFlaNBTJ8zigRwd%2FH4Gab7xN5emFyA5UzUeq%2ByKReJ0V5oA35ojWVcRTcur2smW0Qx%2BH4ugKgr2Wv5oZrTDSzo3B2atS4lAgrayRZ7QJBmeoJGOizRI4bOI%2BbhqtjSnBDa9oXefg5DdBxh57mtFnl1FZyAp3kmkDaBkhJOdUvryQj%2BvYsoX'
league = League(league_id, year, espn_s2, swid)
team = league.teams[2]
roster = team.roster()

with open('.csv', mode='w') as d:
    d_writer = csv.writer(d, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    d_writer.writerow(roster)