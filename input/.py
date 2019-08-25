
from yahoo_oauth import OAuth2

from yahoo_fantasy_api import league, game, team

sc = OAuth2(None, None, from_file='oauth2.json')

gm = game.Game(sc, 'nfl')

gm.league_ids(year=2019)

lg = gm.to_league('390.l.642807')

lg.current_week()

lg.teams()