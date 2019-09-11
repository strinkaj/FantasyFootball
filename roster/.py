
import numpy as np
import pandas as pd

roster = pd.read_csv('input/.csv',delimiter = ',').T

p1 = pd.read_csv('../input/p1.csv',delimiter = ',')

p1 = p1[p1.avg_type == "weighted"]

p1 = p1[['id','first_name','last_name','team','position','floor','points','ceiling']]

roster['id'] = 0

roster.astype({'id': 'int64'}).dtypes

roster.id[roster.index.str.startswith('Player(Ezekiel')] = 12625
roster.id[roster.index.str.startswith('Player(Travis')] = 11244
roster.id[roster.index.str.startswith('Player(Derrick')] = 12626
roster.id[roster.index.str.startswith('Player(Robert')] = 11228
roster.id[roster.index.str.startswith('Player(Jordan')] = 12634
roster.id[roster.index.str.startswith('Player(Deshaun')] = 13113
roster.id[roster.index.str.startswith('Player(Damien')] = 11886
roster.id[roster.index.str.startswith('Player(Sterling')] = 12658
roster.id[roster.index.str.startswith('Player(Sammy')] = 11670
roster.id[roster.index.str.startswith('Player(Dante')] = 13634
roster.id[roster.index.str.startswith('Player(Courtland')] = 13630
roster.id[roster.index.str.startswith('Player(Jason')] = 6997
roster.id[roster.index.str.startswith('Player(James')] = 13631
roster.id[roster.index.str.startswith('Player(DeSean')] = 9075
roster.id[roster.index.str.startswith('Player(Chiefs')] = 512

roster = pd.merge(roster,p1, on = 'id', how = 'left')

agg = roster.groupby(["position"]).agg({'floor':'min','ceiling':'max'})

agg['roster_pos'] = 1

roster = pd.merge(roster,agg[['ceiling','roster_pos']], on = 'ceiling', how = 'left')

agg = roster[roster.roster_pos != 1].groupby(["position"]).agg({'floor':'min','ceiling':'max'})

agg['roster_pos'] = 2

roster = pd.merge(roster,agg[['ceiling','roster_pos']], on = 'ceiling', how = 'left')