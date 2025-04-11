import pandas as pd

# Carga los datos de los 4 años
df_2003 = pd.read_csv('enemdu_2003.csv')
df_2004 = pd.read_csv('enemdu_2004.csv')
df_2005 = pd.read_csv('enemdu_2005.csv')
df_2006 = pd.read_csv('enemdu_2006.csv')

# Agrega el año a cada dataset
df_2003['year'] = 2003
df_2004['year'] = 2004
df_2005['year'] = 2005
df_2006['year'] = 2006

# Une todos en un solo DataFrame
df = pd.concat([df_2003, df_2004, df_2005, df_2006])
