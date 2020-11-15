from pathlib import Path
import pandas as pd
import sqlite3

tridel_path = "tridel.db"
Path(tridel_path).exists()

risk_conn = sqlite3.connect(tridel_path)
c = risk_conn.cursor()

risks_py1 = c.execute('SELECT * FROM risks')
risks_py2 = c.fetchall()
risks_py3 = c.fetchone()

risks_py4 = c.execute('SELECT risk_name FROM risks')
risks_py4 = c.fetchall()
