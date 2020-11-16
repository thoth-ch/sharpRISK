from pathlib import Path
import sqlite3

tridel_path = "tridel.db"
Path(tridel_path).exists()

risk_conn = sqlite3.connect(tridel_path)
c = risk_conn.cursor()

action_responsibles_py = c.execute('SELECT action_responsible FROM actions')
action_responsibles_py = c.fetchall()

risk_conn.close()