import pysrt
import sys
import re

f = pysrt.open(sys.argv[1])
line = "\n".join([t.text.replace("\n","") for t in f])
line = re.sub(r"[➡→≪＜＞》《 　“”]+","",line)
for search,replace in [("？","?"),("！","!"),("。","｡"),("（","("),("）",")"),("～","~"),("「","["),("」","]"),("『","["),("』","]")]:
    line = line.replace(search,replace)
print(line)
