import pysrt
import sys
import re

f = pysrt.open(sys.argv[1])
line = "".join([t.text for t in f])
line = re.sub(r"[\n➡→≪＜＞》《 　]+","",line)
for search,replace in [("？","?"),("！","!"),("。","｡"),("（","("),("）",")"),("～","~"),("「","["),("」","]"),("『","["),("』","]")]:
    line = line.replace(search,replace)
line = re.sub(r"([!\.｡。?]+)","\g<1>\n",line)
line = re.sub(r"\)\(",")\n(",line)
line = re.sub(r"([^\n])\(","\g<1>\n(",line)
#line = re.sub(r"\]([^$])","]\n\g<1>",line)
line = re.sub(r"([!\.｡。?])\n]","\g<1>]",line)
line = re.sub(r"\n+","\n",line)
print(line)
