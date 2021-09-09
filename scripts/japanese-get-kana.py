import pykakasi
import sys

text = "".join(sys.argv[1:])

kakasi = pykakasi.kakasi()
result = kakasi.convert(text)

kana = []
romaji = []

for item in result:
    kana.append(item['hira'])
    romaji.append(item['hepburn'])

romajitxt = " ".join(romaji)
for (i,j) in [(" ha "," wa "),("ha "," wa "),(" ha"," wa "),(" wo "," o ")]:
    romajitxt = romajitxt.replace(i,j)

print("%s\t%s\t%s" % (text,"".join(kana),romajitxt))
