#!/usr/bin/env python
# coding: utf-8

from bs4 import BeautifulSoup
from urllib.parse import urlencode
from urllib.request import Request, urlopen
import socket
import sys

url = 'http://www.romajidesu.com/translator' # Set destination URL here
post_fields = {'m': 'converters', 'a':'kanji_romaji', 'k': "".join(sys.argv[1:])}     # Set POST fields here

try:
    request = Request(url, urlencode(post_fields).encode())
    result = urlopen(request,timeout=10).read().decode()
except:
    sys.exit(1)

soup = BeautifulSoup(result,features="lxml")

x = soup.find(id="japanese_input")
kanji = x.string.replace(' ','')

x = soup.find(id="res_kana")
out = []
types = []
for c in list(x.children):
    if c.name == "span":
        types.append(c['class'][0])
        out.append(c.string)
kana = "".join(out)

x = soup.find(id="res_romaji")
out = []
for c in list(x.children):
    if c.name == "span":
        out.append(c.string)
romaji = " ".join(out).replace("｡",".")

replace_chars = [("( ","("),(" )",")"),("[ ","["),(" ]","]"),(" ?","?"),(" ｡","｡"),(" .","."),(" !","!")]
for search,replace in replace_chars:
    kana = kana.replace(search,replace)
    romaji = romaji.replace(search,replace)

print("%s\t%s\t%s" % (kanji, kana, romaji))
print(types, file=sys.stderr)
