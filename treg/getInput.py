#!/usr/bin/python3
import fileinput
import sys
from urllib.request import build_opener

cookie = list(fileinput.input(sys.argv[1]))[0][:-1]

day = sys.argv[2]
url = "https://adventofcode.com/2019/day/{0}/input".format(day)

opener = build_opener()
opener.addheaders.append(('Cookie', 'session={0}'.format(cookie)))
response = opener.open(url)
print(response.read().decode("utf-8"))



