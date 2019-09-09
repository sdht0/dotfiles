import string
import random
import sys

length = int(sys.argv[1]) if len(sys.argv) >= 2 else random.randint(27, 30)
if length == 0:
    print("Zero length password. Exiting")
    sys.exit(0)

sets = []

special_chars = sys.argv[2] if len(sys.argv) >= 3 else '!#$%&()*+,-./:;<=>?@[]^_`{|}~'
if len(special_chars) > 0:
    sets.append(special_chars)

upper_chars = sys.argv[3] if len(sys.argv) >= 4 else string.ascii_uppercase
if len(upper_chars) > 0:
    sets.append(upper_chars)

lower_chars = sys.argv[4] if len(sys.argv) >= 5 else string.ascii_lowercase
if len(lower_chars) > 0:
    sets.append(lower_chars)

digits = sys.argv[5] if len(sys.argv) >= 6 else string.digits
if len(digits) > 0:
    sets.append(digits)

all_chars = "".join(sets)

if len(all_chars) == 0:
    print("No character set defined")
    sys.exit(1)

password = []

from_each = 2 if (len(sets) * 2) <= length else 1
length_from_sets = len(sets) * from_each
if length_from_sets <= length:
    for s in sets:
        for i in range(from_each):
            password.append(random.choice(s))
else:
    length_from_sets = 0

for i in range(0, length - length_from_sets):
    password.append(random.choice(all_chars))

random.shuffle(password)
print(''.join(password))
