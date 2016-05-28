import csv
from collections import defaultdict

users = list()
edges = defaultdict(list)

with open('users.csv', 'rb') as usersfile:
    usersr = csv.DictReader(usersfile)
    for row in usersr:
        users.append(int(row["id"]))

with open('edges.csv', 'rb') as edgesfile:
    edgesr = csv.DictReader(edgesfile)
    for row in edgesr:
        edges[int(row["V1"])].append(int(row["V2"]))

print("users", len(users))
print("edges", len(edges.items()))
      
for (k,v) in edges.iteritems():
    print(k, v, len(v))

nofriends = [user for user in users if not edges.has_key(user)]
print("no friends", nofriends, len(nofriends))

print("average number of friendzs", sum([len(v) for (k,v) in edges.items()]) / (float(len(users)) / 2))
# 4.94
