import sys
import itertools

def stream():
    with open("movie_genres.csv") as f:
        # ignore the first two lines
        f.readline()
        f.readline()
        for line in f.xreadlines():
            try:
                id, title, genre = line.strip().split("|")
                yield (id, 
                        title.strip().replace("'", ""),
                        genre.strip().replace("'", ""))
            except ValueError:
                pass


n = 0
out = open("imdb.html", "w")

print >>out, """
<html>
<script>
"""

out.write("var x='")
s = stream()
for id, title, genre in s:
    out.write("%s|%s|%s|" % (id, title, genre))
out.write("';\n");

print >>out, """
</script>
<body>
Loaded... wow.
</body>
<script>
console.debug(x.length);
var y = x.split("|");
console.debug(y.length / 3);
</script>
</html>
"""

out.close()
