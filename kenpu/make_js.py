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
out = open("imdb-movie-genres.js", "w")

print >>out, """
/* --------------- IMDB MOVIE-GENRE --------------- */
"""

out.write("var IMDB_MOVIE_GENRES='")
s = stream()
for id, title, genre in s:
    out.write("%s|%s|%s|" % (id, title, genre))
out.write("';\n");

out.close()
