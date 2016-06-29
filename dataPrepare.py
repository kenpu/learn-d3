import sys
import json
import psycopg2 as pg
import psycopg2.extras

def write_to_json(data, title):
	with open(title, 'w') as f:
		for line in data:
			towrite,subdata = {},{}
			#subdata["title"] = line[1]
			#subdata["value"] = line[2]
			#towrite[line[0]] = subdata
			towrite[line[1]] = line[2]
			json.dump(towrite,f,sort_keys=True,indent=4,separators=(',',': '))
	print "{} written.".format(title)

def main():
	conn = pg.connect(database="imdb",user="postgres",password="postgres",host="localhost")
	csr = conn.cursor()
	titleQuery = "select info from info_type where id = {}".format(sys.argv[1])
	csr.execute(titleQuery)
	title = csr.fetchone()
	print(title)
	filename = "movies_"+ title[0] +".json"
	print(filename)
	query = "select movie_info.movie_id, title.title, movie_info.info from movie_info, title where movie_info.movie_id = title.id and info_type_id = {} order by title.title".format(sys.argv[1])
	csr.execute(query)
	data = csr.fetchall()
	write_to_json(data,filename)
	
	

if __name__ == '__main__':
	main()
