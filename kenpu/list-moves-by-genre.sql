select
    M.movie_id,
    T.title,
    M.info
FROM
    movie_info M,
    title T
WHERE
    M.movie_id = T.id and
    M.info_type_id = 3 and
    T.title NOT LIKE '%|%'
;
