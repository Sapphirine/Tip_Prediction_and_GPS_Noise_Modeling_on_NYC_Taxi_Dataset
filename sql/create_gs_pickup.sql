CREATE TABLE gs_pickup AS
SELECT trips.*
FROM trips, custom_geometries
WHERE
  custom_geometries.name = 'Goldman Sachs'
  AND pickup_nyct2010_gid = 2100
  AND ST_Within(pickup, custom_geometries.geom);
