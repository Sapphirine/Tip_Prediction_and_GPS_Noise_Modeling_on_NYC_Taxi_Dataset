-- Custom queries
CREATE TABLE my_geom (
  name varchar
);
SELECT AddGeometryColumn('my_geom', 'geom', 4326, 'POLYGON', 2);
-- text AddGeometryColumn(
--         varchar table_name,
--        varchar column_name,
--               integer srid,
--               varchar type,
--          integer dimension,
--    boolean use_typmod=true);

-- NB: For
-- gs_conrad_nea: Goldman Sachs Conrad (North East Avenue)
--                Locate Entrance by Google Street Map
--                Surround the entrance with large margin (almost reaching River Terrace)
--
--
INSERT INTO my_geom
VALUES

('gs_conrad_nea', ST_GeomFromText(
 'POLYGON((
   -74.015453 40.715262,
   -74.016180 40.714988,
   -74.015816 40.714690,
           ))', 4326)),
('Citigroup', ST_GeomFromText('POLYGON((-74.011869 40.7217236,
                                        -74.009867 40.721493,
                                        -74.010140 40.720053,
                                        -74.012083 40.720267,
                                        -74.011869 40.7217236))', 4326));

CREATE INDEX index_custom_geoms ON custom_geometries USING gist (geom);

