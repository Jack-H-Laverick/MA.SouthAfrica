using Parquet
using DataFrames
import GeoDataFrames as GDF
import GeoFormatTypes as GFT
import GeometryOps as GO
import GeoInterface as GI
using FLoops
using Proj

function point_polygon_intersection(points, polygon)
    intersection = Vector{Union{Bool, Missing}}(missing, size(points, 1))

    @floop for (p, point) in enumerate(points)
        intersection[p] = GO.intersects(point, polygon)
    end

    return Bool.(intersection)
end

domains = GDF.read("./Objects/Domains.gpkg")
domains.geometry = GO.reproject(domains.geom; source_crs = GFT.EPSG(9822), target_crs = GFT.EPSG(4326))
domain_poly = first(GO.union(domains.geometry...; target = GI.PolygonTrait()))

# Filter the fleet daily data
fleet_daily = DataFrame(read_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/fleet-daily.parq"))
fleet_daily.geometry = [GI.Point(coord; crs = GFT.EPSG(4326)) for coord in tuple.(fleet_daily.cell_ll_lon, fleet_daily.cell_ll_lat)]
fleet_daily.intersection = point_polygon_intersection(fleet_daily.geometry, domain_poly)

fleet_daily_domain = fleet_daily[fleet_daily.intersection, Not(:intersection, :geometry)]
write_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/fleet-daily-domain.parq", fleet_daily_domain)

# Filter the mmsi daily data
mmsi_daily = DataFrame(read_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/mmsi-daily.parq"))
mmsi_daily.geometry = [GI.Point(coord; crs = GFT.EPSG(4326)) for coord in tuple.(mmsi_daily.cell_ll_lon, mmsi_daily.cell_ll_lat)]
mmsi_daily.intersection = point_polygon_intersection(mmsi_daily.geometry, domain_poly)

mmsi_daily_domain = mmsi_daily[mmsi_daily.intersection, Not(:intersection, :geometry)]
write_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/mmsi-daily-domain.parq", mmsi_daily_domain)

# Filter the fishing monthly data
fleet_monthly = DataFrame(read_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/fleet-monthly.parq"))
fleet_monthly.geometry = [GI.Point(coord; crs = GFT.EPSG(4326)) for coord in tuple.(fleet_monthly.cell_ll_lon, fleet_monthly.cell_ll_lat)]
fleet_monthly.intersection = point_polygon_intersection(fleet_monthly.geometry, domain_poly)

fleet_monthly_domain = fleet_monthly[fleet_monthly.intersection, Not(:intersection, :geometry)]
write_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/fleet-monthly-domain.parq", fleet_monthly_domain)
