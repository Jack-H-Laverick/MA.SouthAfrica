"""
Filter Global Fishing Watch files to cells that are within the domain and Seas Around Us
regions to speed up processing of data to regional NetCDFs. This spatial filtering is
performed in Julia for increased speed compared to R's sf package.
"""

using Parquet
using DataFrames
import GeoDataFrames as GDF
import GeoFormatTypes as GFT
import GeometryOps as GO
import GeoInterface as GI
import ArchGDAL as AG
using FLoops
using Proj

function point_polygon_intersection(points_df, polygon)
    coordinates = vcat(vcat(GI.coordinates(polygon)...)...)
    min_x = minimum(first.(coordinates))
    max_x = maximum(first.(coordinates))
    min_y = minimum(last.(coordinates))
    max_y = maximum(last.(coordinates))

    points_df = points_df[
        (points_df.cell_ll_lon .> min_x) .&
        (points_df.cell_ll_lon .< max_x) .&
        (points_df.cell_ll_lat .> min_y) .&
        (points_df.cell_ll_lat .< max_y),
    :]

    points = points_df.geometry

    # intersection = Vector{Union{Bool, Missing}}(missing, size(points, 1))

    # for (p, point) in enumerate(points)
    #     intersection[p] = AG.intersects(point, polygon)
    # end

    return points_df[AG.intersects.(points, [polygon]), :]
end

domains = GDF.read("./Objects/domains.gpkg")
# domains.geometry = GO.reproject(domains.geom; source_crs = GFT.EPSG(4222), target_crs = GFT.EPSG(4326))
domain_poly = first(GO.union(domains.geom...; target = GI.PolygonTrait()))

sau_area_poly = GDF.read("./Objects/sau_atlantic_cape_region.gpkg")
sau_area_poly = first(sau_area_poly.geom)

fleet_daily = DataFrame(read_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/fleet-daily.parq"))
# fleet_daily.geometry = [GI.Point(coord; crs = GFT.EPSG(4326)) for coord in tuple.(fleet_daily.cell_ll_lon, fleet_daily.cell_ll_lat)]
fleet_daily.geometry = AG.createpoint.(tuple.(fleet_daily.cell_ll_lon, fleet_daily.cell_ll_lat))

mmsi_daily = DataFrame(read_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/mmsi-daily.parq"))
mmsi_daily.geometry = AG.createpoint.(tuple.(mmsi_daily.cell_ll_lon, mmsi_daily.cell_ll_lat))

fleet_monthly = DataFrame(read_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/fleet-monthly.parq"))
fleet_monthly.geometry = AG.createpoint.(tuple.(fleet_monthly.cell_ll_lon, fleet_monthly.cell_ll_lat))

# Filter the fleet daily data to the domain region
fleet_daily_domain = point_polygon_intersection(fleet_daily_domain, domain_poly)
write_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/fleet-daily-domain.parq", fleet_daily_domain[:, Not(:geometry)])

# Filter the fleet daily data to the sau_area region
# fleet_daily.sau_area_intersection = point_polygon_intersection(fleet_daily.geometry, sau_area_poly)
fleet_daily_sau_area = point_polygon_intersection(fleet_daily, sau_area_poly)
write_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/fleet-daily-sau_area.parq", fleet_daily_sau_area[:, Not(:geometry)])

fleet_daily = nothing
GC.gc()

# Filter the mmsi daily data to the domain region
mmsi_daily_domain = point_polygon_intersection(mmsi_daily, domain_poly)
write_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/mmsi-daily-domain.parq", mmsi_daily_domain[:, Not(:geometry)])

# Filter the mmsi daily data to the sau_area region
mmsi_daily_sau_area = point_polygon_intersection(mmsi_daily, sau_area_poly)
write_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/mmsi-daily-sau_area.parq", mmsi_daily_sau_area[:, Not(:geometry)])

mmsi_daily = nothing
GC.gc()

# Filter the fishing monthly data to the domain region
fleet_monthly_domain = point_polygon_intersection(fleet_monthly, domain_poly)
write_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/fleet-monthly-domain.parq", fleet_monthly_domain[:, Not(:geometry)])

# Filter the fishing monthly data to the sau_area region
fleet_monthly_sau_area = point_polygon_intersection(fleet_monthly, sau_area_poly)
write_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/fleet-monthly-sau_area.parq", fleet_monthly_sau_area[:, Not(:geometry)])

fleet_monthly = nothing
GC.gc()