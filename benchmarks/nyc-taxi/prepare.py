#!/usr/bin/env python3
"""Download and prepare the NYC taxi graph for the Scalaxy benchmark.

Sources (NYC TLC, public domain / NYC Open Data):
  * taxi zone lookup:  https://d37ci6vzurychx.cloudfront.net/misc/taxi+_zone_lookup.csv
  * yellow taxi trips: https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_YYYY-MM.parquet

Requires: pandas + pyarrow.  Outputs (next to this script):
  zones.csv             263 taxi zones (id, borough, zone, service_zone)
  trips.csv             per-trip rows (pickup, dropoff, distance, fare, passengers)
  trips_aggregated.csv  zone-pair aggregated rows (pickup, dropoff, trips,
                        distance, fare, passengers)

Usage:
  python3 prepare.py [YYYY-MM]     # default 2024-01
"""
import sys, os
import urllib.request

import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
MONTH = sys.argv[1] if len(sys.argv) > 1 else "2024-01"
ZONES_URL = "https://d37ci6vzurychx.cloudfront.net/misc/taxi+_zone_lookup.csv"
TRIPS_URL = f"https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_{MONTH}.parquet"


def download(url, dest):
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=900) as r, open(dest, "wb") as f:
        while True:
            chunk = r.read(1 << 20)
            if not chunk:
                break
            f.write(chunk)
    return dest


def main():
    data_dir = os.path.join(HERE, "data")
    os.makedirs(data_dir, exist_ok=True)

    zones_raw = download(ZONES_URL, os.path.join(data_dir, "taxi_zone_lookup.csv"))
    parquet = download(TRIPS_URL, os.path.join(data_dir, f"yellow_tripdata_{MONTH}.parquet"))

    zones = pd.read_csv(zones_raw)
    zones = zones[zones.LocationID <= 263][["LocationID", "Borough", "Zone", "service_zone"]]
    zones.columns = ["id", "borough", "zone", "service_zone"]
    zones.to_csv(os.path.join(HERE, "zones.csv"), index=False)

    import pyarrow.parquet as pq
    table = pq.read_table(parquet, columns=["PULocationID", "DOLocationID",
                                            "trip_distance", "fare_amount",
                                            "passenger_count"])
    pdf = table.to_pandas()
    pdf = pdf[(pdf.PULocationID >= 1) & (pdf.PULocationID <= 263) &
              (pdf.DOLocationID >= 1) & (pdf.DOLocationID <= 263)]
    trips = pd.DataFrame({
        "pickup": pdf.PULocationID,
        "dropoff": pdf.DOLocationID,
        "distance": pdf.trip_distance.round(2),
        "fare": pdf.fare_amount.round(2),
        "passengers": pdf.passenger_count.fillna(0).astype(int),
    })
    trips.to_csv(os.path.join(HERE, "trips.csv"), index=False)

    agg = trips.groupby(["pickup", "dropoff"], as_index=False).agg(
        trips=("pickup", "size"),
        distance=("distance", "sum"),
        fare=("fare", "sum"),
        passengers=("passengers", "sum"),
    ).round({"distance": 2, "fare": 2})
    agg.to_csv(os.path.join(HERE, "trips_aggregated.csv"), index=False)

    print(f"zones: {len(zones)}")
    print(f"trips: {len(trips)}")
    print(f"aggregated edges: {len(agg)}")


if __name__ == "__main__":
    main()
