import numpy as np
import xarray as xr

def era5_modifier(data, output_name):
    z = xr.open_dataset(data)
    var_mod = np.nansum(z.tp, axis = 1)
    ds = xr.Dataset({
    "tp": (["time", "latitude", "longitude"], var_mod)
    },
    coords = {
        "longitude": (["longitude"], z.longitude),
        "latitude": (["latitude"], z.latitude),
        "time": (["time"], z.time)
    }
    )
    ds.attrs["Conventions"] = z.Conventions
    ds.attrs["history"] = z.history
    ds.longitude.attrs = z.longitude.attrs
    ds.latitude.attrs = z.latitude.attrs
    ds.time.attrs = z.time.attrs
    ds.tp.attrs = z.tp.attrs
    
    ds.to_netcdf(output_name)