import urllib
import time
from geopy.geocoders import GoogleV3
import geojson

geolocator = GoogleV3()

url = "https://raw.githubusercontent.com/mcriqui/DDD/master/final_with_location.csv"
webpage = urllib.urlopen(url)
episodes = webpage.read().split('\n')
resources = []

for index, episode in enumerate(episodes):
    episodes[index] = episodes[index].split(',')
    time.sleep(.25)
    season = episodes[index][0]
    title = episodes[index][1]
    restaurant = episodes[index][2]
    city = episodes[index][3].strip()
    state = episodes[index][4].strip()
    business_id = episodes[index][5]
    rating = episodes[index][6]
    number_of_reviews = episodes[index][7]
    longitude = episodes[index][11]
    latitude = episodes[index][12]

    #if ((city=='Washington' and state=='DC') or (city=='Arlington' and state=='VA') or (city=="Falls Church" and state=='VA') or (city=="Silver Spring" and state=="MD")):
    if city in ('Washington', 'Arlington','Falls Church','Silver Spring') and state in ('DC','Virginia','Maryland'):
        item = {
            "type": "Feature",
            "geometry": {
                "type": "Point",
                "coordinates": [
                    longitude,
                    latitude
                ]
            },
            "properties": {
                "marker-symbol": "restaurant",
                "marker-color": "#00CC99",
                "marker-size":"small",
                "name": restaurant,
                "rating": rating,
                "season": season,
                "episode title": title,
                "city": city,
                "state": state,
            }
        }
        resources.append(item)

print resources

geo = {
    "type": "FeatureCollection",
    "features": resources
}

with open("ddd-dmv.geosjon", 'w') as outfile:
     geojson.dump(geo, outfile, indent=4, sort_keys=True)
