import json
with open("ricorico.json", "r") as f:
    for item in json.load(f):
        if "type" in item and item["type"] == "Plato":
            print(item["id"])
    # print([item["id"] for item in json.load(f) if item["type"] and item["type"] == "Plato"])
