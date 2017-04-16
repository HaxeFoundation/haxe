var map = new haxe.ds.DynamicStruct();
map.exists("foo") == false;
map.get("foo") == null;
map.set("foo", 1);
map.set("bar", 2);
map["baz"] = 3;
map.exists("foo") == true;
map.exists("bar") == true;
map.exists("baz") == true;
map.get("foo") == 1;
map.get("bar") == 2;
map["baz"] == 3;
var keys = ["foo", "bar", "baz"];
for (key in map.keys()) {
    t(keys.remove(key));
}
keys == [];
map.remove("bar") == true;
map.remove("bar") == false;
map.exists("foo") == true;
map.exists("bar") == false;
map.exists("baz") == true;
map.get("bar") == null;

var map2:haxe.ds.DynamicStruct<Int> = {k: 5};
map2["k"] == 5;
