var map1 = new haxe.ds.IntMap();
(map1 is haxe.ds.IntMap) == true;
map1.set(1, 2);
map1.set(2, 4);
map1.set(3, 6);
map1.get(1) == 2;
map1.get(2) == 4; 
map1.get(3) != 8;
// iterator
[for (k in map1.keys()) k] == [1,2,3];
[for (v in map1) v] == [2,4,6];
// key value iterator
[for (k=>v in map1) k] == [1,2,3];
[for (k=>v in map1) v] == [2,4,6];
[for (k=>v in map1) k*v] == [2,8,18];


var map2 = new haxe.ds.IntMap();
(map2 is haxe.ds.IntMap) == true;
map2.set(1, "2");
map2.set(2, "4");
map2.set(3, "6");
map2.get(1) == "2";
map2.get(2) == "4"; 
map2.get(3) != "8";
// iterator
[for (k in map2.keys()) k] == [1,2,3];
[for (v in map2) v] == ["2","4","6"];
// key value iterator
[for (k=>v in map2) k] == [1,2,3];
[for (k=>v in map2) v] == ["2","4","6"];