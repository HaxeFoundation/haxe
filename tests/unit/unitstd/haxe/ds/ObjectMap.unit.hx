var k1 = new IntWrap(1);
var k2 = new IntWrap(2);
var k3 = new IntWrap(3);
var o = new haxe.ds.ObjectMap();

// non existent
o.exists(k1) == false;
o.exists(k2) == false;
o.exists(k3) == false;
o.get(k1) == null;
o.get(k2) == null;
o.get(k3) == null;

// set + exists
o.set(k1, "9");
o.set(k2, "8");
o.set(k3, "7");
o.exists(k1) == true;
o.exists(k2) == true;
o.exists(k3) == true;

// get
o.get(k3) == "7";
o.get(k2) == "8";
o.get(k1) == "9";

// keys
var a = [];
for (k in o.keys())
	a.push(k);
a.length == 3;
a[0] in [k1, k2, k3];
a[1] in [k1, k2, k3];
a[2] in [k1, k2, k3];
o.exists(k1) == true;
o.exists(k2) == true;
o.exists(k3) == true;
o.get(k3) == "7";
o.get(k2) == "8";
o.get(k1) == "9";

// iterator
var a:Array<String> = [];
for (k in o) {
	a.push(k);
}
a.length == 3;
a[0] in ["9", "8", "7"];
a[1] in ["9", "8", "7"];
a[2] in ["9", "8", "7"];
o.exists(k1) == true;
o.exists(k2) == true;
o.exists(k3) == true;
o.get(k3) == "7";
o.get(k2) == "8";
o.get(k1) == "9";

// remove
o.remove(k2) == true;
o.exists(k1) == true;
o.exists(k2) == false;
o.exists(k3) == true;
o.get(k1) == "9";
o.get(k2) == null;
o.get(k3) == "7";
var a = [];
for (k in o.keys())
	a.push(k);
a.length == 2;
a[0] in [k1, k3];
a[1] in [k1, k3];
var a:Array<String> = [];
for (k in o.iterator()) {
	a.push(k);
}
a.length == 2;
a[0] in ["9", "7"];
a[1] in ["9", "7"];
o.remove(k2) == false;