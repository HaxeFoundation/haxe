var vec = new haxe.Vector(3);
var vNullInt = #if (flash9 || cpp || java || cs) 0 #else null #end;
var vNullBool = #if (flash9 || cpp || java || cs) false #else null #end;
var vNullFloat = #if (flash9 || cpp || java || cs) 0.0 #else null #end;

vec.length() == 3;
vec.get(0) == vNullInt;
vec.get(1) == vNullInt;
vec.get(2) == vNullInt;
vec.set(1, 2);
vec.length() == 3;
vec.get(0) == vNullInt;
vec.get(1) == 2;
vec.get(2) == vNullInt;

// out of bounds
vec.get( -1) == null;
vec.get(4) == null;

// float init
var vec = new haxe.Vector<Float>(3);
vec.get( -1) == null;
vec.get(0) == vNullFloat;
vec.get(1) == vNullFloat;
vec.get(2) == vNullFloat;
vec.get(3) == null;

// bool init
var vec = new haxe.Vector<Bool>(3);
vec.get( -1) == null;
vec.get(0) == vNullBool;
vec.get(1) == vNullBool;
vec.get(2) == vNullBool;
vec.get(3) == null;

// fromArray
var vec:haxe.Vector<String> = ["1", "2", "3"];
vec.length() == 3;
vec.get(0) == "1";
vec.get(1) == "2";
vec.get(2) == "3";

// objects
var tpl = new haxe.Template("foo");
var vec:haxe.Vector<haxe.Template> = [tpl];
tpl == vec.get(0);