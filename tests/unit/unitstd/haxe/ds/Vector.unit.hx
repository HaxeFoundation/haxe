var vec = new haxe.ds.Vector(3);
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

// float init
var vec = new haxe.ds.Vector<Float>(3);
vec.get(0) == vNullFloat;
vec.get(1) == vNullFloat;
vec.get(2) == vNullFloat;

// bool init
// Adobe's compilers seem to have a bug here that gives null instead of false
#if !as3
var vec = new haxe.ds.Vector<Bool>(3);
vec.get(0) == vNullBool;
vec.get(1) == vNullBool;
vec.get(2) == vNullBool;
#end

// fromArray
var arr = ["1", "2", "3"];
var vec:haxe.ds.Vector<String> = haxe.ds.Vector.fromArrayCopy(arr);
#if (!flash && !neko)
arr != vec.toData();
#end
vec.length() == 3;
vec.get(0) == "1";
vec.get(1) == "2";
vec.get(2) == "3";

// objects
var tpl = new C();
var vec:haxe.ds.Vector<C> = haxe.ds.Vector.fromArrayCopy([tpl]);
tpl == vec.get(0);

// toData + fromData
var vec:haxe.ds.Vector<String> = haxe.ds.Vector.fromArrayCopy(["1", "2", "3"]);
var data = vec.toData();
var vec2 = haxe.ds.Vector.fromData(data);
vec2.get(0) == "1";
vec2.get(1) == "2";
vec2.get(2) == "3";

// []
vec2[0] == "1";
vec2[1] == "2";
vec2[2] == "3";
vec2[1] = "4";
vec2[1] == "4";
vec2[0] += "a";
vec2[0] = "1a";