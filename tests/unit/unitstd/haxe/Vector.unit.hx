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

// float init
var vec = new haxe.Vector<Float>(3);
vec.get(0) == vNullFloat;
vec.get(1) == vNullFloat;
vec.get(2) == vNullFloat;

// bool init
var vec = new haxe.Vector<Bool>(3);
vec.get(0) == vNullBool;
vec.get(1) == vNullBool;
vec.get(2) == vNullBool;

// fromArray
var arr = ["1", "2", "3"];
var vec:haxe.Vector<String> = haxe.Vector.fromArrayCopy(arr);
#if (!flash && !neko)
arr != vec.toData();
#end
vec.length() == 3;
vec.get(0) == "1";
vec.get(1) == "2";
vec.get(2) == "3";

// objects
var tpl = new haxe.Template("foo");
var vec:haxe.Vector<haxe.Template> = haxe.Vector.fromArrayCopy([tpl]);
tpl == vec.get(0);

// toData + fromData
var vec:haxe.Vector<String> = haxe.Vector.fromArrayCopy(["1", "2", "3"]);
var data = vec.toData();
var vec2 = haxe.Vector.fromData(data);
vec2.get(0) == "1";
vec2.get(1) == "2";
vec2.get(2) == "3";