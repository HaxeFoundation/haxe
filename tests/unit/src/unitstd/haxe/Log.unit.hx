var s = "";
var p:haxe.PosInfos = null;
var old = haxe.Log.trace;
haxe.Log.trace = function(v, ?i) {
	s = v;
	p = i;
}
trace("test trace");
haxe.Log.trace = old;
s == "test trace";
p.fileName == "src/unitstd/haxe/Log.unit.hx";
p.lineNumber == 8;
haxe.Log.trace = null;
exc(function() trace("exc test"));
haxe.Log.trace = old;
