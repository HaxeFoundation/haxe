var s = "";
var p:haxe.PosInfos = null;
var old = haxe.Log.trace;
haxe.Log.trace = function(v, ?i) {
	s = v;
	p = i;
}
trace("test trace");
s == "test trace";
p.fileName == "Log.unit.hx";
p.lineNumber == 8;
haxe.Log.trace = null;
exc(function() trace("exc test"));
haxe.Log.trace = old;