package cs.system.text;

@:native("System.Text.StringBuilder")
extern class StringBuilder {
	var Length(default, never):Int;

	function new():Void;

	@:overload(function(value:String):StringBuilder {})
	@:overload(function(value:String, startIndex:Int, length:Int):StringBuilder {})
	function Append(value:Dynamic):StringBuilder;

	function ToString():String;
}
