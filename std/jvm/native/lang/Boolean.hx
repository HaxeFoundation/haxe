package jvm.native.lang;

/**
 * ...
 * @author waneck
 */

extern class Boolean 
{
	static var FALSE(default, null):Boolean;
	static var TRUE(default, null):Boolean;
	
	
	@:overload(function(s:String):Void {})
	public function new(value:Bool):Void;
	
	function booleanValue():Bool;
}