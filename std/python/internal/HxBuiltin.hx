package python.internal;

@:pythonImport("builtins") extern class HxBuiltin
{
	public static function bool(x:Dynamic):Bool;

	public static function issubclass(x:Class<Dynamic>, from:Class<Dynamic>):Bool;
	public static function callable(x:Dynamic):Bool;

	public static function isinstance(obj:Dynamic, cl:Dynamic):Bool;

	public static function hasattr(obj:Dynamic, attr:String):Bool;
	public static function getattr(obj:Dynamic, attr:String):Dynamic;

	public static function delattr(o:Dynamic, attr:String):Void;
	public static function setattr(o:Dynamic, attr:String, val:Dynamic):Void;

	public static function len(x:Dynamic):Int;
	public static function int(x:Dynamic):Int;
	public static function ord(s:String):Int;

	public static function map (a:Dynamic, b:Dynamic):Dynamic;
	public static function str(o:Dynamic):String;

	public static function float(x:Dynamic):Float;

	public static function list<T>(i:Dynamic):Array<T>;

	public static function chr(c:Int):String;

	@:overload(function (a1:Float, a2:Float, ?a3:Float, ?a4:Float, ?a5:Float, ?a6:Float, ?a7:Float, ?a8:Float, ?a9:Float):Float {})
	public static function max(a1:Int, a2:Int, ?a3:Int, ?a4:Int, ?a5:Int, ?a6:Int, ?a7:Int, ?a8:Int, ?a9:Int):Int;

	public static function round(f:Float):Int;

	@:overload(function (a1:Float, a2:Float, ?a3:Float, ?a4:Float, ?a5:Float, ?a6:Float, ?a7:Float, ?a8:Float, ?a9:Float):Float {})
	public static function min(a1:Int, a2:Int, ?a3:Int, ?a4:Int, ?a5:Int, ?a6:Int, ?a7:Int, ?a8:Int, ?a9:Int):Int;
}