package lua;
@:native("_G.string") 
extern class StringTools {
	public static function len(str:String) : Int;
	public static function sub(str:String, start:Int, end:Int) : String;
	public static function charCodeAt(str:String, index:Int) : Int;
	public static function find(str:String, target:String, start:Int, end :Int, plain:Bool) : Int;
	public static function byte(str:String, index:Int) : Int;

	@:overload(function(pos:Int):String{})
	public static function substr(str:String, pos:Int, len:Int) : String;
	//TODO: The rest
}
