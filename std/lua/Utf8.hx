package lua;
#if (lua_ver > 5.3)
@native("_G.utf8")
#else
@:luaRequire("lua-utf8")
#end
extern class  Utf8 {
	public static function escape(str:String) : String;
	public static function charpos(str:String, charpos:Int, offset:Int) : Int; //and Int
	public static function next(str:String, charpos:Int, offset:Int) : Int; //and Int
	public static function insert(str:String, idx:Int, substring:String) : String;
	public static function remove(str:String, start:Int, stop:Int) : String;
	public static function width(str:String, ambi_is_double:Bool, default_width:Int) : Int;
	public static function widthindex(str:String, location:Int, ambi_is_double:Bool, default_width:Int) : Int;
	public static function title(str:String) : String;
	public static function fold(str:String) : String;
	public static function nbasecmp(a:String, b:String) : Table<Int,Int>;
	public static function byte(str:String, pos:Int) : Int;
	public static function char(codes :haxe.extern.Rest<Int>) : String;
	public static function len(str:String) : Int;
	// public static function find() : String;
	// public static function gmatch() : String;
	// public static function gsub() : String;
	// public static function len() : String;
	// public static function lower() : String;
	// public static function match() : String;
	// public static function reverse() : String;
	public static function sub(str:String, start:Int, end:Int) : String;
	// public static function upper() : String;
}
