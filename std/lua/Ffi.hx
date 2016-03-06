package lua;

#if lua_jit
@:native("_G.ffi")
extern class Ffi {

	// Declaring and accessing external symbols
	public static var C : Dynamic;
	public static function gc(cdata : Dynamic, finalizer : Function) : Void;
	public static function load : (name : String, ?global : Bool) : Dynamic;
	public static function metatype<T>(ct : Ctype<T>, metatable : Table<Dynamic>) : Ctype<T>;
	public static function typeof(str:String) : Ctype<Dynamic>;

	// C Type functionality 
	public static function alignof(ct : Ctype<T>) : Int;
	public static function istype(ct : Ctype<T>, obj:Dynamic) : Bool;
	public static function offsetof(ct : Ctype<T>, field:String) : Int;
	public static function sizeof(ct : Ctype<T>, ?nelem : Int) : Int;


	// Utility functionality
	public static function errno(?newerr : Int) : Int;
	public static function fill(dst : Dynamic, len : Int, c:Int) : Void; 
	public static function string(ptr : Dynamic, ?len : Int) : String;

	@:overload(   function     (dst : Dynamic, str : String)             : Dynamic {})
	public static function copy(dst : Dynamic, src : Dynamic, len : Int) : String;

	// Target specific functionality
	public static var os : String;
	public static var arch : String;
	public static function abi(param : String) : Bool; 
}

extern class Ctype<T> {}

// TODO FFI callback type (gc methods)
#end

