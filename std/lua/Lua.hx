package lua;
import haxe.extern.Rest;
import haxe.Constraints.Function;
import haxe.extern.Rest;

/**
  These are all global static methods within Lua.
 **/

@:native("_G")
extern class Lua {
	/**
		A global variable that holds a string containing the current interpreter
		version.
	**/
	public static var _VERSION : String;

	public static var arg : Table<Int, String>;

	/**
		Pushes onto the stack the metatable in the registry.
	**/
	public static function getmetatable(tbl:Table<Dynamic,Dynamic>): Table<Dynamic,Dynamic>;

	/**
		Pops a table from the stack and sets it as the new metatable for the value
		at the given acceptable index.
	**/
	public static function setmetatable(tbl:Table<Dynamic,Dynamic>, mtbl: Table<Dynamic, Dynamic>): Table<Dynamic, Dynamic>;

	/**
		Pops a table from the stack and sets it as the new environment for the value
		at the given index. If the value at the given index is neither a function nor
		a thread nor a userdata, lua_setfenv returns `0`.
		Otherwise it returns `1`.
	**/
	public static function setfenv(i:Int , tbl:Table<Dynamic, Dynamic>): Void;

	/**
		Allows a program to traverse all fields of a table.
		Its first argument is a table and its second argument is an index in this
		table. `next` returns the next index of the table and its associated value.
		When `i` is `null`, `next` returns an initial index and its associated value.
		When called with the last index, or with `null` in an empty table, `next`
		returns `null`.  In particular, you can use `next(t)` to check whether a
		table is empty.

		The order in which the indices are enumerated is not specified, even for
		numeric indices. (To traverse a table in numeric order, use a numerical for
		or the `ipairs` function).

		The behavior of next is undefined if, during the traversal, any value
		to a non-existent field in the table is assigned. Existing fields may
		however be modified. In particular, existing fields may be cleared.
	**/
	public static function next<K,V>(k:Table<K, V>, ?i : V): NextResult<K,V>;

	/**
		Receives an argument of any type and converts it to a string in a reasonable
		format.

		For complete control of how numbers are converted, use`NativeStringTools.format`.
	**/
	public static function tostring(v:Dynamic): String;

	public static function ipairs<T>(t:Table<Int,T>): PairsResult<Int,T>;

	public static function pairs<K,V>(t:Table<K,V>): PairsResult<K,V> ;

	public static function require(module:String) : Dynamic;

	/**
		Converts the Lua value at the given acceptable base to `Int`.
		The Lua value must be a number or a string convertible to a number,
		otherwise `tonumber` returns `0`.
	**/
	public static function tonumber(str:String, ?base:Int): Int;

	/**
		Returns the Lua type of its only argument as a string.
		The possible results of this function are:

		 * `"nil"` (a string, not the Lua value nil),
		 * `"number"`
		 * `"string"`
		 * `"boolean"`
		 * `"table"`
		 * `"function"`
		 * `"thread"`
		 * `"userdata"`
	**/
	public static function type(v:Dynamic) : String;

	/**
		Receives any number of arguments, and prints their values to stdout,
		using the tostring function to convert them to strings.
		`print` is not intended for formatted output, but only as a quick way to show
		a value, typically for debugging.

		For complete control of how numbers are converted, use `NativeStringTools.format`.
	**/
	public static function print(v:haxe.extern.Rest<Dynamic>) : Void;

	/**
		If `n` is a number, returns all arguments after argument number `n`.
		Otherwise, `n` must be the string `"#"`, and select returns the total
		number of extra arguments it received.
	**/
	public static function select(n:Dynamic, rest:Rest<Dynamic>) : Dynamic;

	/**
		Gets the real value of `table[index]`, without invoking any metamethod.
	**/
	public static function rawget<K,V>(t:Table<K,V>, k:K) : V;

	/**
		Sets the real value of `table[index]` to value, without invoking any metamethod.
	**/
	public static function rawset<K,V>(t:Table<K,V>, k:K, v:V) : Void;

	/**
		This function is a generic interface to the garbage collector.
		It performs different functions according to its first argument.
	**/
	public static function collectgarbage(opt:CollectGarbageOption, ?arg:Int) : Int;

	/**
		Issues an error when the value of its argument `v` is `false` (i.e., `null`
		or `false`) otherwise, returns all its arguments. message is an error message.
		when absent, it defaults to "assertion failed!"
	**/
	public static function assert<T>(v:T, ?message:String) : T;

	/**
		Loads and runs the given file.
	**/
	public static function dofile(filename:String) : Void;

	/**
		Generates a Lua error. The error message (which can actually be a Lua value
		of any type) must be on the stack top. This function does a long jump,
		and therefore never returns.
	**/
	public static function error(message:String, ?level:Int) : Void;

	/**
		Calls a function in protected mode.
	**/
	public static function pcall(f:Function, rest:Rest<Dynamic>) : PCallResult;

	/**
		Returns `true` if the two values in acceptable indices `v1` and `v2` are
		primitively equal (that is, without calling metamethods).
		Otherwise returns `false`.
		Also returns `false` if any of the indices are non valid.
	**/
	public static function rawequal(v1:Dynamic, v2:Dynamic) : Bool;

	/**
		This function is similar to pcall, except that you can set a new error
		handler.
	**/
	public static function xpcall(f:Function, msgh:Function, rest:Rest<Dynamic> ) : PCallResult;

	/**
		Loads the chunk from file filename or from the standard input if no filename
		is given.
	**/
	public static function loadfile(filename:String) : LoadResult;

	/**
		Loads the chunk from given string.
	**/
	public static function load(code:haxe.extern.EitherType<String,Void->String>) : LoadResult;

}

/**
	Enum for describing garbage collection options
**/
@:enum
abstract CollectGarbageOption(String) {
	var Stop = "stop";
	var Restart = "restart";
	var Collect = "collect";
	var Count = "count";
	var Step = "step";
	var SetPause = "setpause";
	var SetStepMul = "setstepmul";
}

@:multiReturn
extern class PCallResult {
	var status : Bool;
	var value : Dynamic;
}

@:multiReturn
extern class NextResult<K,V> {
	var index : K;
	var value : V;
}

@:multiReturn
extern class PairsResult<K,V> {
	var next : Table<K,V>->K->NextResult<K,V>;
	var table : Table<K,V>;
	var index : V;
}

@:multiReturn
extern class LoadResult {
	var func : Function;
	var message : String;
}
