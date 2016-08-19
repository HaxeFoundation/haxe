package php7;

import haxe.extern.Rest;

/**
	This class contains externs for native PHP functions defined in global namespace.
	For native PHP constants in global namespace see `php7.Const`.
**/
@:phpGlobal
extern class Global {
	/**
		@see http://php.net/manual/en/function.error-reporting.php
	**/
	static function error_reporting( level:Int ) : Int ;

	/**
		@see http://php.net/manual/en/function.is-int.php
	**/
	static function is_int( value:Dynamic ) : Bool ;

	/**
		@see http://php.net/manual/en/function.is-float.php
	**/
	static function is_float( value:Dynamic ) : Bool ;

	/**
		@see http://php.net/manual/en/function.is-string.php
	**/
	static function is_string( value:Dynamic ) : Bool ;

	/**
		@see http://php.net/manual/en/function.is-bool.php
	**/
	static function is_bool( value:Dynamic ) : Bool ;

	/**
		Checks if `values` is `php7.NativeArray`
		@see http://php.net/manual/en/function.is-array.php
	**/
	static function is_array( value:Dynamic ) : Bool ;

	/**
		@see http://php.net/manual/en/function.is-object.php
	**/
	static function is_object( value:Dynamic ) : Bool ;

	/**
		@see http://php.net/manual/en/function.is-subclass-of.php
	**/
	@:overload(function(value:Dynamic,className:String):Bool {})
	static function is_subclass_of( value:Dynamic, className:String, allow_string:Bool ) : Bool ;

	/**
		@see http://php.net/manual/en/function.intval.php
	**/
	@:overload(function(value:Dynamic):Int {})
	static function intval( value:Dynamic, base:Int ) : Int ;

	/**
		@see http://php.net/manual/en/function.floatval.php
	**/
	static function floatval( value:Dynamic ) : Float ;

	/**
		@see http://php.net/manual/en/function.boolval.php
	**/
	static function boolval( value:Dynamic ) : Bool ;

	/**
		@see http://php.net/manual/en/function.strval.php
	**/
	static function strval( value:Dynamic ) : Float ;

	/**
		@see http://php.net/manual/en/function.phpversion.php
	**/
	@:overload(function():String {})
	static function phpversion( extension:String ) : String ;

	/**
			@see http://php.net/manual/en/function.class_alias.php
	**/
	@:overload(function(original:String,alias:String):Bool {})
	static function class_alias( original:String, alias:String, autoload:Bool ) : Bool ;


	/**
			@see http://php.net/manual/en/function.count.php
	**/
	@:overload(function(array:NativeArray):Int {})
	static function count( array:NativeArray, mode:Int ) : Int ;

	/**
			@see http://php.net/manual/en/function.array-filter.php
	**/
	@:overload(function(array:NativeArray):NativeArray {})
	@:overload(function(array:NativeArray,callback:Dynamic->Bool):NativeArray {})
	static function array_filter( array:NativeArray, callback:Dynamic->Bool, flag:Int ) : NativeArray ;

	/**
			@see http://php.net/manual/en/function.implode.php
	**/
	static function implode( glue:String = "", array:NativeArray ) : String ;

	/**
			@see http://php.net/manual/en/function.array-map.php
	**/
	static function array_map( callback:Dynamic->Dynamic, array:Rest<NativeArray> ) : NativeArray ;

	/**
			@see http://php.net/manual/en/function.array-merge.php
	**/
	static function array_merge( array:Rest<NativeArray> ) : NativeArray ;

	/**
			@see http://php.net/manual/en/function.array-pop.php
	**/
	static function array_pop( array:NativeArray ) : Dynamic;

	/**
			@see http://php.net/manual/en/function.array-push.php
	**/
	static function array_push( array:NativeArray, value:Rest<Dynamic> ) : Int ;

	/**
			@see http://php.net/manual/en/function.array-reverse.php
	**/
	@:overload(function(array:NativeArray):NativeArray {})
	static function array_reverse( array:NativeArray, preserve_keys:Bool ) : NativeArray ;

	/**
			@see http://php.net/manual/en/function.array-search.php
	**/
	@:overload(function(needle:Dynamic,haystack:NativeArray):Dynamic {})
	static function array_search( needle:Dynamic, haystack:NativeArray, strict:Bool ) : Dynamic ;

	/**
			@see http://php.net/manual/en/function.array-shift.php
	**/
	static function array_shift( array:NativeArray ) : Dynamic ;

	/**
			@see http://php.net/manual/en/function.array-slice.php
	**/
	@:overload(function(array:NativeArray,offset:Int):NativeArray {})
	@:overload(function(array:NativeArray,offset:Int,length:Int):NativeArray {})
	static function array_slice( array:NativeArray, offset:Int, length:Int, preserve_keys:Bool ) : NativeArray ;

	/**
			@see http://php.net/manual/en/function.array-splice.php
	**/
	@:overload(function(array:NativeArray,offset:Int):NativeArray {})
	@:overload(function(array:NativeArray,offset:Int,length:Int):NativeArray {})
	static function array_splice( array:NativeArray, offset:Int, lenght:Int, replacement:Dynamic ) : NativeArray ;

	/**
			@see http://php.net/manual/en/function.array-unshift.php
	**/
	static function array_unshift( arr:NativeArray, value:Rest<Dynamic> ) : Int ;

	/**
			@see http://php.net/manual/en/function.usort.php
	**/
	static function usort( array:NativeArray, value_compare_func:Dynamic->Dynamic->Int ) : Bool ;
}
