package php7;

import haxe.extern.EitherType;
import haxe.extern.Rest;

/**
	This class contains externs for native PHP functions defined in global namespace.
	For native PHP constants in global namespace see `php7.Const`.
**/
@:phpGlobal
extern class Global {
	/**
		@see http://php.net/manual/en/function.exit.php
	**/
	static function exit( status:EitherType<String,Int> ) : Void ;

	/**
		@see http://php.net/manual/en/function.exit.php
	**/
	static function die( status:EitherType<String,Int> ) : Void ;

	/**
		@see http://php.net/manual/en/function.error-reporting.php
	**/
	static function error_reporting( ?level:Int ) : Int ;

	/**
		@see http://php.net/manual/en/function.set-error-handler.php
	**/
	@:overload(function( error_handler:Int->String->Bool, ?error_types:Int ) : Dynamic {})
	@:overload(function( error_handler:Int->String->String->Bool, ?error_types:Int ) : Dynamic {})
	@:overload(function( error_handler:Int->String->String->Int->Bool, ?error_types:Int ) : Dynamic {})
	static function set_error_handler( ?error_handler:Int->String->String->Int->Array<Dynamic>->Bool, ?error_types:Int ) : Dynamic ;

	/**
		@see http://php.net/manual/en/function.restore-error-handler.php
	**/
	static function restore_error_handler() : Bool ;

	/**
		@see http://php.net/manual/en/function.set-exception-handler.php
	**/
	static function set_exception_handler( exception_handler:Throwable->Void ) : Dynamic ;

	/**
		@see http://php.net/manual/en/function.restore-exception-handler.php
	**/
	static function restore_exception_handler() : Bool ;

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
		@see http://php.net/manual/en/function.is-numeric.php
	**/
	static function is_numeric( value:Dynamic ) : Bool ;

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
		@see http://php.net/manual/en/function.is-null.php
	**/
	static function is_null( value:Dynamic ) : Bool ;

	/**
		@see http://php.net/manual/en/function.is-subclass-of.php
	**/
	static function is_subclass_of( value:Dynamic, className:String, allow_string:Bool = true ) : Bool ;

	/**
		@see http://php.net/manual/en/function.class-exists.php
	**/
	static function class_exists( class_name:String, autoload:Bool = true ) : Bool ;

	/**
		@see http://php.net/manual/en/function.interface-exists.php
	**/
	static function interface_exists( interface_name:String, autoload:Bool = true ) : Bool ;

	/**
		@see http://php.net/manual/en/function.intval.php
	**/
	static function intval( value:Dynamic, base:Int = 10 ) : Int ;

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
	static function strval( value:Dynamic ) : String ;

	/**
		@see http://php.net/manual/en/function.phpversion.php
	**/
	static function phpversion( ?extension:String ) : String ;

	/**
		@see http://php.net/manual/en/function.class-alias.php
	**/
	static function class_alias( original:String, alias:String, autoload:Bool = true ) : Bool ;

	/**
		@see http://php.net/manual/en/function.count.php
	**/
	static function count( array:Dynamic, ?mode:Int ) : Int ;

	/**
		@see http://php.net/manual/en/function.array-filter.php
	**/
	@:overload(function(array:NativeArray,callback:Dynamic->Bool,?flag:Int):NativeArray {})
	static function array_filter( array:NativeArray, ?callback:Dynamic->?Dynamic->Bool, flag:Int = 0 ) : NativeArray ;

	/**
		@see http://php.net/manual/en/function.implode.php
	**/
	@:overload(function(pieces:NativeArray):String {})
	static function implode( glue:String, pieces:NativeArray ) : String ;

	/**
		@see http://php.net/manual/en/function.array-map.php
	**/
	static function array_map( callback:Dynamic->Dynamic, array:Rest<NativeArray> ) : NativeArray ;

	/**
		@see http://php.net/manual/en/function.array-merge.php
	**/
	static function array_merge( array:Rest<NativeArray> ) : NativeArray ;

	/**
		@see http://php.net/manual/en/function.array-diff.php
	**/
	static function array_diff( array:Rest<NativeArray> ) : NativeArray ;

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
	static function array_reverse( array:NativeArray, preserve_keys:Bool = false ) : NativeArray ;

	/**
		@see http://php.net/manual/en/function.array-search.php
	**/
	static function array_search( needle:Dynamic, haystack:NativeArray, strict:Bool = false) : EitherType<Bool,EitherType<String,Int>> ;

	/**
		@see http://php.net/manual/en/function.array-shift.php
	**/
	static function array_shift( array:NativeArray ) : Dynamic ;

	/**
		@see http://php.net/manual/en/function.array-slice.php
	**/
	static function array_slice( array:NativeArray, offset:Int, length:Int = null, preserve_keys:Bool = false ) : NativeArray ;

	/**
		@see http://php.net/manual/en/function.array-splice.php
	**/
	static function array_splice( array:NativeArray, offset:Int, lenght:Int = 0, ?replacement:Dynamic ) : NativeArray ;

	/**
		@see http://php.net/manual/en/function.array-unshift.php
	**/
	static function array_unshift( arr:NativeArray, value:Rest<Dynamic> ) : Int ;

	/**
		@see http://php.net/manual/en/function.array-values.php
	**/
	static function array_values( arr:NativeArray ) : NativeIndexedArray<Dynamic> ;

	/**
		@see http://php.net/manual/en/function.array-keys.php
	**/
	static function array_keys( arr:NativeArray ) : NativeIndexedArray<EitherType<String,Int>> ;

	/**
		@see http://php.net/manual/en/function.array-key-exists.php
	**/
	static function array_key_exists( key:EitherType<String,Int>, arr:NativeArray ) : Bool ;

	/**
		@see http://php.net/manual/en/function.array-fill.php
	**/
	static function array_fill( start_index:Int, num:Int, value:Dynamic ) : NativeArray ;

	/**
		@see http://php.net/manual/en/function.usort.php
	**/
	static function usort( array:NativeArray, value_compare_func:Dynamic->Dynamic->Int ) : Bool ;

	/**
		@see http://php.net/manual/en/function.reset.php
	**/
	static function reset( array:NativeArray ) : Dynamic;

	/**
		@see http://php.net/manual/en/function.current.php
	**/
	static function current( array:NativeArray ) : Dynamic;

	/**
		@see http://php.net/manual/en/function.next.php
	**/
	static function next( array:NativeArray ) : Dynamic;

	/**
		@see http://php.net/manual/en/function.prev.php
	**/
	static function prev( array:NativeArray ) : Dynamic;

	/**
		@see http://php.net/manual/en/function.end.php
	**/
	static function end( array:NativeArray ) : Dynamic;

	/**
		@see http://php.net/manual/en/function.key.php
	**/
	static function key( array:NativeArray ) : EitherType<String,Int>;

	/**
		@see http://php.net/manual/en/function.each.php
	**/
	static function each( array:NativeArray ) : NativeArray;

	/**
		@see http://php.net/manual/en/function.defined.php
	**/
	static function defined( name:String ) : Bool;

	/**
		@see http://php.net/manual/en/function.define.php
	**/
	static function define( name:String, value:Dynamic, case_insensitive:Bool = false ) : Bool;

	/**
		@see http://php.net/manual/en/function.echo.php
	**/
	static function echo( args:Rest<String> ) : Void;

	/**
		@see http://php.net/manual/en/function.method-exists.php
	**/
	static function method_exists( object:Dynamic, method_name:String ) : Bool;

	/**
		@see http://php.net/manual/en/function.property-exists.php
	**/
	static function property_exists( object:Dynamic, property_name:String ) : Bool;

	/**
		@see http://php.net/manual/en/function.is-callable.php
	**/
	static function is_callable( value:Dynamic,  syntax_only:Bool = false, ?callable_name:String ) : Bool;

	/**
		@see http://php.net/manual/en/function.isset.php
	**/
	static function isset( value:Dynamic, args:Rest<Dynamic> ) : Bool;

	/**
		@see http://php.net/manual/en/function.unset.php
	**/
	static function unset( value:Dynamic, values:Rest<Dynamic> ) : Void;

	/**
		@see http://php.net/manual/en/function.get-object-vars.php
	**/
	static function get_object_vars( object:{} ) : NativeAssocArray<Dynamic>;

	/**
		@see http://php.net/manual/en/function.get-class.php
	**/
	static function get_class( object:{} = null ) : EitherType<Bool,String>;

	/**
		@see http://php.net/manual/en/function.get-parent-class.php
	**/
	static function get_parent_class( ?object:Dynamic ) : EitherType<Bool,String>;

	/**
		@see http://php.net/manual/en/function.var-dump.php
	**/
	static function var_dump( args:Rest<Dynamic> ) : Void;

	/**
		@see http://php.net/manual/en/function.ord.php
	**/
	static function ord( string:String ) : Int;

	/**
		@see http://php.net/manual/en/function.chr.php
	**/
	static function chr( code:Int ) : String;

	/**
		@see http://php.net/manual/en/function.strpos.php
	**/
	static function strpos( haystack:String, needle:String, offset:Int = 0 ) : EitherType<Bool, Int>;

	/**
		@see http://php.net/manual/en/function.strrpos.php
	**/
	static function strrpos( haystack:String, needle:String, offset:Int = 0 ) : EitherType<Bool, Int>;

	/**
		@see http://php.net/manual/en/function.str-split.php
	**/
	static function str_split( string:String, split_length:Int = 1 ) : EitherType<Bool,NativeIndexedArray<String>>;

	/**
		@see http://php.net/manual/en/function.strlen.php
	**/
	static function strlen( string:String ) : Int;

	/**
		@see http://php.net/manual/en/function.strcmp.php
	**/
	static function strcmp( str1:String, str2:String ) : Int;

	/**
		@see http://php.net/manual/en/function.str-repeat.php
	**/
	static function str_repeat( input:String, multiplier:Int ) : String;

	/**
		@see http://php.net/manual/en/function.explode.php
	**/
	static function explode( delimiter:String, string:String, ?limit:Int ) : EitherType<Bool,NativeIndexedArray<String>>;

	/**
		@see http://php.net/manual/en/function.substr.php
	**/
	static function substr( string:String, start:Int, ?length:Int ) : EitherType<Bool,String>;

	/**
		@see http://php.net/manual/en/function.strtoupper.php
	**/
	static function strtoupper( string:String ) : String;

	/**
		@see http://php.net/manual/en/function.strtolower.php
	**/
	static function strtolower( string:String ) : String;

	/**
		@see http://php.net/manual/en/function.debug-backtrace.php
	**/
	static function debug_backtrace( ?options:Int, ?limit:Int ) : NativeIndexedArray<NativeAssocArray<Dynamic>>;

	/**
		@see http://php.net/manual/en/function.call-user-func.php
	**/
	static function call_user_func( callback:Dynamic, arguments:Rest<Dynamic> ) : Dynamic;

	/**
		@see http://php.net/manual/en/function.call-user-func-array.php
	**/
	static function call_user_func_array( callback:Dynamic, arguments:NativeArray ) : Dynamic;

	/**
		@see http://php.net/manual/en/function.func-get-args.php
	**/
	static function func_get_args() : NativeIndexedArray<Dynamic>;

	/**
		@see http://php.net/manual/en/function.abs.php
	**/
	static function abs<T:Float>( number:T ) : T;

	/**
		@see http://php.net/manual/en/function.min.php
	**/
	static function min( values:Rest<Dynamic> ) : Dynamic;

	/**
		@see http://php.net/manual/en/function.max.php
	**/
	static function max( values:Rest<Dynamic> ) : Dynamic;

	/**
		@see http://php.net/manual/en/function.sin.php
	**/
	static function sin( arg:Float ) : Float;

	/**
		@see http://php.net/manual/en/function.cos.php
	**/
	static function cos( arg:Float ) : Float;

	/**
		@see http://php.net/manual/en/function.atan2.php
	**/
	static function atan2( y:Float, x:Float ) : Float;

	/**
		@see http://php.net/manual/en/function.tan.php
	**/
	static function tan( arg:Float ) : Float;

	/**
		@see http://php.net/manual/en/function.exp.php
	**/
	static function exp( arg:Float ) : Float;

	/**
		@see http://php.net/manual/en/function.log.php
	**/
	static function log( arg:Float ) : Float;

	/**
		@see http://php.net/manual/en/function.sqrt.php
	**/
	static function sqrt( arg:Float ) : Float;

	/**
		@see http://php.net/manual/en/function.floor.php
	**/
	static function floor( arg:Float ) : Float;

	/**
		@see http://php.net/manual/en/function.ceil.php
	**/
	static function ceil( arg:Float ) : Float;

	/**
		@see http://php.net/manual/en/function.round.php
	**/
	static function round( val:Float, precision:Int = 0, ?mode:Int ) : Float;

	/**
		@see http://php.net/manual/en/function.atan.php
	**/
	static function atan( arg:Float ) : Float;

	/**
		@see http://php.net/manual/en/function.asin.php
	**/
	static function asin( arg:Float ) : Float;

	/**
		@see http://php.net/manual/en/function.acos.php
	**/
	static function acos( arg:Float ) : Float;

	/**
		@see http://php.net/manual/en/function.pow.php
	**/
	static function pow( base:Float, exp:Float ) : Float;

	/**
		@see http://php.net/manual/en/function.mt-rand.php
	**/
	@:overload(function() : Int {})
	static function mt_rand( base:Int, exp:Int ) : Int;

	/**
		@see http://php.net/manual/en/function.mt-getrandmax.php
	**/
	static function mt_getrandmax() : Int;

	/**
		@see http://php.net/manual/en/function.is-nan.php
	**/
	static function is_nan( arg:Float ) : Bool;

	/**
		@see http://php.net/manual/en/function.is-finite.php
	**/
	static function is_finite( arg:Float ) : Bool;

	/**
		@see http://php.net/manual/en/function.trim.php
	**/
	static function trim( str:String, ?character_mask:String ) : String;

	/**
		@see http://php.net/manual/en/function.rtrim.php
	**/
	static function rtrim( str:String, ?character_mask:String ) : String;

	/**
		@see http://php.net/manual/en/function.ltrim.php
	**/
	static function ltrim( str:String, ?character_mask:String ) : String;

	/**
		@see http://php.net/manual/en/function.getenv.php
	**/
	static function getenv( varname:String ) : EitherType<String,Bool>;

	/**
		@see http://php.net/manual/en/function.putenv.php
	**/
	static function putenv( setting:String ) : Bool;

	/**
		@see http://php.net/manual/en/function.sleep.php
	**/
	static function sleep( seconds:Int ) : EitherType<Bool,Int>;

	/**
		@see http://php.net/manual/en/function.usleep.php
	**/
	static function usleep( micro_seconds:Int ) : Void;

	/**
		@see http://php.net/manual/en/function.setlocale.php
	**/
	@:overload(function( category:Int, locale:NativeIndexedArray<String> ) : EitherType<Bool,String> {})
	static function setlocale( category:Int, locale:Rest<String> ) : EitherType<Bool,String>;

	/**
		@see http://php.net/manual/en/function.getcwd.php
	**/
	static function getcwd() : EitherType<String,Bool>;

	/**
		@see http://php.net/manual/en/function.chdir.php
	**/
	static function chdir( directory:String ) : Bool;

	/**
		@see http://php.net/manual/en/function.php-uname.php
	**/
	static function php_uname( mode:String = 'a' ) : String;

	/**
		@see http://php.net/manual/en/function.system.php
	**/
	static function system( command:String, ?return_var:Ref<Int> ) : EitherType<String,Bool>;

	/**
		@see http://php.net/manual/en/function.microtime.php
	**/
	static function microtime( get_as_float:Bool = false ) : EitherType<Float,String>;

	/**
		@see http://php.net/manual/en/function.fopen.php
	**/
	static function fopen( filename:String, mode:String, use_include_path:Bool = false, ?context:Resource ) : EitherType<Bool,Resource>;

	/**
		@see http://php.net/manual/en/function.fgetc.php
	**/
	static function fgetc( handle:Resource ) : EitherType<Bool,String>;

	/**
		@see http://php.net/manual/en/function.preg-match.php
	**/
	static function preg_match( pattern:String, subject:String, ?matches:Ref<NativeIndexedArray<String>>, ?flags:Int, ?offset:Int ) : EitherType<Bool,Int> ;

	/**
		@see http://php.net/manual/en/function.preg-match-all.php
	**/
	static function preg_match_all( pattern:String, subject:String, ?matches:Ref<NativeIndexedArray<String>>, ?flags:Int, ?offset:Int ) : EitherType<Bool,Int> ;
}