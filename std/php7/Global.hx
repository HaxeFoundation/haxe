package php7;



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
}
