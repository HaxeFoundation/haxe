package hl.types;

extern class Api {

	@:hlNative("std","get_field") static function getField( obj : Dynamic, hash : Int ) : Dynamic;
	@:hlNative("std","set_field") static function setField( obj : Dynamic, hash : Int, value : Dynamic ) : Void;
	@:hlNative("std","has_field") static function hasField( obj : Dynamic, hash : Int ) : Bool;
	@:hlNative("std","delete_field") static function deleteField( obj : Dynamic, hash : Int ) : Bool;
	@:hlNative("std","call_method") static function callMethod( f : haxe.Constraints.Function, args : NativeArray<Dynamic> ) : Dynamic;
	@:hlNative("std","get_closure_value") static function getClosureValue( f : haxe.Constraints.Function ) : Dynamic;
	@:hlNative("std","no_closure") static function noClosure( f : haxe.Constraints.Function ) : haxe.Constraints.Function;
	@:hlNative("std", "safe_cast") static function safeCast( v : Dynamic, t : Type ) : Dynamic;

}
