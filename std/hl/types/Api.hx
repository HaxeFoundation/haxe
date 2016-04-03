package hl.types;

extern class Api {

	static inline function rethrow( v : Dynamic ) : Void { untyped $rethrow(v); }
	@:hlNative("std","obj_get_field") static function getField( obj : Dynamic, hash : Int ) : Dynamic;
	@:hlNative("std","obj_set_field") static function setField( obj : Dynamic, hash : Int, value : Dynamic ) : Void;
	@:hlNative("std","obj_has_field") static function hasField( obj : Dynamic, hash : Int ) : Bool;
	@:hlNative("std","obj_delete_field") static function deleteField( obj : Dynamic, hash : Int ) : Bool;
	@:hlNative("std","call_method") static function callMethod( f : haxe.Constraints.Function, args : NativeArray<Dynamic> ) : Dynamic;
	@:hlNative("std","get_closure_value") static function getClosureValue( f : haxe.Constraints.Function ) : Dynamic;
	@:hlNative("std","no_closure") static function noClosure( f : haxe.Constraints.Function ) : haxe.Constraints.Function;
	@:hlNative("std", "value_cast") static function safeCast( v : Dynamic, t : Type ) : Dynamic;
	@:hlNative("std", "make_var_args") static function makeVarArgs( v : NativeArray<Dynamic> -> Dynamic ) : haxe.Constraints.Function;
	@:hlNative("std", "get_virtual_value") static function getVirtualValue( v : Dynamic ) : Dynamic;
}
