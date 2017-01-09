/*
 * Copyright (C)2005-2017 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package hl;

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
	@:hlNative("std", "set_error_handler") static function setErrorHandler( v : Dynamic -> Void ) : Void;
	@:hlNative("std", "breakpoint") static function breakPoint() : Void;
	@:hlNative("std", "sys_is64") static function is64() : Bool;

}
