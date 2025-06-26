/*
 * Copyright (C)2005-2019 Haxe Foundation
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

package python;

import haxe.extern.Rest;

@:noPackageRestrict
@:noClosure
extern class Syntax {

	@:noUsing macro public static function importModule(module:String):haxe.macro.Expr;

	@:noUsing macro public static function importAs(module:String, className:String):haxe.macro.Expr;

	@:overload(function(className:String, args:Rest<Dynamic>):Dynamic {})
	static function construct<T>(cls:Class<T>, args:Rest<Dynamic>):T;

	@:noUsing
	@:deprecated("python.Syntax.newInstance() is deprecated. Use python.Syntax.construct() instead.")
	macro public static function newInstance(c:haxe.macro.Expr, params:Array<haxe.macro.Expr>):haxe.macro.Expr;

	extern static function _newInstance(c:Dynamic, args:Array<Dynamic>):Dynamic;

	@:noUsing
	extern static function isIn(a:Dynamic, b:Dynamic):Bool;

	@:noUsing
	extern static function delete(a:Dynamic):Void;

	@:noUsing
	extern static function binop(a:Dynamic, op:String, b:Dynamic):Dynamic;

	@:noUsing
	extern static function assign(a:Dynamic, b:Dynamic):Void;

	static function code(code:String, args:Rest<Dynamic>):Dynamic;

	@:noUsing
	@:deprecated("python.Syntax.pythonCode() is deprecated. Use python.Syntax.code() instead.")
	macro public static function pythonCode(b:ExprOf<String>, rest:Array<haxe.macro.Expr>):haxe.macro.Expr;

	@:noUsing
	static function _pythonCode<T>(b:String, args:Array<Dynamic>):T;

	@:noUsing
	macro public static function arrayAccess(x:haxe.macro.Expr, rest:Array<haxe.macro.Expr>):haxe.macro.Expr.ExprOf<Dynamic>;

	@:noUsing
	macro public static function arrayAccessWithTrailingColon(x:haxe.macro.Expr, rest:Array<haxe.macro.Expr>):haxe.macro.Expr.ExprOf<Dynamic>;

	extern static function _arrayAccess(a:Dynamic, args:Array<Dynamic>, ?trailingColon:Bool = false):Dynamic;

	@:noUsing
	extern static function arraySet(a:Dynamic, i:Dynamic, v:Dynamic):Dynamic;

	extern static function _foreach(id:Dynamic, it:Dynamic, block:Dynamic):Dynamic;

	@:noUsing
	macro public static function foreach<T>(v:haxe.macro.Expr, it:haxe.macro.Expr, b:haxe.macro.Expr):haxe.macro.Expr;

	@:noUsing macro public static function importFromAs(from:String, module:String, className:String):haxe.macro.Expr;

	@:noUsing
	macro public static function callField(o:haxe.macro.Expr, field:haxe.macro.Expr.ExprOf<String>, params:Array<haxe.macro.Expr>):haxe.macro.Expr;

	extern static function call(e:Dynamic, args:Array<Dynamic>):Dynamic;

	@:noUsing
	extern static function field(o:Dynamic, field:String):Dynamic;

	@:noUsing
	macro public static function tuple(args:Array<haxe.macro.Expr>):haxe.macro.Expr;

	extern static function _tuple(args:Array<Dynamic>):Dynamic;

	@:noUsing
	extern static function varArgs(args:Array<Dynamic>):Dynamic;

	macro public static function callNamedUntyped(e:haxe.macro.Expr, args:haxe.macro.Expr):haxe.macro.Expr;

	extern static function _callNamedUntyped(e:Dynamic, args:Dynamic):Dynamic;

	extern static function opPow(a:Int, b:Int):Int;
}
