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
package python.internal;
#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
#end

@:noPackageRestrict
class Internal {

	#if macro

	static var _prefix = "_hx_";


	static var _className = _prefix + "class_name";
	static var _class = _prefix + "class";
	static var _fields = _prefix + "fields";
	static var _super = _prefix + "super";
	static var _methods = _prefix + "methods";
	static var _statics = _prefix + "statics";
	static var _interfaces = _prefix + "interfaces";
	static var _emptyInit = _prefix + "empty_init";
	static var _constructs = _prefix + "constructs";

	static var _classes = _prefix + "classes";

	static var _dict = "__dict__";

	static function _getPrefixed (x:Expr):Expr {
		return switch (x.expr) {
			case EConst(CString(x,_)): macro @:pos(Context.currentPos()) $v{_prefix + x};
			case _ : macro @:pos(Context.currentPos()) (python.Syntax.binop($v{_prefix},"+",$x):String);
		}
	}



	static function withPos(x:String):Expr {
		return macro @:pos(Context.currentPos()) $v{x};
	}

	static function fieldWithPos(o:Expr, x:String):Expr {
		return macro @:pos(Context.currentPos()) (untyped __define_feature__($v{"python." + x}, python.Syntax.field($o, $v{x})));
	}

	static function has (o:Expr, field:String):Expr {
		return macro (untyped __define_feature__($v{"python." + field}, python.internal.UBuiltins.hasattr($o, $v{field})) : Bool);
	}

	#end

	macro public static function getPrefixed (x:ExprOf<String>):Expr {
		return switch (x.expr) {
			case EConst(CString(x,_)): macro @:pos(Context.currentPos()) $v{_prefix + x};
			case _ : macro @:pos(Context.currentPos()) (python.Syntax.binop($v{_prefix},"+",$x):String);
		}
	}

	macro public static function classRegistry ():Expr {
		return macro (untyped __define_feature__($v{"python." + _classes}, python.Syntax.pythonCode($v{_classes})) : python.Dict<String, Class<Dynamic>>);
	}

	macro public static function callFieldPrefixed (o:Expr, x:String, params:Array<Expr>):Expr {
		var args = [o,macro @:pos(Context.currentPos()) $v{_prefix + x}].concat(params);
		return macro @:pos(Context.currentPos()) python.Syntax.callField($a{args});
	}

	macro public static function fieldPrefixed (o:Expr, x:String):Expr {
		var args = [o,macro @:pos(Context.currentPos()) $v{_prefix + x}];
		return macro @:pos(Context.currentPos()) python.Syntax.field($a{args});
	}

	macro public static function hasAttrPrefixed (o:Expr, x:String):Expr {
		var args = [o,macro @:pos(Context.currentPos()) $v{_prefix + x}];
		return macro @:pos(Context.currentPos()) python.Syntax.field($a{args});
	}



	macro public static function importAsPrefixed (o:String, x:String) {
		return macro @:pos(Context.currentPos()) python.Syntax.importAs($v{o}, $v{_prefix + x});
	}

	macro public static function prefix ():Expr {
		return macro @:pos(Context.currentPos()) $v{_prefix};
	}

	macro public static function pythonCodePrefixed (x:String):Expr {
		return macro python.Syntax.pythonCode($v{_prefix + x});
	}

	macro public static function hasClassName (o:Expr):Expr {
		return has(o, _className);
	}

	macro public static function hasInterfaces (o:Expr):Expr {
		return has(o, _interfaces);
	}

	macro public static function hasClass (o:Expr):Expr {
		return has(o, _class);
	}

	macro public static function hasConstructs (o:Expr):Expr {
		return has(o, _constructs);
	}

	macro public static function hasEmptyInit (o:Expr):Expr {
		return has(o, _emptyInit);
	}

	macro public static function hasMethods (o:Expr):Expr {
		return has(o, _methods);
	}

	macro public static function hasFields (o:Expr):Expr {
		return has(o, _fields);
	}

	macro public static function hasSuper (o:Expr):Expr {
		return has(o, _super);
	}

	macro public static function hasStatics (o:Expr):Expr {
		return has(o, _statics);
	}

	macro public static function classNameVal ():Expr {
		return withPos(_className);
	}

	macro public static function methodsVal ():Expr {
		return withPos(_methods);
	}

	macro public static function classVal():Expr {
		return withPos(_className);
	}

	macro public static function superVal():Expr {
		return withPos(_super);
	}

	macro public static function interfacesVal():Expr {
		return withPos(_interfaces);
	}

	macro public static function fieldsVal():Expr {
		return withPos(_fields);
	}

	macro public static function staticsVal():Expr {
		return withPos(_statics);
	}

	macro public static function constructsVal():Expr {
		return withPos(_constructs);
	}

	macro public static function emptyInitVal():Expr {
		return withPos(_emptyInit);
	}

	macro public static function fieldClassName (o:Expr):Expr {
		return fieldWithPos(o, _className);
	}

	macro public static function fieldInterfaces (o:Expr):Expr {
		return fieldWithPos(o, _interfaces);
	}

	macro public static function fieldClass (o:Expr):Expr {
		return fieldWithPos(o, _class);
	}

	macro public static function fieldSuper (o:Expr):Expr {
		return fieldWithPos(o, _super);
	}

	macro public static function fieldStatics (o:Expr):Expr {
		return fieldWithPos(o, _statics);
	}

	macro public static function fieldMethods (o:Expr):Expr {
		return fieldWithPos(o, _methods);
	}

	macro public static function fieldFields (o:Expr):Expr {
		return fieldWithPos(o, _fields);
	}

	macro public static function fieldConstructs (o:Expr):Expr {
		return fieldWithPos(o, _constructs);
	}

	macro public static function fieldDict (o:Expr):Expr {
		return macro @:pos(Context.currentPos()) python.Syntax.field($o, $v{_dict});
	}

	macro public static function fieldEmptyInit (o:Expr):Expr {
		return fieldWithPos(o, _emptyInit);
	}

	macro public static function callEmptyInit (o:Expr, instance:Expr):Expr {
		return macro @:pos(Context.currentPos()) python.Syntax.callField($o, $v{_emptyInit}, $instance);
	}
}