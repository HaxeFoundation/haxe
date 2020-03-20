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

import haxe.macro.Expr;
import haxe.macro.Context;
import haxe.macro.ExprTools;

@:noPackageRestrict
@:noClosure
class Syntax {
	@:noUsing
	macro public static function importModule(module:String):Expr {
		return macro python.Syntax.code($v{"import " + module});
	}

	@:noUsing
	macro public static function importAs(module:String, className:String):ExprOf<Void> {
		var n = className.split(".").join("_");
		var e = "import " + module + " as " + n;

		return macro python.Syntax.code($v{e});
	}

	@:noUsing
	@:deprecated("python.Syntax.newInstance() is deprecated. Use python.Syntax.construct() instead.")
	macro public static function newInstance(c:Expr, params:Array<Expr>):Expr {
		return macro python.Syntax._newInstance($c, $a{params});
	}

	@:noUsing
	@:deprecated("python.Syntax.pythonCode() is deprecated. Use python.Syntax.code() instead.")
	macro public static function pythonCode(b:ExprOf<String>, rest:Array<Expr>):Expr {
		if (rest == null)
			rest = [];
		return macro @:pos(Context.currentPos()) untyped python.Syntax._pythonCode($b, $a{rest});
	}

	@:noUsing
	macro public static function arrayAccess(x:Expr, rest:Array<Expr>):ExprOf<Dynamic> {
		return macro python.Syntax._arrayAccess($x, $a{rest});
	}

	@:noUsing
	macro public static function arrayAccessWithTrailingColon(x:Expr, rest:Array<Expr>):ExprOf<Dynamic> {
		return macro python.Syntax._arrayAccess($x, $a{rest}, true);
	}

	@:noUsing
	macro public static function foreach<T>(v:Expr, it:Expr, b:Expr):Expr {
		var id = switch (v.expr) {
			case EConst(CIdent(x)): x;
			case _: Context.error("unexpected " + ExprTools.toString(v) + ": const ident expected", v.pos);
		}

		var iter = try {
			var it = macro($it.__iter__() : python.NativeIterator.NativeIteratorRaw<T>);
			Context.typeof(it);
			it;
		} catch (e:Dynamic) {
			macro($it : python.NativeIterable.NativeIterableRaw<T>);
		}

		return macro {
			var $id = null;
			python.Syntax._foreach($v, $it, cast $b);
		}
	}

	@:noUsing
	macro public static function importFromAs(from:String, module:String, className:String):ExprOf<Void> {
		var n = className.split(".").join("_");

		var e = "from " + from + " import " + module + " as " + n;

		return macro python.Syntax.code($v{e});
	}

	@:noUsing
	macro public static function callField(o:Expr, field:ExprOf<String>, params:Array<Expr>):Expr {
		return macro @:pos(o.pos) python.Syntax.call(python.Syntax.field($o, $field), $a{params});
	}

	@:noUsing
	macro public static function tuple(args:Array<Expr>):Dynamic {
		var args = macro $a{args};
		return macro python.Syntax._tuple($args);
	}

	macro public static function callNamedUntyped(e:Expr, args:Expr):Expr {
		return macro @:pos(e.pos) python.Syntax._callNamedUntyped($e, $args);
	}
}
