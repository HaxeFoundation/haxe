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

package php;

import haxe.macro.Expr;
import haxe.macro.Context;

/**
	Special extern class to support PHP language specifics.
	Don't use these functions unless you are really sure what you are doing.
**/
@:noClosure
@:noPackageRestrict
class Syntax {
	/**
		```haxe
		Syntax.customArrayDecl([v1 => v2, v3 => v4]);
		```
		Generates native array declaration:
		```haxe
		[$v1 => $v2, $v3 => $v4]
		```
	**/
	macro static function customArrayDecl<T>(decl:Expr):ExprOf<php.NativeArray> {
		function invalidExpr(pos:Position) {
			Context.error('Invalid expression passed to php.Syntax.customArrayDecl()', pos);
			return macro @:pos(decl.pos) new php.NativeArray();
		}
		switch decl.expr {
			case EArrayDecl(items):
				var placeholders = [];
				var args = [];
				for(i => e in items) {
					switch e {
						case macro $index => $value:
							placeholders.push('{${i * 2}} => {${i * 2 + 1}}');
							args.push(index);
							args.push(value);
						case _:
							return invalidExpr(e.pos);
					}
				}
				var code = '[${placeholders.join(', ')}]';
				args.unshift(macro @:pos(decl.pos) $v{code});
				return macro @:pos(decl.pos) (php.Syntax.codeDeref($a{args}):php.NativeArray);
			case _:
				return invalidExpr(decl.pos);
		}
	}
}
