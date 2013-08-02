/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package haxe.macro;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;

using haxe.macro.Tools;

class Build {
	macro static public function buildFakeEnum():Array<Field> {
		var fields = Context.getBuildFields();
		var a = switch(Context.getLocalClass().get().kind) {
			case KAbstractImpl(a): a;
			case _: throw "";
		}
		var tThis = a.get().type;
		var ctA = TAbstract(a, []).toComplexType();
		for (field in fields) {
			field.access = [AStatic,APublic,AInline];
			switch(field.kind) {
				case FVar(t, e):
					if (e == null) Context.error("Value required", field.pos);
					var tE = Context.typeof(e);
					if (!Context.unify(tE, tThis)) Context.error('${tE.toString()} should be ${tThis.toString()}', e.pos);
					field.kind = FVar(ctA, macro cast $e);
				case _:
			}
		}
		return fields;
	}
}