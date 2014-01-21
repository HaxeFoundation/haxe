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
	macro static public function buildEnumAbstract():Array<Field> {
		var fields = Context.getBuildFields();
		var a = switch(Context.getLocalClass().get().kind) {
			case KAbstractImpl(a): a;
			case _: throw "";
		}
		var aT = a.get();
		var tThis = aT.type;
		var ctA = TAbstract(a, aT.params.map(function(tp) return tp.t));
		for (field in fields) {
			// allow extra static fields
			if( Lambda.has(field.access,AStatic) )
				continue;
			switch(field.kind) {
				case FVar(t, e):
					field.access = [AStatic,APublic,AInline];
					if (e == null) Context.error("Value required", field.pos);
					var monos = aT.params.map(function(_) return Context.typeof(macro null));
					var tF = ctA.applyTypeParameters(aT.params, monos);
					if (t != null) {
						switch(t.toType().follow()) {
							case t = TAbstract(a2, tl2) if (a.toString() == a2.toString()): tF = t;
							case _: Context.error("Explicit field type must be " + ctA.toString(), field.pos);
						}
					}
					var tE = Context.typeof(e);
					if (!Context.unify(tE, tThis)) Context.error('${tE.toString()} should be ${tThis.toString()}', e.pos);
					field.meta.push({name: ":impl", params: [], pos: field.pos});
					field.meta.push({name: ":enum", params: [], pos: field.pos});
					field.kind = FVar(tF.toComplexType(), macro cast $e);
				case _:
			}
		}
		return fields;
	}
}
