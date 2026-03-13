/*
Copyright (c) 2016 Guillaume Desquesnes, Valentin Lemière

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

package json2object;

#if !macro
class TypeUtils
{
}
#else
import haxe.macro.Expr;
import haxe.macro.Context;
import haxe.macro.Type;

using json2object.utils.TypeTools;

class TypeUtils
{
	static var exist = new Map<String, Bool>();

	static function filterOnlyVar (ff:ClassField) : Bool
	{
		return switch (ff.kind)
		{
			case FVar(_, _):
				true;

			default:
				false;
		}
	}

	static function convertClassField (ff:ClassField) : Field
	{
		return {
			name: ff.name,
			doc: ff.doc,
			access: [APublic],
			kind: FVar(ff.type.toComplexType(), null),
			pos: ff.pos,
			meta: ff.meta.get()
		};
	}

	public static function copyType(t:ClassType) : ClassType
	{
		t.pack.pop();
		var n = t.name + "__json2object_nonprivate_copy";
		var m = t.pack.join(".");

		t = {
			module: m,
			name: n,
			isPrivate: false,
			#if (haxe_ver >= 4) isFinal: false, #end
			init: t.init,
			constructor: t.constructor,
			doc: t.doc,
			params: t.params,
			pos: t.pos,
			fields: t.fields,
			overrides: t.overrides,
			pack: t.pack,
			kind: t.kind,
			meta: t.meta,
			superClass: t.superClass,
			interfaces: t.interfaces,
			statics: t.statics,
			isExtern: t.isExtern,
			#if haxe4 
			#if (haxe >= version("4.2.0-rc.1")) 
			isAbstract: t.isAbstract, 
			#end 
			#end
			exclude: t.exclude,
			isInterface: t.isInterface
		}

		if (!exist.exists(t.name))
		{
			var td : TypeDefinition = {
				#if haxe4
				doc: t.doc,
				#end
				pack: t.pack,
				name: t.name,
				pos: t.pos,
				meta: t.meta.get(),
				params: [for (p in t.params) { name: p.name }],
				isExtern: t.isExtern,
				kind: TDClass(),
				fields: [for (f in Lambda.filter(t.fields.get(), filterOnlyVar)) convertClassField(f)]
			}

			Context.defineType(td);

			exist.set(t.name, true);
		}

		return t;
	}
}
#end
