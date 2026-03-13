/*
Copyright (c) 2017-2019 Guillaume Desquesnes, Valentin Lemière

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
package json2object.writer;

#if !macro
class DataBuilder {}
#else
import haxe.macro.Expr;
import haxe.macro.Context;
import haxe.macro.Type;
import haxe.macro.TypeTools;
import json2object.Error;

using StringTools;
using haxe.macro.ExprTools;
using json2object.utils.TypeTools;

class DataBuilder {
	@:persistent
	private static var counter = 0;
	private static var writers = new Map<String, Type>();
	private static var jcustom = ":jcustomwrite";

	private static function notNull (type:Type) : Type {
		return switch (type) {
			case TAbstract(_.get()=>t, p):
				(t.name == "Null") ? notNull(p[0]) : type;
			case TType(_.get()=>t, p):
				(t.name == "Null") ? notNull(type.follow()) : type;
			default:
				type;
		}
	}

	private static function isNullable (type:Type) : Bool {
		if (notNull(type) != type) { return true; }
		return switch (type.followWithAbstracts()) {
			case TAbstract(_.get()=>t,_):
				!t.meta.has(":notNull");
			default:
				true;
		}
	}

	private static function makeStringWriter () : Expr {
		return macro return ((indentFirst) ? buildIndent(space, level) : '') + ((o == null) ? "null" : quote(cast o));
	}

	private static function makeBasicWriter (type:Type) : Expr {
		return isNullable(type)
			? macro return ((indentFirst) ? buildIndent(space, level) : '') + ((o == null) ? "null" : o+"")
			: macro return ((indentFirst) ? buildIndent(space, level) : '') + o;
	}

	private static function makeArrayWriter (subType:Type, baseParser:BaseType) : Expr {
		var cls = { name:baseParser.name, pack:baseParser.pack, params:[TPType(subType.toComplexType())]};
		return macro {
			var indent = buildIndent(space, level);
			var firstIndent = (indentFirst) ? indent : '';
			if (o == null) { return firstIndent + "null"; }
			var valueWriter = new $cls(ignoreNullOptionals);

			@:privateAccess {
				var values =  [for (element in o) valueWriter._write(element, space, level + 1, true, onAllOptionalNull)];
				var newLine = (space != '' && o.length > 0) ? '\n' : '';

				var json = firstIndent + "[" + newLine;
				json += values.join(',' + newLine) + newLine;
				json += indent + "]";
				return json;
			}
		};
	}

	private static function makeMapWriter (keyType:Type, valueType:Type,  baseParser:BaseType) : Expr {
		var clsValue = { name:baseParser.name, pack:baseParser.pack, params:[TPType(valueType.toComplexType())]};

		var keyMacro = switch (keyType.followWithAbstracts()) {
			case TInst(_.get()=>t, _):
				if (t.module == "String") {
					macro quote(key);
				}
				else {
					Context.fatalError("json2object: Only maps with Int or String keys are writable, got "+keyType.toString(), Context.currentPos());
				}
			case TAbstract(_.get()=>t, _):
				if (t.module == "StdTypes" && t.name == "Int") {
					macro key;
				}
				else {
					Context.fatalError("json2object: Only maps with Int or String keys are writable, got "+keyType.toString(), Context.currentPos());
				}
			default: Context.fatalError("json2object: Only maps with Int or String keys are writable, got "+keyType.toString(), Context.currentPos());
		}

		return macro {
			var indent = buildIndent(space, level);
			var firstIndent = (indentFirst) ? indent : '';
			if (o == null) { return firstIndent + "null"; }
			var valueWriter = new $clsValue(ignoreNullOptionals);

			@:privateAccess {
				var values =  [for (key in o.keys()) indent + space + '"'+key+'": '+valueWriter._write(o.get(key), space, level + 1, false, onAllOptionalNull)];
				var newLine = (space != '' && values.length > 0) ? '\n' : '';

				var json = firstIndent+'{' + newLine;
				json += values.join(',' + newLine) + newLine;
				json += indent+'}';
				return json;
			}
		};
	}

	private static function makeObjectOrAnonWriter (type:Type, baseParser:BaseType) : Expr {
		var fields:Array<ClassField>;

		var tParams:Array<TypeParameter>;
		var params:Array<Type>;

		switch (type) {
			case TAnonymous(_.get()=>t):
				fields = t.fields;
				tParams = [];
				params = [];

			case TInst(_.get()=>t, p):
				if (t.isPrivate)
				{
					t = TypeUtils.copyType(t);
				}

				fields = [];
				var s = t;
				while (s != null)
				{
					fields = fields.concat(s.fields.get());
					s = s.superClass != null ? s.superClass.t.get() : null;
				}

				tParams = t.params;
				params = p;

			case _: return macro return null;
		}

		var assignations:Array<Expr> = [];
		var skips: Array<Expr> = [];

		for (field in fields) {
			if (field.meta.has(":jignored")) { continue; }
			switch(field.kind) {
				case FVar(r,w):
					if (r == AccCall && w == AccCall && !field.meta.has(":isVar")) {
						continue;
					}

					var f_a = (r == AccCall || r == AccNever || r == AccNo) ? macro Reflect.field(o, $v{field.name}) : { expr: EField(macro o, field.name), pos: Context.currentPos() };
					var f_type = field.type.applyTypeParameters(tParams, params);
					var f_cls = {name:baseParser.name, pack:baseParser.pack, params:[TPType(f_type.toComplexType())]};

					var name = field.name;
					for (m in field.meta.get()) {
						if (m.name == ":alias" && m.params.length == 1) {
							switch (m.params[0].expr) {
								case EConst(CString(s)):
									name = s;
								default:
							}
						}
					}
					name = '"' + name + '": ';

					var assignation:Expr = macro indent + space + $v{name};

					var writer:Expr;
					if (field.meta.has(jcustom)) {
						try {
							writer = field.meta.extract(jcustom)[0].params[0];
							validateCustomWriter(field.type, writer);
						} catch (e:CustomFunctionError) {
							Context.fatalError(invalidWriterErrorMessage(field.type, writer, e.message), Context.currentPos());
						}
					}
					if (writer != null) {
						assignation = macro $assignation + $writer(cast $f_a);
					} else if (field.meta.has(":noquoting")) {
						assignation = macro $assignation
							+ new $f_cls(ignoreNullOptionals).dontQuote()._write(cast $f_a, space, level + 1, false, onAllOptionalNull);
					} else {
						assignation = macro $assignation + new $f_cls(ignoreNullOptionals)._write(cast $f_a, space, level + 1, false, onAllOptionalNull);
					}
					assignations.push(assignation);

					skips.push(
						field.meta.has(':optional')
							? macro $f_a == null
							: macro false
					);

				default:
			}
		}
		var array = {expr:EArrayDecl(assignations), pos:Context.currentPos()};
		var skips = {expr:EArrayDecl(skips), pos:Context.currentPos()};

		return macro {
			var indent = buildIndent(space, level);
			var firstIndent = (indentFirst) ? indent : '';
			if (o == null) { return firstIndent + "null"; }
			@:privateAccess{
				var decl = ${array};
				if (ignoreNullOptionals) {
					var skips = ${skips};
					if (skips.indexOf(false) == -1) {
						decl = onAllOptionalNull != null ? [onAllOptionalNull()] : [];
					}
					else {
						decl = [ for (i in 0...decl.length) skips[i] ? continue : decl[i]];
					}
				}
				var newLine = (space != '' && decl.length > 0) ? '\n' : '';

				var json = firstIndent + "{" + newLine;
				json += decl.join(',' + newLine) + newLine;
				json += indent + "}";
				return json;
			}
		};
	}

	private static function makeEnumWriter (type:Type, baseParser:BaseType) : Expr {
		var tParams:Array<TypeParameter>;
		var params:Array<Type>;

		var cases = [];
		switch (type) {
			case TEnum(_.get()=>t, p):
				tParams = t.params;
				params = p;
				for (n in t.names) {
					switch (t.constructs.get(n).type) {
						case TEnum(_,_):
							var value = '"'+n+'"';
							cases.push({expr: macro firstIndent + $v{value}, guard: null, values: [macro $i{n}]});
						case TFun(args, _):
							var constructor = [];
							var assignations:Array<Expr> = [];
							for (a in args) {
								constructor.push(macro $i{a.name});

								var a_type = a.t.applyTypeParameters(tParams, params);
								var a_cls = {name:baseParser.name, pack:baseParser.pack, params:[TPType(a_type.toComplexType())]};

								assignations.push(macro indent + space + space + '"'+$v{a.name} +'": '+ new $a_cls(ignoreNullOptionals)._write($i{a.name}, space, level + 2, false, onAllOptionalNull));
							}


							var call = {expr:ECall(macro $i{n}, constructor), pos:Context.currentPos()};
							var array = {expr:EArrayDecl(assignations), pos:Context.currentPos()};
							var jsonExpr = macro {
								var decl = ${array};
								var newLine = (space != '' && decl.length > 0) ? '\n' : '';
								var json = firstIndent +'{' + newLine;
								json += indent + space + '"'+$v{n}+'": {' + newLine;
								json += decl.join(',' + newLine) + newLine;
								json += indent + space +'}' + newLine;
								json += indent +'}';
							}
							cases.push({expr: jsonExpr, guard: null, values: [call]});

						default:
					}
				}
			default:
		}
		var switchExpr = {expr:ESwitch(macro o, cases, null), pos:Context.currentPos()};
		return macro {
			var indent = buildIndent(space, level);
			var firstIndent = (indentFirst) ? indent : '';
			if (o == null) { return firstIndent + "null"; }
			@:privateAccess {
				return $switchExpr;
			}
		};
	}

	private static function makeAbstractEnumWriter (type:Type) : Expr {
		switch (type.followWithAbstracts()) {
			case TInst(_.get()=>t, _):
				if (t.module != "String") {
					Context.fatalError("json2object: Unsupported abstract enum type:"+type.toString(), Context.currentPos());
				}
				else {
					return makeStringWriter();
				}
			case TAbstract(_.get()=>t, _):
				if (t.module != "StdTypes" && (t.name != "Int" && t.name != "Bool" && t.name != "Float")) {
					Context.fatalError("json2object: Unsupported abstract enum type:"+type.toString(), Context.currentPos());
				}
				else {
					return makeBasicWriter(type);
				}
			default: Context.fatalError("json2object: Unsupported abstract enum type:"+type.toString(), Context.currentPos());
		}
		return null;
	}

	private static function makeCustomWriter(t:Type, c:ClassType):Expr {
		var cexpr:Expr;
		try {
			cexpr = c.meta.extract(jcustom)[0].params[0];
			validateCustomWriter(t, cexpr);
		} catch (e:CustomFunctionError) {
			Context.fatalError(invalidWriterErrorMessage(t, cexpr, e.message), Context.currentPos());
		}
		return macro {
			return ${cexpr}(o);
		};
	}

	private static function invalidWriterErrorMessage(t:Type, e:Expr, m:String):String {
		var methodName = jcustom;

		if (e != null) {
			methodName = e.toString();
			var index = methodName.lastIndexOf(".") + 1;
			methodName = methodName.substr(index);
		}

		return 'Failed to create custom writer using ${e.toString()}, the function prototype should be (${t.toString()})->String: $m';
	}

	private static function validateCustomWriter(target:Type, e:Expr) {
		switch Context.typeof(e) {
			case TFun(args, ret):
				if (ret.toString() != "String"){
					throw new CustomFunctionError('Return type should be String');
				}

				if (args.length != 1) {
					throw new CustomFunctionError("Should have one argument");
				}

				if (args[0].t.toString() != target.toString()) {
					throw new CustomFunctionError('Argument type should be ${target.toString()}');
				}

			default:
				throw new CustomFunctionError("Custom writer should point to a static function");
		}
	}

	public static function makeWriter (c:BaseType, type:Type, base:Type) {
        if (base == null) { base = type; }

		var writerMapName = base.toString();
		if (writers.exists(writerMapName)) {
			return writers.get(writerMapName);
		}

		var writerName = c.name + "_" + (counter++);
		var writerClass = macro class $writerName {
			public var ignoreNullOptionals : Bool;
			private var shouldQuote : Bool = true;
			public function new (?ignoreNullOptionals:Bool=false) {
				this.ignoreNullOptionals = ignoreNullOptionals;
			}


			private inline function quote (str:String) {
				return shouldQuote ? json2object.writer.StringUtils.quote(str) : str;
			}
			private inline function dontQuote () {
				shouldQuote = false;
				return this;
			}

			private function buildIndent (space:String, level:Int) {
				if (level == 0) { return ''; }
				var buff = new StringBuf();
				for (i in 0...level) {
					buff.add(space);
				}
				return buff.toString();
			}
		};

		var writeExpr = switch (type) {
			case TInst(_.get()=>t, p) :
				switch(t.module) {
					case "String":
						makeStringWriter();
					case "Array" | "List" | "haxe.ds.List":
						if (p.length == 1 && p[0] != null) {
							makeArrayWriter(p[0], c);
						}
						else {
							macro return null;
						}
					case _:
						switch (t.kind) {
							case KTypeParameter(_):
								Context.fatalError("json2object: Type parameters are not writable: " + t.name, Context.currentPos());

							default:
								macro return null;
						}
						if (t.meta.has(jcustom)) {
							makeCustomWriter(type, t);
						} else {
							makeObjectOrAnonWriter(type, c);
						}
				}
			case TAnonymous(_.get()=>t):
				makeObjectOrAnonWriter(type, c);
			case TAbstract(_.get()=>t, p):
				if (t.name == "Null") {
					return makeWriter(c, p[0], type);
				}
				else if (t.name == "Any") {
					Context.fatalError("json2object: Parser of "+t.name+" are not generated", Context.currentPos());
				}
				else if (t.module == "UInt" || t.name == "UInt") {
					makeBasicWriter(base);
				}
				else if (t.module == "StdTypes") {
					switch (t.name) {
						case "Int", "Float", "Single", "Bool":
							makeBasicWriter(base);
						default: Context.fatalError("json2object: Parser of "+t.name+" are not generated", Context.currentPos());
					}
				}
				else if (t.module == #if (haxe_ver >= 4) "haxe.ds.Map" #else "Map" #end) {
					makeMapWriter(p[0], p[1], c);
				}
				else {
					if (t.meta.has(":enum")) {
						makeAbstractEnumWriter(type.applyTypeParameters(t.params, p));
					}
					else if (t.meta.has(":coreType")) {
						Context.fatalError("json2object: Parser of coreType ("+t.name+") are not generated", Context.currentPos());
					}
					else {
						var ap = t.type.applyTypeParameters(t.params, p);
						return makeWriter(c, ap, ap);
					}
				}
			case TEnum(_.get()=>t, p):
				makeEnumWriter(type.applyTypeParameters(t.params, p), c);
			case TType(_.get()=>t, p) :
				return makeWriter(c, t.type.applyTypeParameters(t.params, p), type);
			case TLazy(f):
				return makeWriter(c, f(), f());
			default: Context.fatalError("json2object: Writer for "+type.toString()+" are not generated", Context.currentPos());
		}

		var onAllOptionalNullCT : ComplexType = TFunction([],Context.getType("String").toComplexType());
		var args = [
			{name:"o", meta:null, opt:false, type:base.toComplexType(),value:null},
			{name:"space", meta:null, opt:false, type:Context.getType("String").toComplexType(), value:macro ""},
			{name:"level", meta:null, opt:false, type:Context.getType("Int").toComplexType(), value:macro 0},
			{name:"indentFirst", meta:null, opt:false, type:Context.getType("Bool").toComplexType(), value:macro false},
			{name:"onAllOptionalNull", meta:null, opt:true, type:onAllOptionalNullCT, value: macro null}
		];
		var privateWrite:Field = {
			doc: null,
			kind: FFun({args:args, expr:writeExpr, params:null, ret:null}),
			access: [APrivate],
			name: "_write",
			pos:Context.currentPos(),
			meta: null
		}
		writerClass.fields.push(privateWrite);

		var write:Field = {
			doc: null,
			kind: FFun({args:[args[0], args[1]], expr:macro return _write(o, space, 0, false), params:null, ret:null}),
			access: [APublic],
			name: "write",
			pos:Context.currentPos(),
			meta: null
		}
		writerClass.fields.push(write);

		haxe.macro.Context.defineType(writerClass);

		var constructedType = haxe.macro.Context.getType(writerName);
		writers.set(writerMapName, constructedType);
		return constructedType;

	}

	public static function build() {
		switch (Context.getLocalType()) {
			case TInst(c, [type]):
				return makeWriter(c.get(), type, type);
			case _:
				Context.fatalError("json2object: Writing tools must be a class", Context.currentPos());
				return null;
		}
	}
}
#end
