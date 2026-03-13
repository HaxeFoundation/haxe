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

package json2object.reader;

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

typedef JsonType = {jtype:String, name:String, params:Array<Type>}
typedef ParserInfo = {packs:Array<String>, clsName:String}

class DataBuilder {

	@:persistent
	private static var counter = 0;
	private static var parsers = new Map<String, Type>();
	private static var callPosition:Null<haxe.macro.Position> = null;
	private static var jcustom = ":jcustomparse";

	private static function notNull(type:Type):Type {
		return switch (type) {
			case TAbstract(_.get()=>t, p):
				(t.name == "Null") ? notNull(p[0]) : type;
			case TType(_.get()=>t, p):
				(t.name == "Null") ? notNull(type.follow()) : type;
			default:
				type;
		}
	}

	private static function isNullable(type:Type) {
		if (notNull(type) != type) { return true; }
		return switch (type.followWithAbstracts()) {
			case TAbstract(_.get()=>t,_):
				!t.meta.has(":notNull");
			default:
				true;
		}
	}

	// return true if type.followWithAbstract == String, Int, Float or Bool or Array of the previous
	private static function isBaseType(type:Type) {
		return switch (type.followWithAbstracts()) {
			case TInst(_.get()=>t, p):
				(t.module == "String" || (t.module == "Array" && isBaseType(p[0])));
			case TAbstract(_.get()=>t, p):
				(t.module == "StdTypes" && (t.name == "Int" || t.name == "Float" || t.name == "Bool"));
			default: false;
		}
	}

	private static function changeExprOf(field:Field, e:Expr) {
		switch (field.kind) {
			case FFun(f):
				f.expr = e;
			default: return;
		}
	}

	private static function changeFunction(name:String, of:TypeDefinition, to:Expr) {
		for (field in of.fields) {
			if (field.name == name) {
				changeExprOf(field, to);
			}
		}
	}

	public static function makeStringParser(parser:TypeDefinition) {
		changeFunction("loadJsonString", parser, macro {value = cast s;});
		changeFunction("loadJsonNull", parser, macro {value = null;});
	}

	public static function makeIntParser(parser:TypeDefinition, ?base:Type=null) {
		var e = macro {
			value = loadJsonInt(f, pos, variable, value);
		};
		changeFunction("loadJsonNumber", parser, e);
		if (base != null && isNullable(base)) {
			changeFunction("loadJsonNull", parser, macro {value = null;});
		}
	}

	public static function makeUIntParser(parser:TypeDefinition, ?base:Type=null) {
		var e = macro {
			value = loadJsonUInt(f, pos, variable, value);
		};
		changeFunction("loadJsonNumber", parser, e);
		if (base != null && isNullable(base)) {
			changeFunction("loadJsonNull", parser, macro {value = null;});
		}
	}

	public static function makeFloatParser(parser:TypeDefinition, ?base:Type=null) {
		var e = macro {
			value = loadJsonFloat(f, pos, variable, value);
		};
		changeFunction("loadJsonNumber", parser, e);
		if (base != null && isNullable(base)) {
			changeFunction("loadJsonNull", parser, macro {value = null;});
		}
	}

	public static function makeBoolParser(parser:TypeDefinition, ?base:Type=null) {
		changeFunction("loadJsonBool", parser, macro { value = cast b; });
		if (base != null && isNullable(base)) {
			changeFunction("loadJsonNull", parser, macro {value = null;});
		}
	}

	public static function makeArrayParser(parser:TypeDefinition, subType:Type, baseParser:BaseType) {
		var cls = { name:baseParser.name, pack:baseParser.pack, params:[TPType(subType.toComplexType())]};

		var e = if (Context.defined("cs")) {
			// Reduced version doesn't work on C#, using manual copy of the loop
			macro value = {
				var parser = new $cls(errors, putils, THROW);
				cast [
					for (j in a)
						try { parser.loadJson(j, variable); }
						catch (e:json2object.Error.InternalError) {
							if (e != ParsingThrow) {
								throw e;
							}

							continue;
						}
				];
			}
		} else {
			macro value = cast loadJsonArrayValue(a, new $cls(errors, putils, THROW).loadJson, variable);
		}

		changeFunction("loadJsonArray", parser, e);
		changeFunction("loadJsonNull", parser, macro {value = null;});
	}

	public static function makeListParser(parser:TypeDefinition, subType:Type, baseParser:BaseType) {
		var cls = { name:baseParser.name, pack:baseParser.pack, params:[TPType(subType.toComplexType())]};
		var list = {name:"List", pack:[#if (haxe_ver >= 4)"haxe", "ds"#end], params:[TPType(subType.toComplexType())]};

		var e = macro value = {
			var parser = new $cls(errors, putils, THROW);
			var res = new $list();
			for (j in a) {
				try {
					res.add(parser.loadJson(j, variable));
				}
				catch (e:json2object.Error.InternalError) {
					if (e != ParsingThrow) {
						throw e;
					}
				}
			}
			res;
		}

		changeFunction("loadJsonArray", parser, e);
		changeFunction("loadJsonNull", parser, macro {value = null;});
	}

	public static function makeCustomParser(parser:TypeDefinition, type:Type, t:ClassType){
		var cexpr:Expr;
		try {
			cexpr = t.meta.extract(jcustom)[0].params[0];
			validateCustomParser(type, cexpr);
		} catch (e:CustomFunctionError) {
			Context.fatalError(invalidParserErrorMessage(type, cexpr, e.message), Context.currentPos());
		}

		var e = macro {
			return value = ${cexpr}(json, variable);
		}

		var args:Array<FunctionArg> = [
			{
				name: "json",
				type: macro:hxjsonast.Json
			},
			{
				name:"variable",
				type: macro:String,
				opt: true,
				value: macro ""
			}
		];

		var loadJ:Field = {
			doc: null,
			kind: FFun({
				args: args,
				expr: e,
				params: null,
				ret: TypeTools.toComplexType(type)
			}),
			access: [AOverride, APublic],
			name: "loadJson",
			pos: Context.currentPos(),
			meta: null
		}
		parser.fields.push(loadJ);
	}

	private static function invalidParserErrorMessage(t:Type, e:Expr, m:String):String {
		var methodName = jcustom;

		if (e != null) {
			methodName = e.toString();
			var index = methodName.lastIndexOf(".") + 1;
			methodName = methodName.substr(index);
		}

		return 'Failed to create custom parser using ${e.toString()}, the function prototype should be (hxjsonast.Json, String)->${t.toString()}: $m';
	}

	private static function validateCustomParser(target:Type, e:Expr) {
		switch Context.typeof(e) {
			case TFun(args, ret):
				if (ret.toString() != target.toString()){
					throw new CustomFunctionError('Return type should be ${target.toString()}');
				}

				if (args.length != 2) {
					throw new CustomFunctionError("Should have two arguments");
				}

				if (args[0].t.toString() != "hxjsonast.Json") {
					throw new CustomFunctionError('First argument type should be hxjsonast.Json');
				}

				if (args[1].t.toString() != "String") {
					throw new CustomFunctionError('Second argument type should be String');
				}

			default:
				throw new CustomFunctionError("Custom parser should point to a static function");
		}
	}

	public static function makeObjectOrAnonParser(parser:TypeDefinition, type:Type, superType:Type, baseParser:BaseType) {
		var cls = {name:baseParser.name, pack:baseParser.pack, params:[TPType(type.toComplexType())]};

		var initializator:Expr;
		var isAnon = false;
		var isPrivate = null;
		var fields:Array<ClassField>;

		var tParams:Array<TypeParameter>;
		var params:Array<Type>;

		switch (type) {
			case TAnonymous(_.get()=>t):
				isAnon = true;
				fields = t.fields;
				tParams = [];
				params = [];

			case TInst(_.get()=>t, p):
				if (t.isPrivate)
				{
					t = TypeUtils.copyType(t);
					isPrivate = t;
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

				var pack = t.module.split(".");
				pack.push(t.name);

				var e = {
					expr: EConst(CIdent(pack.shift())),
					pos: Context.currentPos()
				};

				while (pack.length > 0)
				{
					e = {
						expr: EField(e, pack.shift()),
						pos: Context.currentPos()
					};
				}

				initializator = macro Type.createEmptyInstance($e{e});

			case _: return;
		}

		var baseValues:Array<{field:String, expr:Expr #if (haxe_ver >= 4) , quotes:haxe.macro.Expr.QuoteStatus #end}> = [];
		var autoExprs:Array<Expr> = [];
		var cases:Array<Case> = [];
		var assignedKeys:Array<Expr> = [];
		var assignedValues:Array<Expr> = [];

		for (field in fields) {
			if (field.meta.has(":jignored")) { continue; }
			switch(field.kind) {
				case FVar(r,w):
					if (r == AccCall && w == AccCall && !field.meta.has(":isVar")) {
						continue;
					}

					var needReflect = w == AccNever || w == AccCall #if (haxe_ver >= 4) || w == AccCtor #end;
					var canRead = r == AccNormal || r == AccNo;

					var f_a = { expr: EField(macro value, field.name), pos: Context.currentPos() };
					var f_type = field.type.applyTypeParameters(tParams, params);
					var f_cls = {name:baseParser.name, pack:baseParser.pack, params:[TPType(f_type.toComplexType())]};
					var nullCheck = Context.defined("cpp") && isNullable(f_type); // For cpp

					assignedKeys.push(macro $v{field.name});
					assignedValues.push(macro $v{field.meta.has(":optional")});

					var reader:Expr = macro new $f_cls(errors, putils, OBJECTTHROW).loadJson;
					var hasCustomParser = field.meta.has(jcustom);
					if (hasCustomParser){
						try {
							reader = field.meta.extract(jcustom)[0].params[0];
							validateCustomParser(field.type, reader);
						} catch(e:CustomFunctionError){
							Context.fatalError(invalidParserErrorMessage(field.type, reader, e.message), Context.currentPos());
						}
					}

					var assignation = if (needReflect) {
						macro {
							loadObjectFieldReflect($reader, field, $v{field.name}, assigned, pos);
						};
					} else if (nullCheck) {
						macro {
							var v = loadObjectField($reader, field, $v{field.name}, assigned, $f_a, pos);
							if (v != null) {
								$f_a = cast v;
							} else {
								$f_a = null;
							}
						};
					} else if (canRead) {
						macro {
							$f_a = cast loadObjectField($reader, field, $v{field.name}, assigned, $f_a, pos);
						};
					} else {
						macro {
							var v = loadObjectField($reader, field, $v{field.name}, assigned, $f_a, pos);
							if (v != null) {
								$f_a = cast v;
							}
						};
					}

					var caseValue = null;
					for (m in field.meta.get()) {
						if (m.name == ":alias" && m.params.length == 1) {
							switch (m.params[0].expr) {
								case EConst(CString(_)):
									caseValue = m.params[0];
								default:
							}
						}
					}

					if (caseValue == null) {
						caseValue = { expr: EConst(CString(field.name)), pos: Context.currentPos()};
					}

					cases.push({ expr: assignation, guard: null, values: [caseValue] });

					if (field.meta.has(":default")) {
						var metas = field.meta.extract(":default");
						if (metas.length > 0) {
							var meta = metas[0];
							if (meta.params != null && meta.params.length == 1) {
								if (meta.params[0].toString() == "auto") {
									baseValues.push({field:field.name, expr:macro new $f_cls([], putils, NONE).getAuto() #if (haxe_ver >= 4) , quotes:Unquoted #end});
								}
								else {
									baseValues.push({ field: field.name, expr: { expr: ECheckType(meta.params[0], f_type.toComplexType()), pos: meta.params[0].pos } #if (haxe_ver >= 4) , quotes: Unquoted #end });
								}
							}
						}
					}
					else {
						var e = switch(field.type) {
							case TAbstract(_.get() => t, _) if (t.name == "Any"): macro null;
							case TDynamic(_): macro null;
							default: macro new $f_cls([], putils, NONE).loadJson({value:JNull, pos:{file:"",min:0, max:1}});
						}
						baseValues.push({field:field.name, expr:e #if (haxe_ver >= 4) , quotes:Unquoted #end});
					}

					if (needReflect) {
						autoExprs.push(macro Reflect.setField(value, $v{field.name}, $e{baseValues[baseValues.length - 1].expr}));
					}
					else {
						autoExprs.push(macro $f_a = ${baseValues[baseValues.length - 1].expr});
					}

				default:
			}
		}

		var default_e = macro errors.push(UnknownVariable(field.name, putils.convertPosition(field.namePos)));
		var loop = { expr: ESwitch(macro field.name, cases, default_e), pos: Context.currentPos() };

		if (isAnon) {
			initializator = { expr: EObjectDecl(baseValues), pos: Context.currentPos() };
			changeFunction("getAuto", parser, macro return $initializator);
		}
		else {
			var casting =
				if (isPrivate != null && Context.defined("cpp") && !Context.defined("cppia"))
				{
					// hxcpp can't directly use the cast
					var abstractType = superType.toComplexType();
					macro cast ((cpp.Pointer.addressOf(value).reinterpret() : cpp.Pointer<$abstractType>).value);
				}
				else
				{
					macro cast value;
				}

			var autoExpr = macro {
				var value = $initializator;
				@:privateAccess {
					$b{autoExprs};
				}
				return $casting;
			}
			changeFunction("getAuto", parser, macro return $autoExpr);
		}

		var assignedKeys = { expr: EArrayDecl(assignedKeys), pos: Context.currentPos() };
		var assignedValues = { expr: EArrayDecl(assignedValues), pos: Context.currentPos() };

		var e = macro {
			var assigned = new Map<String,Bool>();
			objectSetupAssign(assigned, $assignedKeys, $assignedValues);
			value = getAuto();
			@:privateAccess {
				for (field in o) {
					$loop;
				}
			}
			objectErrors(assigned, pos);
		};

		changeFunction("loadJsonObject", parser, e);
		changeFunction("loadJsonNull", parser, macro {value = null;});
	}

	public static function makeMapParser(parser:TypeDefinition, key:Type, value:Type, baseParser:BaseType) {

		var k_cls = {name:baseParser.name, pack:baseParser.pack, params:[TPType(key.toComplexType())]};
		var keyMacro = switch (key.followWithAbstracts()) {
			case TInst(_.get()=>t, _):
				if (t.module == "String") {
					macro try {
						new $k_cls(errors, putils, THROW).loadJson({value:JString(field.name), pos:putils.revert(pos)}, variable);
					} catch (e:json2object.Error.InternalError) {
						if (e != ParsingThrow) {
							throw e;
						}

						continue;
					}
				}
				else {
					Context.fatalError("json2object: Only maps with Int or String keys are parsable, got "+key.toString(), callPosition);
				}
			case TAbstract(_.get()=>t, _):
				if (t.module == "StdTypes" && t.name == "Int") {
					macro try {
						new $k_cls(errors, putils, THROW).loadJson({value:JNumber(field.name), pos:putils.revert(pos)}, variable);
					} catch (e:json2object.Error.InternalError) {
						if (e != ParsingThrow) {
							throw e;
						}

						continue;
					}
				}
				else {
					Context.fatalError("json2object: Only maps with Int or String keys are parsable, got "+key.toString(), callPosition);
				}
			default: Context.fatalError("json2object: Only maps with Int or String keys are parsable, got "+key.toString(), callPosition);
		}

		var v_cls = {name:baseParser.name, pack:baseParser.pack, params:[TPType(value.toComplexType())]};
		var valueMacro = macro {
			try {
				new $v_cls(errors, putils, THROW).loadJson(field.value, field.name);
			}
			catch (e:json2object.Error.InternalError) {
				if (e != ParsingThrow) {
					throw e;
				}

				continue;
			}
		};

		var cls = {name:"Map", pack:[#if (haxe_ver >= 4)"haxe", "ds"#end], params:[TPType(key.toComplexType()), TPType(value.toComplexType())]};

		var e = macro {
			value = cast new $cls();
			for (field in o) {
				value.set($keyMacro, $valueMacro);
			}
		}

		changeFunction("loadJsonObject", parser, e);
		changeFunction("loadJsonNull", parser, macro {value = null;});
	}

	public static function makeEnumParser(parser:TypeDefinition, type:Type, baseParser:BaseType) {

		var objMacro:Expr;
		var strMacro:Expr;

		var typeName:String;
		switch (type) {
			case TEnum(_.get()=>t, p):
				typeName = t.name;
				var internStringCases = new Array<Case>();
				var internObjectCases = new Array<Case>();
				for (n in t.names) {

					var l = t.module.split(".");
					l.push(t.name);
					l.push(n);
					var subExpr = {expr:EConst(CIdent(l.shift())), pos:Context.currentPos()};
					while (l.length > 0) {
						subExpr = {expr:EField(subExpr, l.shift()), pos:Context.currentPos()};
					}

					switch (t.constructs.get(n).type) {
						case TEnum(_,_):
							subExpr = macro value = cast ${subExpr};
							internStringCases.push({expr: subExpr, guard: null, values: [macro $v{n}]});

							var objSubExpr = macro if (s0.length == 0) {
								$subExpr;
							} else {
								errors.push(InvalidEnumConstructor(field.name, $v{t.name}, pos));
								parsingThrow();
							};
							internObjectCases.push({expr: objSubExpr, guard: null, values: [macro $v{n}]});

						case TFun(args, _):
							var names = [for (a in args) a.name];

							var enumParams:Array<Expr> = [];
							var blockExpr = [ macro var _names = $v{names} ];
							blockExpr.push(
								macro if (s0.length != $v{args.length} || s0.filter(function (_v) { return _names.indexOf(_v.name) != -1;}).length != s0.length) {
									errors.push(InvalidEnumConstructor(field.name, $v{t.name}, pos));
									parsingThrow();
								}
							);
							var argCount = 0;
							for (a in args) {
								var arg_name = '__${a.name}';
								var at = a.t.applyTypeParameters(t.params, p);
								enumParams.push(macro $i{arg_name});
								blockExpr.push({expr: EVars([{name: arg_name, type:at.toComplexType(), expr:null}]), pos:Context.currentPos()});

								var a_cls = {name:baseParser.name, pack:baseParser.pack, params:[TPType(at.toComplexType())]};
								var v = macro $i{arg_name} = new $a_cls(errors, putils, THROW).loadJson(s0.filter(function (o) { return o.name == _names[$v{argCount}];})[0].value, field.name+"."+$v{a.name});
								blockExpr.push(v);
								argCount++;
							}

							subExpr = (enumParams.length > 0)
								? {expr:ECall(subExpr, enumParams), pos:Context.currentPos()}
								: subExpr;
							blockExpr.push(macro value = cast ${subExpr});

							var lil_expr:Expr = {expr: EBlock(blockExpr), pos:Context.currentPos()};
							internObjectCases.push({ expr: lil_expr, guard: null, values: [{ expr: EConst(CString(n)), pos: Context.currentPos()}] });


						default:
					}
				}
				var default_e = macro {
						errors.push(IncorrectEnumValue(variable, $v{t.name}, pos));
						parsingThrow();
					};
				objMacro = {expr: ESwitch(macro field.name, internObjectCases, default_e), pos: Context.currentPos() };
				objMacro = macro if (o.length != 1) {
					errors.push(IncorrectType(variable, $v{typeName}, pos));
					parsingThrow();
				} else {
					var field = o[0];
					switch (o[0].value.value) {
						case JObject(s0):
							${objMacro};
						default:
							errors.push(IncorrectType(field.name, $v{typeName}, putils.convertPosition(field.value.pos)));
							parsingThrow();
					}
				}
				strMacro = {expr: ESwitch(macro $i{"s"}, internStringCases, default_e), pos: Context.currentPos() };
			default:
		}

		changeFunction("loadJsonObject", parser, objMacro);
		changeFunction("loadJsonString", parser, strMacro);
		changeFunction("loadJsonNull", parser, macro { value = null; });
	}

	public static function makeAbstractEnumParser(parser:TypeDefinition, type:Type, baseParser:BaseType) {
		var name:String;

		switch (type.followWithAbstracts()) {
			case TInst(_.get()=>t, _):
				if (t.module != "String") {
					Context.fatalError("json2object: Unsupported abstract enum type:"+type.toString(), callPosition);
				}
				name = "String";
			case TAbstract(_.get()=>t, _):
				if (t.module != "StdTypes" && (t.name != "Int" && t.name != "Bool" && t.name != "Float")) {
					Context.fatalError("json2object: Unsupported abstract enum type:"+type.toString(), callPosition);
				}
				name = t.name;
			default: Context.fatalError("json2object: Unsupported abstract enum type:"+type.toString(), callPosition);
		}

		var caseValues = new Array<Expr>();

		var e = macro null;

		switch (type) {
			case TAbstract(_.get()=>t, p) :
				for (field in t.impl.get().statics.get()) {
					if (!field.meta.has(":enum") || !field.meta.has(":impl")) {
						continue;
					}
					if (field.expr() == null) { continue; }
					caseValues.push(
						switch (field.expr().expr) {
							case TConst(_): Context.getTypedExpr(field.expr());
							case TCast(caste, _):
								switch (caste.expr) {
									case TConst(tc):
										switch (tc) {
											case TNull: continue;
											default: Context.getTypedExpr(caste);
										}
									default: Context.getTypedExpr(caste);
								}
							default: continue;
						}
					);
				}

				if (caseValues.length == 0 && !isNullable(type)) {
					Context.fatalError("json2object: Abstract enum of type "+ type.toString() +"can't be parsed if empty", callPosition);
				}

				var v = switch (name) {
					case "String": macro s;
					case "Int", "Float":
						var cls = {name:baseParser.name, pack:baseParser.pack, params:[TPType(Context.getType(name).toComplexType())]} ;
						macro new $cls([], putils, NONE).loadJson({value:JNumber(f), pos:putils.revert(pos)}, variable);
					case "Bool": macro b;
					default: macro null;
				}

				if (caseValues.length > 0) {
					if (name == "String") {
						var values = { expr: EArrayDecl(caseValues), pos: Context.currentPos() };
						e = macro {
							value = cast loadString(s, pos, variable, $values, ${caseValues[0]});
						};
					} else {
						var case_e = [{expr:macro value = cast $v, guard:null, values:caseValues}];
						var default_e = macro {value = cast ${caseValues[0]}; onIncorrectType(pos, variable);};

						e = {expr: ESwitch(macro cast $v, case_e, default_e), pos: Context.currentPos() };
					}
				}
				else {
					e = macro null;
				}

				var defaultValue = (caseValues.length == 0) ? macro null : macro cast ${caseValues[0]};

				changeFunction("onIncorrectType", parser, macro {
					value = ${defaultValue};
					errors.push(IncorrectType(variable, $v{type.toString()}, pos));
					objectThrow(pos, variable);
				});

				if (isNullable(t.type)) {
					changeFunction("loadJsonNull", parser, macro {value = cast null;});
				}
			default:
		}

		switch (name) {
			case "String":
				changeFunction("loadJsonString", parser, e);
			case "Int", "Float":
				changeFunction("loadJsonNumber", parser, e);
			case "Bool":
				changeFunction("loadJsonBool", parser, e);
			default:
		}
	}

	public static function makeAbstractParser(parser:TypeDefinition, type:Type, baseParser:BaseType) {
		var hasFromFloat = false;
		var hasOneFrom = false;

		switch (type) {
			case TAbstract(_.get()=>t, p):

				var from = (t.from.length == 0) ? [{t:t.type, field:null}] : t.from;
				var i = 0;
				for(fromType in from) {
					switch (fromType.t.followWithAbstracts()) {
						case TInst(_.get()=>st, sp):
							if (st.module == "String") {
								if (i == 0) { makeStringParser(parser); }
								else {
									var cls = {name:baseParser.name, pack:baseParser.pack, params:[TPType(fromType.t.toComplexType())]};
									changeFunction("loadJsonString",
										parser,
										macro {
											value = new $cls(errors, putils, NONE).loadJson(
											{value:JString(s), pos:putils.revert(pos)},
												variable);
											});
									changeFunction("loadJsonNull", parser, macro {value = null;});
								}
								hasOneFrom = true;
							}
							else if (st.module == "Array") {
								var subType = sp[0];
								for (i in 0...t.params.length) {
									if (subType.unify(t.params[i].t)) {
										subType = p[i];
										break;
									}
								}

								hasOneFrom = true;
								if (i == 0) {
									makeArrayParser(parser,subType.followWithAbstracts(), baseParser);
								}
								else if (isBaseType(subType.followWithAbstracts())) {
									var aParams = switch (fromType.t.followWithAbstracts()) {
										case TInst(r,_): [TPType(TInst(r,[subType]).toComplexType())];
										default:[];
									}
									var cls = {name:baseParser.name, pack:baseParser.pack, params:aParams};
									changeFunction("loadJsonArray",
										parser,
										macro {
											value = new $cls(errors, putils, NONE).loadJson(
											{value:JArray(a), pos:putils.revert(pos)},
												variable);
											});
									changeFunction("loadJsonNull", parser, macro {value = null;});
								}
								else {
									hasOneFrom = false;
								}
							}
							else {
								if (i == 0) {
									var t = fromType.t;
									if (st.isPrivate) {
										var privateType = TypeUtils.copyType(st);
										t = Context.getType(privateType.name);
									}

									var cls = {name:baseParser.name, pack:baseParser.pack, params:[TPType(t.toComplexType())]};
									var casting =
										if (st.isPrivate && Context.defined("cpp") && !Context.defined("cppia"))
										{
											// hxcpp can't directly use the cast
											var abstractType = type.toComplexType();
											macro {
												var __tmp__new = new $cls(errors, putils, NONE).loadJson(
													{value:JObject(o), pos:putils.revert(pos)},
													variable);
												cast ((cpp.Pointer.addressOf(__tmp__new).reinterpret() : cpp.Pointer<$abstractType>).value);
											}
										}
										else if (st.isPrivate && (Context.defined("cs") || Context.defined("java") || Context.defined("hl")))
										{
											Context.fatalError("json2object: Abstract of private are not supported on this target", callPosition);
										}
										else
										{
											macro cast new $cls(errors, putils, NONE).loadJson(
											{value:JObject(o), pos:putils.revert(pos)},
											variable);
										}
									changeFunction("loadJsonObject", parser, macro {
										value = $casting;
									});
									changeFunction("loadJsonNull", parser, macro {value = null;});
									hasOneFrom = true;
								}
							}
						case TAbstract(_.get()=>st, sp):
							if (st.module == "StdTypes") {
								var cls = {name:baseParser.name, pack:baseParser.pack, params:[TPType(fromType.t.toComplexType())]};
								switch (st.name) {
									case "Int":
										if (!hasFromFloat) {
											if (i == 0) {
												makeIntParser(parser, fromType.t);
											}
											else {
												changeFunction("loadJsonNumber",
													parser,
													macro {
														value = new $cls(errors, putils, NONE).loadJson(
														{value:JNumber(f), pos:putils.revert(pos)},
															variable);
														});
											}
											hasOneFrom = true;
										}
									case "Float":
										if (i == 0) {
												makeFloatParser(parser, fromType.t);
										}
										else {
											changeFunction("loadJsonNumber",
												parser,
												macro {
													value = new $cls(errors, putils, NONE).loadJson(
													{value:JNumber(f), pos:putils.revert(pos)},
														variable);
													});
										}
										hasFromFloat = true;
										hasOneFrom = true;
									case "Bool":
										if (i == 0) {
											makeBoolParser(parser, fromType.t);
										}
										else {
											changeFunction("loadJsonBool",
												parser,
												macro {
													value = new $cls(errors, putils, NONE).loadJson(
													{value:JBool(b), pos:putils.revert(pos)},
														variable);
													});
										}
										hasOneFrom = true;
								}
							}
							else if (i == 0 && t.module == #if (haxe_ver >= 4) "haxe.ds.Map" #else "Map" #end) {
								var key = sp[0];
								var value = sp[1];
								for (i in 0...t.params.length) {
									if (key.unify(t.params[i].t)) {
										key = p[i];
									}
									if (value.unify(t.params[i].t)) {
										value = p[i];
									}
								}
								makeMapParser(parser, key, value, baseParser);
								hasOneFrom = true;
							}
						case TAnonymous(_.get()=>st):
							if (i == 0) {
								var cls = {name:baseParser.name, pack:baseParser.pack, params:[TPType(fromType.t.toComplexType())]};
									changeFunction("loadJsonObject", parser, macro {
										value = cast new $cls(errors, putils, NONE).loadJson(
											{value:JObject(o), pos:putils.revert(pos)},
											variable);
									});
									changeFunction("loadJsonNull", parser, macro {value = null;});
								hasOneFrom = true;
							}
						default:
					}
					i++;
				}

				if (isNullable(t.type)) {
					changeFunction("loadJsonNull", parser, macro {value = cast null;});
				}
			default:
		}
		if (!hasOneFrom) {
			Context.fatalError("json2object: No parser can be generated for "+type.toString()+ " as it has no supported @:from", callPosition);
		}
	}

	public static function makeParser(c:BaseType, type:Type, ?base:Type=null) {

		if (base == null) { base = type; }

		var parserMapName = base.toString();
		if (parsers.exists(parserMapName)) {
			return parsers.get(parserMapName);
		}

		var defaultValueExpr:Expr = switch (type) {
			case TAbstract(_.get()=>t,_):
				switch (t.name) {
					case "Int", "Float", "Single" if (!isNullable(base) && t.module == "StdTypes"):
						macro value = 0;
					case "UInt" if (!isNullable(base) && t.module == "UInt"):
						macro value = 0;
					case "Bool" if (!isNullable(base) && t.module == "StdTypes"):
						macro value = false;
					default: macro {};
				}
			default: macro {};
		}

		var parserName = c.name + "_" + (counter++);
		var parent = {name:"BaseParser", pack:["json2object", "reader"], params:[TPType(base.toComplexType())]};
		var parser = macro class $parserName extends $parent {
			public function new(?errors:Array<json2object.Error>=null, ?putils:json2object.PositionUtils=null, ?errorType:json2object.Error.ErrorType=json2object.Error.ErrorType.NONE) {
				super(errors, putils, errorType);
				${defaultValueExpr}
			}

			override private function onIncorrectType(pos:json2object.Position, variable:String) {
				errors.push(IncorrectType(variable, $v{type.toString()}, pos));
				super.onIncorrectType(pos, variable);
			}

			override private function loadJsonNull(pos:json2object.Position, variable:String) {
			}
			override private function loadJsonString(s:String, pos:json2object.Position, variable:String) {
			}
			override private function loadJsonNumber(f:String, pos:json2object.Position, variable:String) {
			}
			override private function loadJsonBool(b:Bool, pos:json2object.Position, variable:String) {
			}
			override private function loadJsonArray(a:Array<hxjsonast.Json>, pos:json2object.Position, variable:String) {
			}
			override private function loadJsonObject(o:Array<hxjsonast.Json.JObjectField>, pos:json2object.Position, variable:String) {
			}
		};

		if (Context.defined("cs")) {
			// C# fix for conversion with baseparser
			parser.meta.push({
				name: ":nativeGen",
				params: null,
				pos: Context.currentPos()
			});
		}

		var parser_cls = { name: parserName, pack: [], params: null, sub: null };
		var getAutoExpr = macro return new $parser_cls([], putils, NONE).loadJson({value:JNull, pos:{file:"",min:0, max:1}});
		var getAuto:Field = {
			doc: null,
			kind: FFun({args:[], expr:getAutoExpr, params:null, ret:TypeTools.toComplexType(base)}),
			access: [APublic],
			name: "getAuto",
			pos:Context.currentPos(),
			meta: null
		}
		parser.fields.push(getAuto);

		switch (type) {
			case TInst(_.get()=>t, p) :
				switch(t.module) {
					case "String":
						makeStringParser(parser);
					case "Array" if (p.length == 1 && p[0] != null):
						makeArrayParser(parser, p[0], c);
					case "List" | "haxe.ds.List" if (p.length == 1 && p[0] != null):
						makeListParser(parser, p[0], c);
					case _:
						switch (t.kind) {
							case KTypeParameter(_):
								Context.fatalError("json2object: Type parameters are not parsable: " + t.name, callPosition);

							default:
						}
						if (t.meta.has(jcustom)){
							makeCustomParser(parser, type, t);
						} else {
							makeObjectOrAnonParser(parser, type, null, c);
						}
				}
			case TAnonymous(_):
				makeObjectOrAnonParser(parser, type, null, c);
			case TAbstract(_.get()=>t, p):
				if (t.name == "Null") {
					return makeParser(c, p[0], type);
				}
				else if (t.name == "Any") {
					Context.fatalError("json2object: Parser of "+t.name+" are not generated", callPosition);
				}
				else if (t.module == "UInt" && t.name == "UInt") {
					makeUIntParser(parser, base);
				}
				else if (t.module == "StdTypes") {
					switch (t.name) {
						case "Int" :
							makeIntParser(parser, base);
						case "Float", "Single":
							makeFloatParser(parser, base);
						case "Bool":
							makeBoolParser(parser, base);
						default: Context.fatalError("json2object: Parser of "+t.name+" are not generated", callPosition);
					}
				}
				else if (t.module == #if (haxe_ver >= 4) "haxe.ds.Map" #else "Map" #end) {
					makeMapParser(parser, p[0], p[1], c);
				}
				else {
					if (t.meta.has(":enum")) {
						makeAbstractEnumParser(parser, type.applyTypeParameters(t.params, p), c);
					}
					else if (t.meta.has(":coreType")) {
						Context.fatalError("json2object: Parser of coreType ("+t.name+") are not generated", Context.currentPos());
					}
					else {
						makeAbstractParser(parser, type.applyTypeParameters(t.params, p), c);
					}
				}
			case TEnum(_.get()=>t, p):
				makeEnumParser(parser, type.applyTypeParameters(t.params, p), c);
			case TType(_.get()=>t, p) :
				return makeParser(c, t.type.applyTypeParameters(t.params, p), type);
			case TLazy(f):
				return makeParser(c, f());
			default: Context.fatalError("json2object: Parser of "+type.toString()+" are not generated", callPosition);
		}

		parser.fields = parser.fields.filter(function (field) {
			return switch (field.kind) {
				case FFun({expr:{expr:EBlock([])}}): false;
				default: true;
			}
		});

		haxe.macro.Context.defineType(parser);

		var constructedType = haxe.macro.Context.getType(parserName);
		parsers.set(parserMapName, constructedType);
		return constructedType;

	}

	public static function build() {
		switch (Context.getLocalType()) {
			case TInst(c, [type]):
				var pos = Context.getPosInfos(Context.currentPos());
				if (pos.min != -1 && pos.max != -1) {
					callPosition = Context.makePosition(pos);
				}
				return makeParser(c.get(), type);
			case _:
				Context.fatalError("json2object: Parsing tools must be a class", Context.currentPos());
				return null;
		}
	}
}
#end
