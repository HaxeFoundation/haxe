package json2object.utils;

import haxe.ds.Option;
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
import haxe.macro.TypeTools as StdTypeTools;

/**
Taken from https://github.com/haxetink/tink_macro/blob/296cc6c8610c5638a4f3b4a0f52d715ce74dcc30/src/tink/macro/Sisyphus.hx
and https://github.com/haxetink/tink_macro/blob/296cc6c8610c5638a4f3b4a0f52d715ce74dcc30/src/tink/macro/Types.hx
by Juraj Kirchheim MIT Licenced https://github.com/haxetink/tink_macro/blob/master/LICENSE
**/
class TypeTools {
	#if (macro || display)

	public static inline function follow(type:Type, ?once:Bool):Type {
		return StdTypeTools.follow(type, once);
	}

	public static inline function followWithAbstracts(type:Type, once:Bool=false):Type {
		return StdTypeTools.followWithAbstracts(type, once);
	}

	public static inline function applyTypeParameters(type:Type, typeParameters:Array<TypeParameter>, concreteTypes:Array<Type>):Type {
		return StdTypeTools.applyTypeParameters(type, typeParameters, concreteTypes);
	}

	public static inline function toString(type:Type):String {
		return StdTypeTools.toString(type);
	}

	public static inline function unify(t1:Type, t2:Type):Bool {
		return StdTypeTools.unify(t1, t2);
	}

	#end

	#if macro

	public static inline function toComplexType(type:Null<Type>):Null<ComplexType> {
		return {
			inline function direct()
				return StdTypeTools.toComplexType(type);

			switch (type) {
				case null:
					null;
				case TEnum(_.get().isPrivate => true, _): direct();
				case TInst(_.get().isPrivate => true, _): direct();
				case TType(_.get().isPrivate => true, _): direct();
				case TAbstract(_.get().isPrivate => true, _): direct();
				case TMono(_): direct();
				case TEnum(_.get() => baseType, params):
					TPath(toTypePath(baseType, params));
				case TInst(_.get() => classType, params):
					switch (classType.kind) {
						case KTypeParameter(_):
							var ct = asComplexType(classType.name);
							switch toType(ct) {
								case Some(TInst(_.get() => cl, _)) if (cl.kind.match(KTypeParameter(_)) && cl.module == classType.module && cl.pack.join('.') == classType.pack.join('.')):
									ct;
								default:
									direct();
							}
						default:
							TPath(toTypePath(classType, params));
					}
				case TType(_.get() => baseType, params):
					TPath(toTypePath(baseType, params));
				case TFun(args, ret):
					TFunction([ for (a in args) a.opt ? nullable(toComplexType(a.t)) : toComplexType(a.t) ], toComplexType(ret));
				case TAnonymous(_.get() => { fields: fields }):
					TAnonymous([ for (cf in fields) toField(cf) ]);
				case TDynamic(t):
					if (t == null) {
						macro : Dynamic;
					} else {
						var ct = toComplexType(t);
						macro : Dynamic<$ct>;
					}
				case TLazy(f):
					toComplexType(f());
				case TAbstract(_.get() => baseType, params):
					TPath(toTypePath(baseType, params));
				default:
					throw "Invalid type";
			}
		}
	}

	static function toTypePath(baseType:BaseType, params:Array<Type>):TypePath {
		return {
			var module = baseType.module;
			var name = module.substring(module.lastIndexOf(".") + 1);
			var sub = switch baseType.name {
				case _ == name => true: null;
				case v: v;
			}

			{
				pack: baseType.pack,
				name: name,
				sub: sub,
				params: [for (t in params) switch t {
					case TInst(_.get().kind => KExpr(e), _): TPExpr(e);
					default: TPType(toComplexType(t));
				}],
			}
		}
	}

	static inline function asComplexType(s:String, ?params)
	{
    	return TPath(asTypePath(s, params));
	}

	static function asTypePath(s:String, ?params):TypePath {
		var parts = s.split('.');
		var name = parts.pop(),
		sub = null;
		if (parts.length > 0 && parts[parts.length - 1].charCodeAt(0) < 0x5B) {
			sub = name;
			name = parts.pop();
			if(sub == name) sub = null;
		}
		return {
			name: name,
			pack: parts,
			params: params == null ? [] : params,
			sub: sub
		};
	}

	static function toType(t:ComplexType, ?pos:Position):Option<Type> {
		if (pos == null) pos = Context.currentPos();
		return try {
			Some(Context.typeof(macro @:pos(pos) {
				var v:$t = null;
				v;
			}));
		} catch (_:Dynamic) {
			None;
		}
	}

	static function nullable(complexType:ComplexType):ComplexType return macro : Null<$complexType>;

	static function toField(cf : ClassField) : Field return {
		function varAccessToString(va : VarAccess, getOrSet : String) : String return {
			switch (va) {
				case AccNormal: "default";
				case AccNo: "null";
				case AccNever: "never";
				case AccResolve: throw "Invalid TAnonymous";
				case AccCall: getOrSet;
				case AccInline: "default";
				case AccRequire(_, _): "default";
				default: throw "not implemented";
			}
		}
		if (cf.params.length == 0) {
			name: cf.name,
			doc: cf.doc,
			access:
			(cf.isPublic ? [ APublic ] : [ APrivate ])
			#if haxe4 .concat(if (cf.isFinal) [AFinal] else []) #end
			,
			kind: switch([ cf.kind, cf.type ]) {
				#if haxe4
				case [ FVar(_, _), ret ] if (cf.isFinal):
					FVar(toComplexType(ret), null);
				#end
				case [ FVar(read, write), ret ]:
					FProp(
						varAccessToString(read, "get"),
						varAccessToString(write, "set"),
						toComplexType(ret),
						null
					);
				case [ FMethod(_), TFun(args, ret) ]:
					FFun({
						args: [
						for (a in args) {
							name: a.name,
							opt: a.opt,
							type: toComplexType(a.t),
						}
						],
						ret: toComplexType(ret),
						expr: null,
					});
				default:
					throw "Invalid TAnonymous";
			},
			pos: cf.pos,
			meta: cf.meta.get(),
		} else {
			throw "Invalid TAnonymous";
		}
	}

	#end
}
