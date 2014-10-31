package cs.internal;
#if macro
import haxe.macro.Expr;
import haxe.macro.Type;
import haxe.macro.Context.*;

using haxe.macro.ExprTools;
#end

/**
	Used internally to typecheck strict metas. Implement the `StrictMeta` interface to use this feature.
**/
@:noPackageRestrict class StrictMetaMacro
{
#if macro
	public static function build():Array<Field>
	{
		var fields = getBuildFields();
		var found = false;
		for (f in fields)
		{
			var m = handleMeta(f.meta);
			if (f.meta != m)
			{
				found = true;
				f.meta = m;
			}
		}

		var ctype = getLocalClass().get();
		handleMeta(ctype.meta.get(), ctype.meta);

		if (found) return fields;
		return null;
	}

	private static function handleMeta(meta:Metadata, ?acc:MetaAccess):Metadata
	{
		var ret = [],
				found = false;
		for (m in meta)
		{
			if (m.name == 'M')
			{
				found = true;
				m.name = ':meta';

				if (m.params == null)
					throw new Error('@M strict meta must contain a parameter which indicates which attribute will be used', m.pos);
				else if (m.params.length != 1)
					throw new Error('Invalid number of parameters for @M strict meta. It must contain a single parameter', m.pos);
				switch (m.params[0].expr)
				{
					case EConst(CIdent(_)) | EField(_,_):
						var classExpr = getClassExpr(m.params[0]);
						m.params[0] = classExpr;
						if (acc != null)
						{
							acc.remove('M');
							acc.add(':meta', [classExpr], m.pos);
						}
					case ECall(cls, params):
						var idx = -1;
						for (p in params)
						{
							idx++;
							switch (p.expr)
							{
								case EBinop(OpAssign | OpAssignOp(_),_,_):
									break;
								case _:
							}
						}
						var ctorParams = params.slice(0,idx),
								fieldParams = params.slice(idx);
						var ctors = [ for (p in ctorParams) getArgExpr(p) ],
								fields = [ for (p in fieldParams) getFieldExpr(p) ];
						var ctor = getCtorExpr(cls, ctors, [ for (f in fields) f.retExpr ]);
						check(ctor.exprCheck, [ for (field in fields) field.exprCheck ]);
						// change the meta
						m.params[0] = ctor.retExpr;
						if (acc != null)
						{
							acc.remove('M');
							acc.add(':meta',[ctor.retExpr], m.pos);
						}
					case _:
				}
				ret.push(m);
			} else {
				ret.push(m);
			}
		}

		if (found) return ret;
		return meta;
	}

	private static function check(ctor:Expr, fields:Array<Expr>)
	{
		var block = { expr:EBlock(fields), pos:ctor.pos };
		typeof(macro {
			var __attrib__ = $ctor;
			$block;
		});
	}

	private static function getClassExpr(e:Expr):Expr
	{
		var t = clsType(e);
		var pos = e.pos;
		checkType(t,pos);
		var name = getNativeName(t,pos);
		return toFields(name,pos);
	}

	private static function toFields(name:String, pos:Position)
	{
		var names = name.split('.');
		function loop()
		{
			var name = names.pop();
			if (names.length == 0)
				return { expr:EConst(CIdent(name)), pos:pos };
			else
				return { expr:EField(loop(), name), pos:pos };
		}
		return loop();
	}

	private static function getCtorExpr(e:Expr, args:Array<{ exprCheck:Expr, retExpr:Expr }>, fields:Array<Expr>):{exprCheck:Expr, retExpr:Expr}
	{
		var t = clsType(e);
		var pos = e.pos;
		checkType(t,pos);
		var exprCheck = parse('new ${e.toString()}()',pos);
		switch(exprCheck.expr)
		{
			case ENew(v,_):
				exprCheck.expr = ENew(v,[ for (arg in args) arg.exprCheck ]);
			case _: throw 'assert';
		}

		var name = getNativeName(t,pos);
		var retExpr = { expr:ECall(toFields(name,pos), [ for (arg in args) arg.retExpr ].concat(fields)), pos:pos };

		return { exprCheck:exprCheck, retExpr: retExpr };
	}

	private static function getFieldExpr(arg:Expr):{ exprCheck:Expr, retExpr:Expr }
	{
		switch(arg.expr)
		{
			case EBinop(op = OpAssign | OpAssignOp(_), macro $i{field}, val):
				var argExpr = getArgExpr(val);
				return {
					exprCheck: { expr:EBinop(op, { expr:EField(macro __attrib__, field), pos:arg.pos }, argExpr.exprCheck), pos:arg.pos },
					retExpr: { expr:EBinop(op, macro $i{field}, argExpr.retExpr), pos: arg.pos }
				};
			case _:
				throw new Error('Invalid format for metadata. Expecting `fieldname = value`', arg.pos);
		}
	}

	private static function getArgExpr(arg:Expr):{ exprCheck:Expr, retExpr:Expr }
	{
		function mapRet(e:Expr)
		{
			return switch(e.expr)
			{
				case EBinop(OpOr, _, _)
				   | EParenthesis(_)
				   | EArrayDecl(_):
					e.map(mapRet);
				case EConst(CIdent('null' | 'true' | 'false') | CString(_) | CInt(_) | CFloat(_)):
					e;
				case EConst(CIdent(_)) | EField(_,_):
					var t = typeof(e);
					switch(t)
					{
						case TType(_.get() => { pack:[], name:name = ('Class' | 'Enum') }, _):
							var type = parse(getNativeName(assocType(t, e.pos), e.pos), e.pos);
							macro typeof($type);
						case _:
							e.map(mapRet);
					}
				case _:
					throw new Error("Unsupported expression iside @M metadata", e.pos);
			}
		}

		var retExpr = mapRet(arg);
		function mapCheck(e:Expr)
		{
			return switch(e.expr)
			{
				case EBinop(OpOr, e1, e2):
					var r1 = mapCheck(e1),
							r2 = mapCheck(e2);
					macro cs.internal.StrictMetaMacro.typeOr($r1,$r2);

				case EParenthesis(_):
					e.map(mapRet);
				case EArrayDecl(el):
					var r = e.map(mapCheck);
					macro cs.internal.StrictMetaMacro.typeArray($r);
				case EConst(CIdent('null' | 'true' | 'false') | CString(_) | CInt(_) | CFloat(_)):
					e;
				case EConst(CIdent(_)) | EField(_,_):
					var t = typeof(e);
					switch(t)
					{
						case TType(_.get() => { pack:[], name:name = ('Class' | 'Enum') }, _):
							if (name == 'Class')
								macro cs.Lib.toNativeType($e);
							else
								macro cs.Lib.toNativeEnum($e);
						case _:
							e;
					}
				case _:
					throw new Error("Unsupported expression iside @M metadata", e.pos);
			}
		}
		return { exprCheck:mapCheck(arg), retExpr: retExpr };
	}

	private static function getNativeName(t:Type, pos)
	{
		var bt:BaseType = null;
		switch (follow(t))
		{
			case TInst(i,_):
				bt = i.get();
			case TEnum(e,_):
				bt = e.get();
			case TAbstract(a,_):
				bt = a.get();
			case _:
				throw new Error('Unexpected type', pos);
		}

		var i = bt;
		for (meta in i.meta.get())
		{
			if (meta.name == ':native' && meta.params != null && meta.params.length == 1)
				switch (meta.params[0].expr)
				{
					case EConst(CString(s)):
						return s;
					case _:
				}
		}
		return i.pack.join('.') + (i.pack.length == 0 ? '' : '.') + i.name;
	}

	private static function checkType(t:Type, pos:Position)
	{
		if (!unify(t,getType('cs.system.Attribute')))
		{
			throw new Error('The type $t does not derive from cs.system.Attribute and thus cannot be a valid attribute',pos);
		}
		switch(t)
		{
			case TInst(c,_):
				c.get().meta.add(':keep',[],pos);
			case _:
		}
	}

	private static function clsType(e:Expr):Type
	{
		try
		{
			var t = typeof(e);
			return assocType(t,e.pos);
		}
		catch(err:Error)
		{
			function addAttribute(e:Expr)
			{
				return switch(e.expr)
				{
					case EConst(CIdent(i)):
						{ expr:EConst(CIdent(i + 'Attribute')), pos:e.pos };
					case _:
						e.map(addAttribute);
				}
			}
			var e2 = addAttribute(e);
			try
			{
				var t = typeof(e2);
				return assocType(t,e.pos);
			}
			catch(err2:Error)
			{
				throw err;
			}
		}
	}

	private static function assocType(t:Type, pos:Position):Type
	{
		return switch (follow(t))
		{
			case TAnonymous(anon):
				switch (anon.get().status)
				{
					case AClassStatics(t):
						TInst(t, [ for (t in t.get().params) t.t ]);
					case AEnumStatics(t):
						TEnum(t, [ for (t in t.get().params) t.t ]);
					case AAbstractStatics(t):
						TAbstract(t, [ for (t in t.get().params) t.t ]);
					case _:
						throw new Error('Invalid type $t',pos);
				}
			case _:
				throw new Error('Invalid type $t',pos);
		}
	}
#else

	// typing helpers
	@:extern inline public static function typeArray<T>(arr:Array<T>):cs.NativeArray<T>
	{
		return null;
	}

	@:extern inline public static function typeOr<T>(e1:T, e2:T):T
	{
		return e1;
	}
#end
}
