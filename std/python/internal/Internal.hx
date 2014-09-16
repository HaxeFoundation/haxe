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
	static var _props = _prefix + "props";
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
			case EConst(CString(x)): macro @:pos(Context.currentPos()) $v{_prefix + x};
			case _ : macro @:pos(Context.currentPos()) (python.Syntax.binop($v{_prefix},"+",$x):String);
		}
	}



	static function withPos(x:String):Expr {
		return macro @:pos(Context.currentPos()) $v{x};
	}

	static function fieldWithPos(o:Expr, x:String):Expr {
		return macro @:pos(Context.currentPos()) python.Syntax.field($o, $v{x});
	}

	static function has (o:Expr, field:String):Expr {
		return macro python.internal.HxBuiltin.hasattr($o, $v{field});
	}

	#end

	macro public static function getPrefixed (x:ExprOf<String>):Expr {
		return switch (x.expr) {
			case EConst(CString(x)): macro @:pos(Context.currentPos()) $v{_prefix + x};
			case _ : macro @:pos(Context.currentPos()) (python.Syntax.binop($v{_prefix},"+",$x):String);
		}
	}

	macro public static function classRegistry ():Expr {
		return macro (python.Syntax.pythonCode($v{_classes}) : python.lib.Dict<String, Class<Dynamic>>);
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

	macro public static function propsVal():Expr {
		return withPos(_props);
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

	macro public static function fieldProps (o:Expr):Expr {
		return fieldWithPos(o, _props);
	}

	macro public static function fieldConstructs (o:Expr):Expr {
		return fieldWithPos(o, _constructs);
	}

	macro public static function fieldDict (o:Expr):Expr {
		return fieldWithPos(o, _dict);
	}

	macro public static function fieldEmptyInit (o:Expr):Expr {
		return fieldWithPos(o, _emptyInit);
	}

	macro public static function callEmptyInit (o:Expr, instance:Expr):Expr {
		return macro @:pos(Context.currentPos()) python.Syntax.callField($o, $v{_emptyInit}, $instance);
	}
}