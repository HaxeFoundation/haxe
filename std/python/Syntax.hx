package python;

#if macro
import haxe.macro.Expr;

import haxe.macro.Context;

import haxe.macro.ExprTools;
#end

using haxe.macro.Tools;

extern class Syntax {

	#if macro
	static var self = macro python.Syntax;
	#end

	@:noUsing macro public static function importModule (module:String):haxe.macro.Expr {
		return macro ($self.pythonCode($v{"import " + module}):Void);
	}

	@:noUsing macro public static function importAs (module:String, className : String):haxe.macro.Expr
	{
		var n = className.split(".").join("_");
		var e = "import " + module + " as " + n;
		var e1 = "_hx_c."+n+" = "+n;

		return macro ({
			$self.pythonCode($v{e});
			$self.pythonCode($v{e1});
		}:Void);
	}

	@:noUsing
	macro public static function newInstance (c:Expr, params:Array<Expr>):haxe.macro.Expr {
		return macro $self._newInstance($c, $a{params});
	}

	static function _newInstance(c:Dynamic, args:Array<Dynamic>):Dynamic { return null; }

	@:noUsing
	public static function isIn(a:Dynamic, b:Dynamic):Bool { return false; }

	@:noUsing
	public static function delete(a:Dynamic):Void { }

	@:noUsing
	public static function binop(a:Dynamic, op:String, b:Dynamic):Dynamic { return null; }

	@:noUsing
	public static function assign(a:Dynamic, b:Dynamic):Void { }

	@:noUsing
	public static function pythonCode<T>(b:String):T { return null; };

	@:noUsing
	macro public static function arrayAccess(x:Expr, rest:Array<Expr>):ExprOf<Dynamic> {
		return macro $self._arrayAccess($x, $a{rest});
	}

	static function _arrayAccess(a:Dynamic, args:Array<Dynamic>):Dynamic { return null; }

	public static function arraySet(a:Dynamic, i:Dynamic, v:Dynamic):Dynamic { return null; }

	@:noUsing macro public static function arrayAccessWithLeadingColon<T>(x:Expr, rest:Array<Expr>):haxe.macro.ExprOf<Dynamic>
	{
		return macro ((untyped __python_array_access_leading_colon__)($a{[x].concat(rest)}):Dynamic);
	}

	@:noUsing macro public static function pyFor <T>(v:Expr, it:Expr, b:Expr):haxe.macro.Expr
	{
		var id = switch (v.expr) {
			case EConst(CIdent(x)):x;
			case _ : Context.error("unexpected " + ExprTools.toString(v) + ": const ident expected", v.pos);
		}

		var res = macro @:pos(it.pos) {
			var $id = $it.getNativeIterator().__next__();
			$it;
			$b;
		}
		return macro ((untyped __python_for__)($res):Void);
	}

	@:noUsing macro public static function importFromAs (from:String, module:String, className : String):haxe.macro.Expr {

		var n = className.split(".").join("_");

		var e = "from " + from + " import " + module + " as " + n;
		var e1 = "_hx_c."+n+" = " + n;
		return macro ({
			$self.pythonCode($v{e});
			$self.pythonCode($v{e1});
		}:Void);
	}

	@:noUsing
	macro public static function callField(o:Expr, field:ExprOf<String>, params:Array<Expr>):haxe.macro.Expr {
		var e = macro $self.call($self.field($o, $field), $a{params});
		e = macro untyped $e; // check "Dynamic should be Void" cases
		return e;
	}

	static function call(e:Dynamic, args:Array<Dynamic>):Dynamic { return null; }

	@:noUsing
	public static function field (o:Dynamic, field:String):Dynamic { return null; }

	@:noUsing
	macro public static function tuple(args:Array<Expr>):Dynamic {
		var args = macro $a{args};
		return macro $self._tuple($args);
	}

	static function _tuple(args:Array<Dynamic>):Dynamic { return null; }

	@:noUsing
	public static function varArgs(args:Array<Dynamic>):Dynamic { return null; }

	@:noUsing
	#if !macro macro #end
	public static function callNamed (e:Expr, args:Expr):haxe.macro.Expr {

		var fArgs = switch (Context.typeof(e)) {
			case TFun(args, ret): args;
			case _ : haxe.macro.Context.error("e must be of type function", e.pos);
		}

		switch (args.expr) {
			case EObjectDecl(fields):
				for (f in fields) {
					var found = false;
					for (a in fArgs) {
						found = a.name == f.field;
						if (found) break;
					}
					if (!found) {
						haxe.macro.Context.error("field " + f.field + " is not a valid argument (valid names " + [for (a in fArgs) a.name].join(",") + ")", args.pos);
					}
				}
				// TODO check at least if fields are valid (maybe if types match);
			case _ : haxe.macro.Context.error("args must be an ObjectDeclaration like { name : 1 }", args.pos);
		}
		return macro @:pos(e.pos) ((untyped __named__)($e, $args):Dynamic);
	}

	macro public static function callNamedUntyped (e:Expr, args:Expr):haxe.macro.Expr
	{
		return macro @:pos(e.pos) ((untyped __named__)($e, $args):Dynamic);
	}
}