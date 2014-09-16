package python;

#if macro
import haxe.macro.Expr;
import haxe.macro.Context;
import haxe.macro.ExprTools;
#end

@:noPackageRestrict
extern class Syntax {

	#if macro
	static var self = macro python.Syntax;
	#end

	@:noUsing macro public static function importModule (module:String):haxe.macro.Expr {
		return macro ($self.pythonCode($v{"import " + module}):Void);
	}

	@:noUsing macro public static function importAs (module:String, className : String):haxe.macro.Expr {
		var n = className.split(".").join("_");
		var e = "import " + module + " as " + n;

		return macro ($self.pythonCode($v{e}):Void);
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
	macro public static function pythonCode(b:ExprOf<String>, rest:Array<Expr>):Expr {
		if (rest == null) rest = [];
		return macro @:pos(Context.currentPos()) $self._pythonCode($b, $a{rest});
	};

	#if !macro
	@:noUsing
	public static function _pythonCode<T>(b:String, args:Array<Dynamic>):T { return null; };
	#end
	@:noUsing
	macro public static function arrayAccess(x:Expr, rest:Array<Expr>):ExprOf<Dynamic> {
		return macro $self._arrayAccess($x, $a{rest});
	}

	@:noUsing
	macro public static function arrayAccessWithTrailingColon(x:Expr, rest:Array<Expr>):ExprOf<Dynamic> {
		return macro $self._arrayAccess($x, $a{rest}, true);
	}

	static function _arrayAccess(a:Dynamic, args:Array<Dynamic>, ?trailingColon:Bool = false):Dynamic { return null; }

	@:noUsing
	public static function arraySet(a:Dynamic, i:Dynamic, v:Dynamic):Dynamic { return null; }


	static function _foreach(id:Dynamic, it:Dynamic, block:Dynamic):Dynamic { return null; }


	@:noUsing
	macro public static function foreach <T>(v:Expr, it:Expr, b:Expr):haxe.macro.Expr
	{
		var id = switch (v.expr) {
			case EConst(CIdent(x)):x;
			case _ : Context.error("unexpected " + ExprTools.toString(v) + ": const ident expected", v.pos);
		}
		var iter = try {
			Context.typeof(macro $it.__iter__());
			macro $it.__iter__().getNativeIteratorRaw();
		} catch (e:Dynamic) {
			macro $it.getNativeIteratorRaw();
		};


		return macro {
			// the first 2 expressions are only used to create a typing context for the foreach construct
			// TODO how can we get rid of them, so that they are not generated?
			var $id = null;
			if (false) $v = $iter.__next__();
			$self._foreach($v, $it, $b);
		}
	}

	@:noUsing macro public static function importFromAs (from:String, module:String, className : String):haxe.macro.Expr {

		var n = className.split(".").join("_");

		var e = "from " + from + " import " + module + " as " + n;

		return macro ($self.pythonCode($v{e}):Void);
	}

	@:noUsing
	macro public static function callField(o:Expr, field:ExprOf<String>, params:Array<Expr>):haxe.macro.Expr {
		return macro @:pos(o.pos) $self.call($self.field($o, $field), $a{params});
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

	macro public static function callNamedUntyped (e:Expr, args:Expr):Expr {
		return macro @:pos(e.pos) $self._callNamedUntyped($e, $args);
	}

	static function _callNamedUntyped(e:Dynamic, args:Dynamic):Dynamic { return null; }

	public static function opPow(a:Int, b:Int):Int { return 0; }
}