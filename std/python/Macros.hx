package python;

#if macro
import haxe.macro.Expr;

import haxe.macro.Context;

import haxe.macro.ExprTools;
#end
#if !macro

import python.lib.Types.PyIterator;
#end


class Macros {

	@:noUsing macro public static function importModule (module:String):haxe.macro.Expr {
		return macro untyped __python__($v{"import " + module});
	}

	@:noUsing macro public static function importAs (module:String, className : String):haxe.macro.Expr
    {

        var n = className.split(".").join("_");

        var e = "import " + module + " as " + n;
        var e1 = "_hx_c."+n+" = "+n;


	    return macro{
            untyped __python__($v{e});
            untyped __python__($v{e1});
        }
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
        return macro (untyped __python_for__)($res);
    }

    @:noUsing macro public static function importFromAs (from:String, module:String, className : String):haxe.macro.Expr {

        var n = className.split(".").join("_");

        var e = "from " + from + " import " + module + " as " + n;
        var e1 = "_hx_c."+n+" = " + n;
	    return macro {
            untyped __python__($v{e});
            untyped __python__($v{e1});
        }
    }

    #if !macro macro #end public static function callNamed (e:Expr, args:Expr):haxe.macro.Expr {
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
        return macro @:pos(e.pos) untyped __named__($e, $args);
    }


}