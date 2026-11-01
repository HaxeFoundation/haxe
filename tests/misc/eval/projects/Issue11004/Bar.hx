package;

#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;

using haxe.macro.ExprTools;
using haxe.macro.TypeTools;
#end

class Bar
{
  public function new()
  {
    trace('New Bar');
  }

  public function nonmacro_func(val:String)
  {
    trace('Hello runtime: $val');
  }

  public macro function macro_func(this_expr:Expr) // :this (should refer to the Bar instance on the Foo)
  {
    var this_ident:String = get_this_ident(this_expr);
    trace('${ this_expr.toString() } computed this_ident as: ${ this_ident }');

    var code = '${ this_ident }.nonmacro_func("${ this_ident }")';
    return Context.parse(code, Context.currentPos());
  }

  #if macro
  static function get_this_ident(this_expr:Expr):String
  {
    // Read the ident from the source code
    return switch (this_expr.expr) {
      case EMeta(_, e):
        var info = Context.getPosInfos(this_expr.pos);
        var bytes = sys.io.File.getBytes(info.file);
        bytes.getString(info.min, info.max-info.min);
      default:
        // Not in the above form? Hmm...
        trace('Unexpected this resolution: ${ this_expr.toString() }');
        this_expr.toString();
    }
  }
  #end
}
