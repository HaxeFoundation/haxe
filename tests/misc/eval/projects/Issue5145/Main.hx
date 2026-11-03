import haxe.macro.Context;
import haxe.macro.Type;
import haxe.macro.Expr;
using haxe.macro.Tools;

interface IA {}
interface IB {}
interface IC extends IA extends IB {}

class Main {

    static function main() test();

    macro static function test() {

        function get_type_path(ct:ComplexType) return switch ct {
            case TPath(tp):tp;
            case _:throw("bad");
        };

        var td = macro class ID {};
        var ia = macro : Main.IA;
        var ib = macro : Main.IB;
        var ic = macro : Main.IC;
        td.kind = TDClass(null,[ia,ib,ic].map(get_type_path),true);
        Context.defineType(td);
        var t = Context.getType("ID");
        switch t {
            case TInst(_.get()=>tt,_):
                if (tt.interfaces.length != 3)
                    Context.error("Number of extended interfaces must be 3",Context.currentPos());
            case _:
        }

        var td = macro class IE {};
        var ia = macro : Main.IA;
        var ib = macro : Main.IB;
        var ic = macro : Main.IC;

        var ia_tpath = switch ia {
            case TPath(tp):tp;
            case _:Context.error("must be TPath",Context.currentPos());
        }
        td.kind = TDClass(ia_tpath,[ib,ic].map(get_type_path),true);
        Context.defineType(td);

        var t = Context.getType("IE");
        switch t {
            case TInst(_.get()=>tt,_):
                if (tt.interfaces.length != 3)
                    Context.error("Number of extended interfaces must be 3",Context.currentPos());
            case _:
        }

        var reification = macro interface IF extends Main.IA extends Main.IB {

        };
        Context.defineType(reification);
        var t = Context.getType("IF");
        switch t {
            case TInst(_.get()=>tt,_):
                if (tt.interfaces.length != 2){
                    Context.error("Number of extended interfaces must be 2",Context.currentPos());
                }
            case _:
        }
        return macro null;
    }
}