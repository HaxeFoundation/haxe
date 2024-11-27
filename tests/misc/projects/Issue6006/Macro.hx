import haxe.macro.Context;
import haxe.macro.Expr;

class Macro {
    static function build() {
        var pos = Context.currentPos();
        Context.defineModule(Context.getLocalModule(), [{
            pos: pos,
            pack: [],
            name: "A",
            kind: TDAbstract(macro : Int),
            fields: [
                {
                    pos: pos,
                    name: "new",
                    access: [APublic],
                    kind: FFun({
                        args: [],
                        ret: null,
                        expr: macro this = 1,
                    })
                }
            ]
        }]);
        return macro : A;
    }
}