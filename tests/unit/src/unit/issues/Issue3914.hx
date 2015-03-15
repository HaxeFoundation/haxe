package unit.issues;

class Issue3914 extends Test {
    #if macro
    static var storedExpr:haxe.macro.Expr;
    #end

    function test() {
        storeExpr(function(a:Array<Int>) {
            for (i in a) {
                try throw i catch (v:Dynamic) {
                    var v2 = v;
                    return v2;
                }
            }
            throw false;
        });
        eq(getStoredExpr()([3,2,1]), 3);
    }

    function test2() {
        storeExpr(function(a:Array<Int>) {
            for (i in a) {
                try throw i catch (v:Dynamic) {
                    var v2 = v;
                    return v2;
                }
            }
            throw false;
        });
        check2();
    }

    function check2() {
        eq(getStoredExpr()([3,2,1]), 3);
    }

    function test3() {
        store3();
        eq(getStoredExpr()([3,2,1]), 3);
    }

    function store3() {
        storeExpr(function(a:Array<Int>) {
            for (i in a) {
                try throw i catch (v:Dynamic) {
                    var v2 = v;
                    return v2;
                }
            }
            throw false;
        });
    }

    static macro function storeExpr(e) {
        storedExpr = haxe.macro.Context.storeTypedExpr(haxe.macro.Context.typeExpr(e));
        return macro {};
    }

    static macro function getStoredExpr() return storedExpr;
}
