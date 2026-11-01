import haxe.macro.Expr;
import haxe.macro.*;

class Base {}
class Extended extends Base {}

class Builder {
    #if macro
    static public function checkUnify():Void {
        var baseType = ComplexTypeTools.toType(macro:Base);
        var extendedType = ComplexTypeTools.toType(macro:Extended);
        var unify = Context.unify(extendedType, baseType);
        Sys.stderr().writeString("Extended unifies Base? " + unify + "\n");
    }
    #end
    macro static public function build():Array<Field> {
        Sys.stderr().writeString("in @:build\n");
        checkUnify();
        var fields = Context.getBuildFields();
        return fields;
    }
}

#if !macro
@:build(Builder.build())
#end
class Main {
    var extended:Extended;

    macro static function checkUnify() {
        Sys.stderr().writeString("in macro function\n");
        Builder.checkUnify();
        return macro {};
    }

    static function main() {
        checkUnify();
    }
}