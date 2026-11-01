package test;

import haxe.macro.Context;
import haxe.macro.Expr;

private class Bar {
	public function new() { }
	public function getValue() {
		return "foo";
	}
}

class Main
{
    static function main()
    {
        defineFooExtendsBarInLocalModule();
		#if !macro
		var foo = new Foo();
		Sys.stderr().writeString(foo.getValue());
		#end
    }

    macro static function defineFooExtendsBarInLocalModule(?e)
    {
        var infos = Context.getPosInfos(Context.currentPos());
        var position = Context.makePosition({min:0, max:0, file:infos.file});

        var superTypePath:TypePath =
        {
            pack: [],
            name: "Bar",
            sub: null
        }

        var kind:TypeDefKind = TypeDefKind.TDClass(superTypePath);

        var Foo:TypeDefinition =
        {
            name: "Foo",
            pack: ["test"],
            pos: position,
            kind: kind,
            fields: []
        }

        Context.defineModule(Context.getLocalModule(), [Foo]);

        return e;
    }
}