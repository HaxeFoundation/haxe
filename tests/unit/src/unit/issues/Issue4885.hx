package unit.issues;

private class Context {

	public var name:String;

	public function new(n:String):Void {
		name = n;
	}

}

class Issue4885 extends Test {
	function test() {
        var
            raw:String = "My name is ::name::.",
            context:Context = new Context("Joan"),
            template:haxe.Template = new haxe.Template(raw),
            output:String = template.execute(context)
        ;
        eq("My name is Joan.", output);
	}
}