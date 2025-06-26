package unit.issues;

@mark({oh: "hi"})
class Issue10041 extends unit.Test {
	function test() {
		eq("hi", haxe.rtti.Meta.getType(Issue10041).mark[0].oh);
	}
}