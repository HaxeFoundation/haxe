package unit.issues;

class Issue8243 extends unit.Test {
#if !(hl || cpp || java) // TODO https://github.com/HaxeFoundation/haxe/issues/8243

	var involveRecursiveAbstractTyping:Null<Rec> = null;

	function test() {
		t(involveRecursiveAbstractTyping == null);
	}
#end
}

private abstract Rec(Array<Rec>) {}