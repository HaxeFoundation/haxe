package unit.issues;

class Issue8243 extends unit.Test {
#if !hl // TODO https://github.com/HaxeFoundation/haxe/issues/8243

	var involveRecursiveAbstractTyping:Null<Rec> = [[[[]]]];

	function test() {
		eq('[[[[]]]]', involveRecursiveAbstractTyping.toString());
	}
#end
}

@:forward(toString)
private abstract Rec(Array<Rec>) from Array<Rec> {}