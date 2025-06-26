package unit.issues;

import haxe.Constraints.IMap;
import haxe.Constraints.Constructible;
import haxe.ds.EnumValueMap;

class Issue9366 extends unit.Test {
	function test() {
		eq("ok", foo());
	}

	public static macro function foo() {
		return macro $v{localVars.name()};
	}

	static final localVars:VarManager<En, EnumValueMap<En, String>> = new VarManager();
}

enum En {
	A;
}

@:generic
private class VarManager<K, M:IMap<K, String> & Constructible<()->Void>> {
	final nameToVarKey:Map<String, K> = new Map();

	public function new() {}

	public function name() {
		var f = s -> nameToVarKey.exists(s);
		return 'ok';
	}
}