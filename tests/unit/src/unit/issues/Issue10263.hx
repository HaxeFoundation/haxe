package unit.issues;

private typedef State = {
	var foo:Map<String, Any>;
}

private typedef Props = {}

class Issue10263 extends Test {
	static var lastState:State;

	function test() {
		eq("State->State", setState(state -> {
			state.foo.set("foo", 1);
			state.foo.set("foo", '1');
			return state;
		}));
		eq("1", lastState.foo.get("foo"));
	}

	extern inline overload static function setState(v:State, ?cb:Void->Void):String {
		return "State";
	}

	extern inline overload static function setState(v:State->State, ?cb:Void->Void):String {
		lastState = v({foo: new Map()});
		return "State->State";
	}

	extern inline overload static function setState(v:State->Props->State, ?cb:Void->Void):String {
		return "State->Props->State";
	}
}