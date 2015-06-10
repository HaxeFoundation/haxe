package unit.issues;

import unit.issues.misc.Issue4306Macro.TypeName;
import unit.issues.misc.Issue4306Macro.Arity;
import unit.issues.misc.Issue4306Macro.EnumListener;

private enum E {
	Message(s:String);
	NoMessage;
}

class Issue4306 extends Test {
	function test() {
		eq("String", getTypeName("foo"));
		eq("Int", getTypeName(12));
		eq("haxe.Template", getTypeName(new haxe.Template("foo")));

		eq(0, getArity(test));
		eq(1, getArity(getTypeName));
		eq(2, getArity(function(x, y) { }));

		function onMessage(s:String) { }
		function onNoMessage() { }

		eq("Message", addEnumListener(Message, onMessage));
		t(unit.TestType.typeError(addEnumListener(Message, onNoMessage)));
		eq("NoMessage", addEnumListener(NoMessage, onNoMessage));
		t(unit.TestType.typeError(addEnumListener(NoMessage, onMessage)));
	}

	static function getTypeName(t:TypeName) {
		return t;
	}

	static function getArity(a:Arity) {
		return a;
	}

	static function addEnumListener<T>(e:EnumListener<T>, f:T) {
		return e;
	}
}