package unit.issues;

class Issue7362 extends unit.Test {
	function test() {
		// postfix @:op(A!)
		var post:Post<Int> = 7;
		eq(7, post!);
		var badPost:Post<Int> = null;
		eq("unwrap null in test", try { badPost!; "?"; } catch (e:String) e);

		// prefix @:op(!A)
		var pre:Pre<Int> = 5;
		eq(5, !pre);
		var badPre:Pre<Int> = null;
		eq("unwrap null in test", try { !badPre; "?"; } catch (e:String) e);
	}
}

private abstract Post<T>(Null<T>) from Null<T> {
	@:op(A!) public inline function unwrap(?pos:haxe.PosInfos):T {
		if (this == null) throw 'unwrap null in ${pos.methodName}';
		return this;
	}
}

private abstract Pre<T>(Null<T>) from Null<T> {
	@:op(!A) public inline function unwrap(?pos:haxe.PosInfos):T {
		if (this == null) throw 'unwrap null in ${pos.methodName}';
		return this;
	}
}
