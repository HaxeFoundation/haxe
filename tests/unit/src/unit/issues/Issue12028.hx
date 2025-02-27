package unit.issues;

private typedef StateHandler<T> = {
    public function onUpdate():T;
}

class Issue12028 extends Test {
	#if !lua
	function testMakeVarArgsInDynamic() {
		var func = function(args:Array<Dynamic>):String {
			return args.length >= 1 ? args[0] : "";
		};

		var f2 = Reflect.makeVarArgs(func);
		eq("a", f2("a")); // Important for repro
		eq("b", foo(f2));
		eq("d", Reflect.callMethod(null, f2, ["d"]));
	}

	function foo(func:Dynamic) {
		return func("b", "c");
	}

	function testVoid2Dyn() {
		var handlers:StateHandler<Void> = {
			onUpdate: function():Bool {
				return true;
			}
		}
		foo2(handlers);
		noAssert();
	}

	function foo2<T>(handlers:StateHandler<T>):Null<T> {
		return handlers.onUpdate();
	}
	#end
}
