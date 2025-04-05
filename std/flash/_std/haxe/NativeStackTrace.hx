package haxe;

import flash.errors.Error;
import haxe.CallStack.StackItem;

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
@:allow(haxe.Exception)
class NativeStackTrace {
	@:ifFeature('haxe.NativeStackTrace.exceptionStack')
	static public inline function saveStack(e:Any):Void {
	}

	static public inline function callStack():String {
		return normalize(new Error().getStackTrace(), 1);
	}

	static public function exceptionStack():String {
		var err:Null<Error> = untyped flash.Boot.lastError;
		return err == null ? '' : normalize(err.getStackTrace());
	}

	static public function toHaxe(native:String, skip:Int = 0):Array<StackItem> {
		var a = new Array();
		var r = ~/at ([^\/]+?)\$?(\/[^\(]+)?\(\)(\[(.*?):([0-9]+)\])?/;
		var rlambda = ~/^MethodInfo-([0-9]+)$/g;
		var cnt = 0;
		while (r.match(native)) {
			native = r.matchedRight();
			if(skip > cnt++) {
				continue;
			}
			var cl = r.matched(1).split("::").join(".");
			var meth = r.matched(2);
			var item;
			if (meth == null) {
				if (rlambda.match(cl))
					item = LocalFunction(Std.parseInt(rlambda.matched(1)));
				else
					item = Method(cl, "new");
			} else
				item = Method(cl, meth.substring(1));
			if (r.matched(3) != null)
				item = FilePos(item, r.matched(4), Std.parseInt(r.matched(5)));
			a.push(item);
		}
		return a;
	}

	static function normalize(stack:String, skipItems:Int = 0):String {
		switch (stack:String).substring(0, 6) {
			case 'Error:' | 'Error\n': skipItems += 1;
			case _:
		}
		return skipLines(stack, skipItems);
	}

	static function skipLines(stack:String, skip:Int, pos:Int = 0):String {
		return if(skip > 0) {
			pos = stack.indexOf('\n', pos);
			return pos < 0 ? '' : skipLines(stack, --skip, pos + 1);
		} else {
			return stack.substring(pos);
		}
	}
}