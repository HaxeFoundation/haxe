package haxe;

import flash.errors.Error;
import haxe.CallStack.StackItem;

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
class NativeStackTrace {
	static public inline function saveStack(e:Any):Void {
	}

	static public inline function callStack():String {
		return new Error().getStackTrace();
	}

	static public function exceptionStack():String {
		var err:Null<Error> = untyped flash.Boot.lastError;
		return err == null ? '' : err.getStackTrace();
	}

	static public function toHaxe(native:String, skip:Int = 0):Array<StackItem> {
		var a = new Array();
		var r = ~/at ([^\/]+?)\$?(\/[^\(]+)?\(\)(\[(.*?):([0-9]+)\])?/;
		var rlambda = ~/^MethodInfo-([0-9]+)$/g;
		var cnt = 0;
		while (r.match(native)) {
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
				item = Method(cl, meth.substr(1));
			if (r.matched(3) != null)
				item = FilePos(item, r.matched(4), Std.parseInt(r.matched(5)));
			a.push(item);
			native = r.matchedRight();
		}
		return a;
	}
}