package haxe;

import hl.NativeArray;
import hl.Bytes;
import haxe.CallStack.StackItem;

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
class NativeStackTrace {
	@:ifFeature('haxe.NativeStackTrace.exceptionStack')
	static public inline function saveStack(exception:Any):Void {
	}

	@:hlNative("std", "exception_stack")
	static public function exceptionStack():NativeArray<Bytes> {
		return null;
	}

	//TODO: implement in hashlink like `exceptionStack`
	static public function callStack():NativeArray<Bytes> {
		var stack:NativeArray<Bytes> = try {
			throw new Exception('', null, 'stack');
		} catch (e:Exception) {
			exceptionStack();
		}
		var skip = 1;
		for(i in 0...stack.length - 1) {
			var s = @:privateAccess String.fromUCS2(stack[i]);
			if(s.indexOf('NativeStackTrace.callStack') < 0) {
				break;
			}
			skip++;
		}
		return skip < stack.length ? stack.sub(skip, stack.length - skip) : stack;
	}

	static public function toHaxe(native:NativeArray<Bytes>, skip:Int = 0):Array<StackItem> {
		var stack = [];
		var r = ~/^([A-Za-z0-9.$_]+)\.([~A-Za-z0-9_]+(\.[0-9]+)?)\((.+):([0-9]+)\)$/;
		var r_fun = ~/^fun\$([0-9]+)\((.+):([0-9]+)\)$/;
		for (i in 0...native.length - 1) {
			if(skip > i) {
				continue;
			}
			var str = @:privateAccess String.fromUCS2(native[i]);
			if (r.match(str))
				stack.push(FilePos(Method(r.matched(1), r.matched(2)), r.matched(4), Std.parseInt(r.matched(5))));
			else if (r_fun.match(str))
				stack.push(FilePos(LocalFunction(Std.parseInt(r_fun.matched(1))), r_fun.matched(2), Std.parseInt(r_fun.matched(3))));
			else
				stack.push(Module(str));
		}
		return stack;
	}
}