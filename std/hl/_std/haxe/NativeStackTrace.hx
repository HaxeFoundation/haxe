package haxe;

import hl.NativeArray;
import hl.Bytes;
import haxe.CallStack.StackItem;

typedef Symbol = #if (hl_ver >= version("1.12.0")) hl.Abstract<"hl_symbol"> #else hl.Bytes #end

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
class NativeStackTrace {
	@:ifFeature('haxe.NativeStackTrace.exceptionStack')
	static public inline function saveStack(exception:Any):Void {
	}

	#if (hl_ver >= version("1.12.0") )

	static public function exceptionStack():NativeArray<Symbol> {
		var count = exceptionStackRaw(null);
		var arr = new NativeArray(count);
		exceptionStackRaw(arr);
		return arr;
	}

	static public inline function callStack():NativeArray<Symbol> {
		var count = callStackRaw(null);
		var arr = new NativeArray(count);
		callStackRaw(arr);
		// This will avoid errors when compiling hl/c on unix
		// See https://github.com/HaxeFoundation/haxe/pull/11382 for long term fix
		if (arr.length == 0) return arr;
		return arr.sub(1, arr.length - 1);
	}

	@:hlNative("std", "exception_stack_raw")
	static function exceptionStackRaw( arr : NativeArray<Symbol> ) : Int {
		return 0;
	}

	@:hlNative("std", "call_stack_raw")
	static function callStackRaw( arr : NativeArray<Symbol> ) : Int {
		return 0;
	}

	@:hlNative("std","resolve_symbol")
	static function resolveSymbol( sym : Symbol, buf : hl.Bytes, bufLen : hl.Ref<Int> ) : hl.Bytes {
		return null;
	}

	#else

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

	#end

	static public function toHaxe(native:NativeArray<Symbol>, skip:Int=0 ):Array<StackItem> {
		var stack = [];
		var r = ~/^([A-Za-z0-9.$_]+)\.([~A-Za-z0-9_]+(\.[0-9]+)?)\((.+):([0-9]+)\)$/;
		var r_fun = ~/^fun\$([0-9]+)\((.+):([0-9]+)\)$/;
		#if (hl_ver >= version("1.12.0"))
		var maxLen = 1024;
		var tmpBuf = @:privateAccess hl.Bytes.alloc(maxLen);
		#end
		for (i in 0...native.length-1) {
			if( i < skip ) continue;
			#if (hl_ver >= version("1.12.0"))
			var len = maxLen;
			var bytes = resolveSymbol(native[i],tmpBuf,len);
			if( bytes == null ) continue;
			#else
			var bytes = native[i];
			#end
			var str = @:privateAccess String.fromUCS2(bytes);
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