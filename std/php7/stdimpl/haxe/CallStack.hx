package php7.stdimpl.haxe;

import haxe.CallStack;


private typedef NativeTrace = NativeIndexedArray<NativeAssocArray<Dynamic>>;

@:dox(hide)
@:noCompletion
class CallStack {
    static var lastExceptionTrace : NativeTrace;

    /**
		Return the call stack elements, or an empty array if not available.
	**/
    public static inline function callStack() : Array<StackItem> {
        return makeStack(Global.debug_backtrace(Const.DEBUG_BACKTRACE_IGNORE_ARGS));
    }

    /**
		Return the exception stack : this is the stack elements between
		the place the last exception was thrown and the place it was
		caught, or an empty array if not available.
	**/
	public static inline function exceptionStack() : Array<StackItem> {
        return makeStack(lastExceptionTrace == null ? new NativeIndexedArray() : lastExceptionTrace);
    }

    @:keep
	static function saveExceptionTrace( e:Throwable ) : Void {
		lastExceptionTrace = e.getTrace();
		var count = php7.Global.count(lastExceptionTrace);
		for (i in 0...count) {
			lastExceptionTrace[i]['args'] = new NativeAssocArray();
		}
	}

    static function makeStack (native:NativeTrace) : Array<StackItem> {
        var result = [];
        var item : StackItem;
        for (e in native) {
            var entry:NativeAssocArray<Dynamic> = cast e; //WTF? $type(e) is String ???
            item = null;
            if ((entry['function']:String).indexOf('{closure}') >= 0) {
                item = LocalFunction();
            } else if ((entry['class']:String).length > 0 && (entry['function']:String).length > 0) {
                var cls = Boot.getClass(entry['class']);
                item = Method(cls.getName(), entry['function']);
            }
            result.push(FilePos(item, entry['file'], entry['line']));
        }

        return result;
    }
}