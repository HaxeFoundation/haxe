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
        var line = Const.__LINE__;

        var native = Global.debug_backtrace(Const.DEBUG_BACKTRACE_IGNORE_ARGS);
        var calledAt = new NativeAssocArray<Dynamic>();
        calledAt['function'] = '';
        calledAt['line'] = line;
        calledAt['file'] = Const.__FILE__;
        calledAt['class'] = '';
        calledAt['args'] = new NativeArray();
        Global.array_unshift(native, calledAt);

        return makeStack(native);
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
			lastExceptionTrace[i]['args'] = new NativeArray();
		}

        var thrownAt = new NativeAssocArray<Dynamic>();
        thrownAt['function'] = '';
        thrownAt['line'] = e.getLine();
        thrownAt['file'] = e.getFile();
        thrownAt['class'] = '';
        thrownAt['args'] = new NativeArray();
        Global.array_unshift(lastExceptionTrace, thrownAt);
	}

    static function makeStack (native:NativeTrace) : Array<StackItem> {
        var result = [];
        var count = Global.count(native);
        for (i in 0...count) {
            var entry = native[i];
            var next = (i + 1 < count ? native[i + 1] : null);
            var item = null;
            if ((next['function']:String).indexOf('{closure}') >= 0) {
                item = LocalFunction();
            } else if ((next['class']:String).length > 0 && (next['function']:String).length > 0) {
                var cls = Boot.getClass(next['class']);
                item = Method(cls.getName(), next['function']);
            }
            result.push(FilePos(item, entry['file'], entry['line']));
        }

        return result;
    }
}