package callstack;

import haxe.CallStack;
using StringTools;

enum CallStackInspect {
	File(file:String);
	Line(line:Int);
	Skip(file:String);
}

class CallStackInspectorFailure extends haxe.Exception {
	public function new(reason:String) {
		super(reason);
	}
}

class CallStackInspector {
	final stack:Array<StackItem>;
	var offset:Int;
	var expectedFile:Null<String>;
	var performedTests:Int;
	var inspectOffset:Int;

	public function new(stack:Array<StackItem>) {
		this.stack = stack;
		offset = 0;
		inspectOffset = -1;
		performedTests = 0;
	}

	public function inspect(items:Array<CallStackInspect>) {
		try {
			for (item in items) {
				doInspect(item);
			}
			return null;
		} catch (e:CallStackInspectorFailure) {
			return e;
		}
	}

	function fail(inspect: CallStackInspect, reason:String) {
		throw new CallStackInspectorFailure('Failure at stack offset $offset, inspect offset $inspectOffset with $inspect: $reason');
	}

	function doInspect(inspect:CallStackInspect) {
		++inspectOffset;
		switch (inspect) {
			case File(file):
				this.expectedFile = file;
			case Line(expectedLine):
				final index = offset++;
				switch (stack[index]) {
					case FilePos(_, file, line):
						if (!file.endsWith(expectedFile)) {
							fail(inspect, 'file $file should be $expectedFile');
						}
						performedTests++;
						if (line != expectedLine) {
							fail(inspect, 'line $line should be $expectedLine');
						}
						performedTests++;
					case v:
						fail(inspect, '$v should be FilePos');
				}
			case Skip(file):
				while (true) {
					if (offset == stack.length) {
						fail(inspect, '$offset went out of bounds while skipping until $file');
					}
					switch (stack[offset]) {
						case FilePos(Method(_), file2, _) if (file2.endsWith(file)):
							expectedFile = file;
							break;
						case _:
							offset++;
					}
				}
		}
	}
}