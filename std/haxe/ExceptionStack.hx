package haxe;

import haxe.CallStack.StackItem;

/**
	Call stack.

	TODO:
	Convert `haxe.CallStack` into `abstract CallStack(Array<StackItem>)` instead
	of using this workaround.
**/
@:forward(length,iterator)
abstract ExceptionStack(Array<StackItem>) from Array<StackItem> {

	@:arrayAccess public inline function get(index:Int):StackItem {
		return this[index];
	}

	public inline function copy():ExceptionStack {
		return this.copy();
	}

	/**
		Error description.
	**/
	public inline function toString():String {
		return CallStack.toString(this);
	}

	/**
		Returns a range of entries of current stack from the beginning to the the common part of this and `stack`.
	**/
	function subtract(stack:ExceptionStack):ExceptionStack {
		var startIndex = -1;
		var i = -1;
		while(++i < this.length) {
			for(j in 0...stack.length) {
				if(equalItems(this[i], stack[j])) {
					if(startIndex < 0) {
						startIndex = i;
					}
					++i;
					if(i >= this.length) break;
				} else {
					startIndex = -1;
				}
			}
			if(startIndex >= 0) break;
		}
		return startIndex >= 0 ? this.slice(0, startIndex) : this;
	}

	static function equalItems(item1:Null<StackItem>, item2:Null<StackItem>):Bool {
		return switch([item1, item2]) {
			case [null, null]: true;
			case [CFunction, CFunction]: true;
			case [Module(m1), Module(m2)]:
				m1 == m2;
			case [FilePos(item1, file1, line1, col1), FilePos(item2, file2, line2, col2)]:
				file1 == file2 && line1 == line2 && col1 == col2 && equalItems(item1, item2);
			case [Method(class1, method1), Method(class2, method2)]:
				class1 == class2 && method1 == method2;
			case [LocalFunction(v1), LocalFunction(v2)]:
				v1 == v2;
			case _: false;
		}
	}
}
