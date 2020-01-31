package haxe;

import haxe.CallStack.StackItem;

/**
	Call stack.

	TODO:
	Convert `haxe.CallStack` into `abstract CallStack(Array<StackItem>)` instead
	of using this workaround.
**/
@:forward(length)
abstract ErrorStack(Array<StackItem>) from Array<StackItem> {

	@:arrayAccess public inline function get(index:Int):StackItem {
		return this[index];
	}

	public inline function copy():ErrorStack {
		return this.copy();
	}

	/**
		Error description.
	**/
	public inline function toString():String {
		return CallStack.toString(this);
	}
}
