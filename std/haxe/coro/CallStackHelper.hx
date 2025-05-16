package haxe.coro;

import haxe.CallStack;

class CallStackHelper {
	static public function cullTopStack(items:Array<StackItem>, skip = 0) {
		final topStack = [];
		for (item in items) {
			if (skip-- > 0) {
				continue;
			}
			switch (item) {
				// TODO: this needs a better check
				case FilePos(_, _, -1, _):
					break;
				// this is a hack
				case FilePos(Method(_, "invokeResume"), _):
					break;
				case _:
					topStack.push(item);
			}
		}
		return topStack;
	}
}