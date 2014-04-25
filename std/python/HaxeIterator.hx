package python;

import python.lib.Exceptions.StopIteration;
import python.lib.Types.NativeIterator;


class HaxeIterator<T>
{
	var it :NativeIterator<T>;
	var x:Null<T> = null;
	var has = false;
	var checked = false;

	public function new (it:NativeIterator<T>) {
		this.it = it;
	}

	public inline function next ():T {
		checked = false;
		return x;
	}

	public function hasNext ():Bool {
		if (checked) {
			return has;
		} else {
			try {
				x = it.__next__();
				has = true;
			} catch (s:StopIteration) {
				has = false;
				x = null;
			}
			checked = true;
			return has;
		}
	}
}