import safety.OutOfBoundsException;

/**
 *  `SafeArray` throws `safety.OutOfBoundsException` when trying to read or write out of array bounds.
 *  Writing at an index of array's length is allowed.
 *  ```
 *  	var a:SafeArray<String> = ["hello"];
 *  	trace(a[100]); //exception
 *  	a[1] = "world"; //ok
 *  	a[100] = "too far"; //exception
 *  ```
 */
@:forward
abstract SafeArray<T>(Array<T>) from Array<T> {
	public var length(get,never):Int;
	inline function get_length() return this.length;

	@:arrayAccess inline function get(index:Int):T {
		if(index < 0 || index >= this.length) {
			throw new OutOfBoundsException('Reading out of array bounds. Array length: ${this.length}. Accessed index: $index.');
		}
		return this[index];
	}

	@:arrayAccess inline function set(index:Int, value:T):T {
		if(index < 0 || index > this.length) {
			throw new OutOfBoundsException('Writing out of array bounds. Array length: ${this.length}. Accessed index: $index.');
		}
		return this[index] = value;
	}

	public inline function stdArray():Array<T> {
		return this;
	}
}