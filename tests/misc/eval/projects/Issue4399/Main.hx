class Main {
	static function main() {
		Sys.stderr().writeString("" + foo(2));
	}

	static var array:Array<Int> = [1,2,3,4,5];

	static function foo(value:Int):Int {
		var i:Int = 0;
		var m:Int = 0;
		while(i < array.length) {
			if(array[i] == value) {
				m = i;
			}
			++i;
		}
		return m;
	}
}