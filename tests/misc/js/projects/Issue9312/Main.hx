class Main {
	static function main() {
		var fn:(Array<Int>)->Void;
		fn = function(a:Array<Int>):Void {
			for(i in a) {
				fn([i]);
			}
		}
		fn([1,2,3]);
	}
}