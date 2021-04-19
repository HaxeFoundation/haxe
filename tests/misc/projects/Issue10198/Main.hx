class Main {
	static public function fn1<T1,R1:T1>(v:R1):T1
		return null;

	static public function fn2<T2,R2:T2>(v:R2):T2
		return null;

	static function main() {
		var b:Int = fn1(fn2('s'));
	}
}