class Main {
	static function main() {
		#if !(mylib >= version("3.2.0"))
		#error "3.10.0 >= 3.2.0 failed"
		#end
		#if (mylib < version("3.9.0"))
		#error "3.10.0 < 3.9.0 should be false"
		#end
		#if !(mylib < version("3.10.1"))
		#error "3.10.0 < 3.10.1 failed"
		#end
		#if !(mylib == version("3.10.0"))
		#error "3.10.0 == 3.10.0 failed"
		#end
	}
}
