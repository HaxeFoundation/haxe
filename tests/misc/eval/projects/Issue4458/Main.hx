class Main {
	static function main() {
		#if (((a > 1)) || (b > 2 && c > 3))
		#error "Expected failure"
		#end        
	}
}
