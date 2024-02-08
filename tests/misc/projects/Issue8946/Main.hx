class Main {
	static function main() {
		#if !EMPTY_FLAG
		throw "Missing branch matching -D EMPTY_FLAG=";
		#end
	}
}
