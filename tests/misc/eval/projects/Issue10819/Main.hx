class Main {
	static var a:Array<Int> = [1];

	static function main() {
		switch a { // <--- Warning : (WVarShadow) This variable shadows a previously declared variable
			case [1]:
			case _:
		}
	}
}
