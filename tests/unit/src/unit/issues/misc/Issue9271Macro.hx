package unit.issues.misc;

#if macro
class Issue9271Macro {
	static function build() {
		return macro : String;
	}
}

class Issue9271MacroSub {
	static function build() {
		return macro : Array<Int>;
	}
}
#end