class Main {
	#if !macro
	static function main() {
		// a macro function's ?pos is auto-filled and cannot be passed explicitly
		foo(@:posInfos here());
	}

	static function here():haxe.PosInfos return null;
	#end

	macro static function foo(?pos:haxe.PosInfos) {
		return macro null;
	}
}
