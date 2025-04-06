@:nullSafety(StrictThreaded)
class Main {
	static function main() {
		var a = 1;
		{-1-}a = null{-2-};
	}
}
