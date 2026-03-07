class Main {
	public static function main():Void foo([]);
	static function foo<T>(map:Map<T, Int>):Void for (k => v in map) {};
}