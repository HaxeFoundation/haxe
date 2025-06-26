class Main {
	static function __init__() {
		var hasBar = Sys.getEnv('bar');
	}

	public static function main() {
		Sys.environment().exists('foo');
	}
}