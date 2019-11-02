package asys;

extern class System {
	/**
		Returns the current value of the environment variable `name`, or `null` if
		it does not exist.
	**/
	public static function getEnv(name:String):Null<String>;

	/**
		Sets the value of the environment variable `name` to `value`. If `value` is
		`null`, the variable is deleted.
	**/
	public static function setEnv(name:String, value:Null<String>):Void;
}
