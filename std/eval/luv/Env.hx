package eval.luv;

/**
	Environment variables.

	@see https://aantron.github.io/luv/luv/Luv/Env
**/
extern class Env {
	/**
		Retrieves the value of an environment variable.
	**/
	static function getEnv(name:String):Result<NativeString>;

	/**
		Sets an environment variable.
	**/
	static function setEnv(name:String, value:NativeString):Result<haxe.NoData>;

	/**
		Retrieves all environment variables.
	**/
	static function environ():Result<Map<String,NativeString>>;
}