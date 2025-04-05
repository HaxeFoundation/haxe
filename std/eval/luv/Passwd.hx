package eval.luv;

typedef PasswdData = {
	var username:String;
	var uid:Int;
	var gid:Int;
	var shell:Null<String>;
	var homedir:NativeString;
}

/**
	Current user information.

	@see https://aantron.github.io/luv/luv/Luv/Passwd
**/
extern class Passwd {
	/**
		Gets passwd entry for the current user.
	**/
	static function getPasswd():Result<PasswdData>;
}