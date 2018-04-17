package python.lib.ssl;

@:require(python_version >= 3.4)
@:pythonImport("ssl", "Purpose")
extern class Purpose {
	public static var SERVER_AUTH:String;
	public static var CLIENT_AUTH:String;
}