package python.lib.ssl;

@:pythonImport("ssl", "Purpose")
extern class Purpose {
	public static var SERVER_AUTH:String;
	public static var CLIENT_AUTH:String;
}