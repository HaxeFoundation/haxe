package neko;

class File {

	public static function getContent( filename : String ) {
		return new String(_content(untyped filename.__s));
	}

	private static var _content = Lib.load("std","file_contents",1);

}