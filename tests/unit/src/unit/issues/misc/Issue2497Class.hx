package unit.issues.misc;

class Issue2497Class {
	public static var addTail: String -> String -> ?String-> String = actualFx;
	public static function actualFx(str1:String, str2:String, ?str3:String = "!") {
		return str1 + " " + str2 + " " + str3;
	}
}