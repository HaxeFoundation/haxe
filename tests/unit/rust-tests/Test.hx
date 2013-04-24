class Test {
	public static function main() {
		var a = "Hellooo world!";
		Sys.println(a);
		for(i in 0...5)
			Sys.println(Std.string(i));
		Sys.println(a == null ? "Yup" : "Nope");
	}
}