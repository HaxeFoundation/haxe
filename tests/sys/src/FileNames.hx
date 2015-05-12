class FileNames {
	static public var names(default, never) = [
		"ok",

		// a space inside
		"two words",

		// Chinese, Japanese
		// "中文，にほんご",

		// "aaa...a" that has 255 characters
		// 255 bytes is the max filename length according to http://en.wikipedia.org/wiki/Comparison_of_file_systems
		// [for (i in 0...255) "a"].join("")
	];
}