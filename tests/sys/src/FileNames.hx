class FileNames {
	static public var names(default, never) = [
		"ok",
		"ok2",
		"ok3",
		"ok4",

		// a space inside
		"two words",

		// some valid seperators
		"two-words",
		"two_words",
		"two,words",
		"two.words",
		"two;words",
		"(two words)",
		"[two words]",

		// "aaa...a"
		[for (i in 0...100) "a"].join(""),
	]
	// long file name
	.concat(switch (Sys.systemName()) {
		case "Windows":
			// http://stackoverflow.com/a/265782/267998
			[];
		case _:
		[
			// 255 bytes is the max filename length according to http://en.wikipedia.org/wiki/Comparison_of_file_systems
			#if !(python || neko || cpp || java || cs)
			[for (i in 0...255) "a"].join(""),
			#end
		];
	});
}
