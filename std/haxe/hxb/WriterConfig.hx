package haxe.hxb;

typedef WriterTargetConfig = {
	/**
		If `false`, this target is ignored by the writer.
	**/
	var ?generate:Null<Bool>;

	/**
		Dot paths of modules or packages to be exluded from the archive.
	**/
	var ?exclude:Null<Array<String>>;

	/**
		Dot paths of modules or packages to be included in the archive. This takes priority
		over exclude. By default, all modules that aren't explicitly excluded are
		included.
	**/
	var ?include:Null<Array<String>>;

	/**
		The hxb version to target. By default, the version of the Haxe compiler itself
		is targeted. See https://github.com/HaxeFoundation/haxe/issues/11505
	**/
	var ?hxbVersion:Null<Int>;

	/**
		If false, no documentation
	**/
	var ?generateDocumentation:Null<Bool>;
}

typedef WriterConfig = {
	/**
		The file path for the archive. Occurrences of `$target` are replaced
		by the name of the current target (js, hl, etc.).
	**/
	var archivePath:String;

	/**
		The configuration for the current target context. If it is `null`, all data
		for the target context is generated.
	**/
	var ?targetConfig:Null<WriterTargetConfig>;

	/**
		The configuration for the macro context. If it is `null`, all data for the
		macro context is generated.
	**/
	var ?macroConfig:Null<WriterTargetConfig>;
}
