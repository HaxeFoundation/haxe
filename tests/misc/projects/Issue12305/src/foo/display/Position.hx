package foo.display;

/**
	Position in a text document expressed as 1-based line and character offset.
**/
typedef Position = {
	/**
		Line position in a document (1-based).
	**/
	var line:Int;

	/**
		Character offset on a line in a document (1-based).
	**/
	var character:Int;
}

/**
	A range in a text document expressed as (1-based) start and end positions.
**/
typedef Range = {
	/**
		The range's start position
	**/
	var start:Position;

	/**
		The range's end position
	**/
	var end:Position;
}

/**
	Represents a location inside a resource, such as a line inside a text file.
**/
typedef Location = {
	var file:FsPath;
	var range:Range;
}
