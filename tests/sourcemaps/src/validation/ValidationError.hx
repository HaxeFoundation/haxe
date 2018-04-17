package validation;

typedef ValidationError = {
	/** Code expected to be generated from the haxe code */
	var expected:String;
	/** Position of the haxe code, which is expected to be generated as `expected` code */
	var pos:{
		/** .hx File */
		var file:String;
		/** 1-base line number in .hx file */
		var line:Int;
		/** 1-base column number in .hx file */
		var column:Int;
	}
}