package validation;

typedef ValidationReport = {
	var success:Bool;
	var summary:{
		var assertions:Int;
		var failures:Int;
	};
	var errors:Array<ValidationError>;
}