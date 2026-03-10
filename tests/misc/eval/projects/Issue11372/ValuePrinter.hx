overload extern inline function printValue(input:MyEnumAbstract) {
	var value:String = input;
	printValue(value);
}

overload extern inline function printValue(input:Int) {
	// Do something.
}

overload extern inline function printValue(input:String) {
	// Do something.
}

overload extern inline function printValue(input:Bool) {
	// Do something.
}