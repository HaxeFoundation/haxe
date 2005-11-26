extern class Date
{
	function new(year : Int, month : Int, date : Int, hour : Int, min : Int, sec : Int, ms : Int) : Void;

	function getTime() : Int;
	function setTime(value : Int) : Void;
	function toString():String;

	// need to think about the API

}


