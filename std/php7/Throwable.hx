package php;

/**
    @see http://php.net/manual/en/class.throwable.php
**/
@:native('Throwable')
extern interface Throwable {
    function getPrevious() : Throwable;   // Returns previous Throwable
    function getMessage() : String;       // message of the exception
    function getCode() : Int;             // code of the exception
    function getFile() : String;          // source filename
    function getLine() : Int;             // source line
    function getTrace() : NativeIndexedArray<NativeAssocArray<Dynamic>>;  // an array of the backtrace
    function getTraceAsString() : String; // formated string of trace
	@:phpMagic function __toString() : String;       // formated string for display
}