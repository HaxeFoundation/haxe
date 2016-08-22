package php7;

/**
    @see http://php.net/manual/en/class.throwable.php
**/
@:native('Throwable')
extern interface Throwable {
    public function getPrevious() : Throwable;   // Returns previous Throwable
    public function getMessage() : String;       // message of the exception
    public function getCode() : Int;             // code of the exception
    public function getFile() : String;          // source filename
    public function getLine() : Int;             // source line
    public function getTrace() : Array<String>;  // an array of the backtrace()
    public function getTraceAsString() : String; // formated string of trace
	public function __toString() : String;       // formated string for display
}