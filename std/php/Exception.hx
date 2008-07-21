package php;

extern class Exception {
  public function new(?message : String, ?code : Int) : Void;
  
  private var message : String;
  private var code : Int;
  private var file : String;
  private var line : Int;
  
  public function getMessage() : String;       // message of the exception 
  public function getCode() : Int;             // code of the exception
  public function getFile() : String;          // source filename
  public function getLine() : Int;             // source line
  public function getTrace() : Array<String>;  // an array of the backtrace()
  public function getTraceAsString() : String; // formated string of trace
  
  public function __toString() : String;       // formated string for display
}