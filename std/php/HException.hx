package php;

extern class HException extends Exception {
  public var e : Dynamic;
  public var p : haxe.PosInfos;
  public function new(e : Dynamic, ?message : String, ?code : Int, ?p : haxe.PosInfos) : Void;
  public function setLine(l:Int) : Void;
  public function setFile(f:String) : Void;
}