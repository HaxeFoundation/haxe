package cpp;

extern class ArrayBase
{
   // Length is number of elements
   public var length(default,null):Int;
   public function getElementSize():Int;
   public function getByteCount():Int;
   public function getBase():RawPointer<Char>;
}

