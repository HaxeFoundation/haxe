package cpp;

// This is just a helper class.  You do not actually need to inherit from this to use
//  NativeGc.addFinalizable(this,inPin), you just need a function called "finalize"
class Finalizable
{
   public function new(inPin = false)
   {
      NativeGc.addFinalizable(this,inPin);
   }

   public function finalize():Void { }
}

