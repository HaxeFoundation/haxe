package unit.issues;

class Issue6121 extends unit.Test {

   public function add(t:Issue6121) : Issue6121 return this;

   public function start(run:()->Issue6121) : Issue6121 return null;

   static public function setCallback( cb:()->Issue6121 ) : Issue6121 return null;

   function callFunction() : Int
   {
      setCallback(function () return add(setCallback(function() return start(function() return null))));
      return 1;
   }

   function callBind() : Int
   {
      setCallback(function () return add(setCallback(start.bind(function() return null))));
      return 2;
   }


	function test() {
		eq(callFunction(),1);
		eq(callBind(),2);
	}
}

