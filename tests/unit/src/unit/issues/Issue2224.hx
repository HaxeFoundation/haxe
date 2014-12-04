package unit.issues;

class Issue2224 extends unit.Test {

	function test() {
		#if !js
		eq("opt=null", getEF1());
		eq("opt=null", getEF2());
		eq("opt=null", getInt());
		eq("opt=null", getClass());
		#end
	}

    static function getEF1( ?opt : Null<haxe.EnumFlags<MyEnum>> ){
        return( 'opt=$opt' );
    }

    static function getEF2( ?opt : haxe.EnumFlags<MyEnum> ){
        return( 'opt=$opt' );
    }

    static function getInt( ?opt : Int ){
        return( 'opt=$opt' );
    }

    static function getClass( ?opt : MyClass ){
        return( 'opt=$opt' );
    }

}