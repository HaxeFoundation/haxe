package user1;

import consts.Consts;
import builder.Builder;

class User1 implements IBuilder {
	var STR:String = Consts.str;
	var STR2:String;
	function f(){
		STR2 = Consts.str;
		var STR3 = Consts.str;
	}
}