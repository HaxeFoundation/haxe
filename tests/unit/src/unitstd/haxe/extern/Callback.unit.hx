var cb:haxe.extern.Callback<String->String->Int> = null;
TestType.typeError(cb = function(s1:String, s2:String) return 0) == false;
TestType.typeError(cb = function(s1:String) return 0) == false;
TestType.typeError(cb = function() return 0) == false;

TestType.typeError(cb = function(s1:String, s2:String, s3:Int) return 0) == true;
TestType.typeError(cb = function(s1:String, s2:Int) return 0) == true;
TestType.typeError(cb = function(s1:String, s2:Int){}) == true;

var cb:haxe.extern.Callback<Void->Int> = null;
TestType.typeError(cb = function() return 0) == false;

var cb:haxe.extern.Callback<Void->Void> = null;
TestType.typeError(cb = function(){}) == false;

var cb:haxe.extern.Callback<?String->?String->Int> = null;
TestType.typeError(cb = function(?s1:String, ?s2:String) return 0) == false;
TestType.typeError(cb = function(?s1:String) return 0) == false;
TestType.typeError(cb = function() return 0) == false;