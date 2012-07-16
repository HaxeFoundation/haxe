package cs.system;

@:native('System.Activator') extern class Activator 
{
	static function CreateInstance(t:cs.system.Type):Dynamic;
}