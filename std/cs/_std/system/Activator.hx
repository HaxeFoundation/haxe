package system;

@:native('System.Activator') extern class Activator 
{
	static function CreateInstance(t:system.Type):Dynamic;
}