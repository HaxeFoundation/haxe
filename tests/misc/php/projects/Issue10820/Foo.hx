class PAckFoo extends pack.Foo {}

class Foo{
	public var bar	: PAckFoo;
	public function new(){
		bar = new PAckFoo();
	}
}