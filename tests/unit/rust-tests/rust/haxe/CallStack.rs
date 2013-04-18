use rust::Lib;
mod HxObject;
mod StringBuf;
use haxe::StackItem;
mod Std;
pub struct CallStack;
pub impl CallStack {
	pub fn exceptionStack() -> Option<~[haxe::StackItem]> {
		return Some(~[]);
	}
	pub fn toString(stack: Option<~[haxe::StackItem]>) -> Option<@str> {
		let mut b: Option<@StringBuf> = StringBuf::new();
		{
			let mut _g: i32 = 0i32;
			while (_g < stack.len()) {
				let mut s: haxe::StackItem = stack[_g];
				{
					_g += 1;
					_g
				};
				{
					(rust::Lib::unwrap(b)).b += Some(@"\nCalled from ");
					(rust::Lib::unwrap(b)).b
				}
				(rust::Lib::unwrap(haxe::CallStack))::itemToString(b,s);
			}
		}
		return (rust::Lib::unwrap(b)).b;
	}
	priv fn itemToString(b: Option<@StringBuf>, s: haxe::StackItem) -> () {
		{
			match (rust::Lib::unwrap((s))) {
			0 =>{
				(rust::Lib::unwrap(b)).b += Some(@"a C function");
				(rust::Lib::unwrap(b)).b
			},
			1 =>
			var {
				{
					(rust::Lib::unwrap(b)).b += Some(@"module ");
					(rust::Lib::unwrap(b)).b
				}
				{
					(rust::Lib::unwrap(b)).b += (rust::Lib::unwrap(Std))::string(s_eModule_0);
					(rust::Lib::unwrap(b)).b
				}
			},
			2 =>
			var , , {
				if (s_eFilePos_0 != None) {
					(rust::Lib::unwrap(haxe::CallStack))::itemToString(b,s_eFilePos_0);
					{
						(rust::Lib::unwrap(b)).b += Some(@" (");
						(rust::Lib::unwrap(b)).b
					}
				}
				{
					(rust::Lib::unwrap(b)).b += (rust::Lib::unwrap(Std))::string(s_eFilePos_1);
					(rust::Lib::unwrap(b)).b
				}
				{
					(rust::Lib::unwrap(b)).b += Some(@" line ");
					(rust::Lib::unwrap(b)).b
				}
				{
					(rust::Lib::unwrap(b)).b += (rust::Lib::unwrap(Std))::string(s_eFilePos_2);
					(rust::Lib::unwrap(b)).b
				}
				if (s_eFilePos_0 != None) {
					{
						(rust::Lib::unwrap(b)).b += Some(@")");
						(rust::Lib::unwrap(b)).b
					}
				}
			},
			3 =>
			var , {
				{
					(rust::Lib::unwrap(b)).b += (rust::Lib::unwrap(Std))::string(s_eMethod_0);
					(rust::Lib::unwrap(b)).b
				}
				{
					(rust::Lib::unwrap(b)).b += Some(@".");
					(rust::Lib::unwrap(b)).b
				}
				{
					(rust::Lib::unwrap(b)).b += (rust::Lib::unwrap(Std))::string(s_eMethod_1);
					(rust::Lib::unwrap(b)).b
				}
			},
			4 =>
			var {
				{
					(rust::Lib::unwrap(b)).b += Some(@"local function #");
					(rust::Lib::unwrap(b)).b
				}
				{
					(rust::Lib::unwrap(b)).b += (rust::Lib::unwrap(Std))::string(s_eLambda_0);
					(rust::Lib::unwrap(b)).b
				}
			},
			}
		}
	}
}
impl HxObject for haxe::CallStack {
}
