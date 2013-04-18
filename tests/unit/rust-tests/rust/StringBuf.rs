use rust::Lib;
mod HxObject;
pub struct StringBuf {
	b: Option<@str>
}
pub impl StringBuf {
	pub fn new() -> Option<@StringBuf> {
		let mut self = StringBuf {b: None}
		{
			(rust::Lib::unwrap(self)).b = Some(@"");
			(rust::Lib::unwrap(self)).b
		}
		return Some(@self);
	}
}
impl HxObject for StringBuf {
}
