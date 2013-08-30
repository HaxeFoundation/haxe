mod lib;
pub struct Eof;
impl Eof {
	priv fn toString(&mut self) -> Option<~str> {
		return Some(~"Eof");
	}
}
impl lib::HxObject for ~haxe::io::Eof {
	fn toString(&self) -> Option<~str> {
		return Some(~"Eof: { }");
	}
}
