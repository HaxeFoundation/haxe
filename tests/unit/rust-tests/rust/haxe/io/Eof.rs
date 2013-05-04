extern mod std;
mod lib;
pub struct Eof
pub impl Eof {
	priv fn toString(&mut self) -> Option<~str> {
		return Some(~"Eof");
	}
}
impl lib::HxObject for haxe::io::Eof {
	
	pub fn toString(&self) -> Option<~str> {
		return Some(~"Eof")
	}
}
