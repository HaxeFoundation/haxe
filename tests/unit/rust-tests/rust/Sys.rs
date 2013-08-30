mod lib;
pub struct Sys;
impl Sys {
	pub fn args() -> Option<~[Option<~str>]> {
		return os::args();
	}
}
impl lib::HxObject for ~Sys {
	fn toString(&self) -> Option<~str> {
		return Some(~"Sys: { }");
	}
}
