package mtwin.mail;

class Exception {
	var s : String;

	public function new(s){
		this.s = s;
	}

	public function toString(){
		return s;
	}
}
