open Type

type continuation_api = {
	control : tclass_field;
	result : tclass_field;
	error : tclass_field;
	completion : tclass_field;
	context : tclass_field;
	state : tclass_field;
	recursing : tclass_field;
}

let create_continuation_api control result error completion context state recursing = {
	control;
	result;
	error;
	completion;
	context;
	state;
	recursing;
}