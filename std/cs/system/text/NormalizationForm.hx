package cs.system.text;

/** Defines the type of normalization to perform. */
@:native("System.Text.NormalizationForm")
extern enum abstract NormalizationForm(Int) {
	var FormC = 1;
	var FormD = 2;
	var FormKC = 5;
	var FormKD = 6;
}
