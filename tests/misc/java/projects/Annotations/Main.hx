import haxe.io.BytesInput;
import sys.io.File;
import sys.io.FileInput;
import format.jvm.Data;

using StringTools;
using Lambda;

typedef Annotations = {
	var ?runtimeVisible:Array<Annotation>;
	var ?runtimeInvisible:Array<Annotation>;
	var ?runtimeVisibleParameter:Array<Array<Annotation>>;
	var ?runtimeInvisibleParameter:Array<Array<Annotation>>;
}

function getAnnotations(attributes:Array<Attribute>) {
	var annotations:Annotations = {};

	for (attribute in attributes) {
		switch (attribute) {
			case RuntimeVisibleAnnotations(a):
				annotations.runtimeVisible = a;
			case RuntimeInvisibleAnnotations(a):
				annotations.runtimeInvisible = a;
			case RuntimeVisibleParameterAnnotations(a):
				annotations.runtimeVisibleParameter = a;
			case RuntimeInvisibleParameterAnnotations(a):
				annotations.runtimeInvisibleParameter = a;
			case _:
		}
	}
	return annotations;
}

function hasAnnotation(annotations:Array<Annotation>, name:String) {
	return annotations.exists(ann -> ann.type == name);
}

function reportPresence(source:String, annotations:Array<Array<Annotation>>, name:String) {
	Sys.println('Presence of $name on $source');
	for (key => annotations in annotations) {
		Sys.println('  $key: ${hasAnnotation(annotations, name)}');
	}
}

function main() {
	var input = File.read("annotationLib.jar");
	var zip = new format.zip.Reader(input);
	var data = zip.read();
	for (entry in data) {
		if (!entry.fileName.endsWith("AnnotationLib.class")) {
			continue;
		}
		var input = new BytesInput(entry.data);
		var reader = new format.jvm.Reader(input);
		var jc = reader.read();
		var annotations = getAnnotations(jc.attributes);
		reportPresence(jc.thisClass, [annotations.runtimeVisible, annotations.runtimeInvisible], "Lhaxe/root/MyVisibleAnnotation;");
		reportPresence(jc.thisClass, [annotations.runtimeVisible, annotations.runtimeInvisible], "Lhaxe/root/MyInvisibleAnnotation;");

		for (method in jc.methods) {
			if (method.name != "test") {
				continue;
			}
			var annotations = getAnnotations(method.attributes);
			reportPresence(method.name, [annotations.runtimeVisible, annotations.runtimeInvisible], "Lhaxe/root/MyVisibleAnnotation;");
			reportPresence(method.name, [annotations.runtimeVisible, annotations.runtimeInvisible], "Lhaxe/root/MyInvisibleAnnotation;");

			for (i in 0...2) {
				var name = '${method.name} (arg $i)';
				reportPresence(name, [annotations.runtimeVisibleParameter[i], annotations.runtimeInvisibleParameter[i]], "Lhaxe/root/MyVisibleAnnotation;");
				reportPresence(name, [annotations.runtimeVisibleParameter[i], annotations.runtimeInvisibleParameter[i]], "Lhaxe/root/MyInvisibleAnnotation;");
			}
		}

		var ann = annotations.runtimeVisible.find(ann -> ann.type == "Lhaxe/root/MyVisibleArrayAnnotation;");
		Sys.println(ann.elementValuePairs[0].elementValue.value);

		var ann = annotations.runtimeVisible.find(ann -> ann.type == "Lhaxe/root/MyVisibleArrayArrayAnnotation;");
		Sys.println(ann.elementValuePairs[0].elementValue.value);
	}
}
