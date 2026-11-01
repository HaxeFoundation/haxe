function main() {
	#if display #end
	#if (display) #end
	#if foo #elseif display #end
	#if foo #elseif display == 42 #end
	#if !display #end
	#if (!display) #end
	#if (foo || display) #end
	#if (foo || display > 0) #end
	#if (foo && !display) #end
	#if (foo || (!bar && display)) #end
	#if (foo || (!bar && display == "nope")) #end
}
