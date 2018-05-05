#include <stdlib.h>

int main(int argc, char *argv[])
{
	if (argc == 2) {
		return atoi(argv[1]);
	} else {
		return -1;
	}
}