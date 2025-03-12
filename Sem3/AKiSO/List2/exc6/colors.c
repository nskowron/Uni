#include <stdio.h>

int main(void)
{
	// 24-bit - supported only on modern terminals, but gives a bigger color choice
	for(int R = 0; R <= 255; ++R)
	{
		for(int G = 0; G <= 255; ++G)
		{
			for(int B = 0; B <= 255; ++B)
			{
				printf("\033[38;2;%d;%d;%dmHello World! - 24\n", R, G, B);
			}
		}
	}

	// 8-bit
	for(int color = 0; color <= 255; ++color)
	{
		printf("\033[38;5;%dmHello World! - 8\n", color);
	}

	// reset
	printf("\033[0m");
}
