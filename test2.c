#import <stdio.h>

int abs(int x)
{
	if(x>=0) return x;
	else return -x;
}

int f(int x,int y)
{
	int erg = 0;

	int f = 1;
	if(x<0)
	{
		f = -1;
	}
/*
	for(int i=1;i<=abs(x);i++)
	{
		erg = erg + f*y;
	}
*/

	return f /*erg*/;
}

int main()
{
	printf("%i",f(-2,-3));
}
