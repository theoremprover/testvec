#import <stdio.h>

int abs(int x)
{
	if(x>0) return x;
	else return 0-x;
}

// Reverse mode
int g_rev(int x,int y)
{
	// [y=3,x=2],[y=6,x=1]
	int e = y;
	// [e=3,x=2],[e=6,x=1]
	if(x>1)
	{
		// [e = 3]
		e = e * 2;
	}

	// [e = 6]
	return e-1;
	// [erg = 5]
}

int f(int x,int y)
{
	int erg = 0;

	int f = 1;
	if(x<0)
	{
		f = -1;
	}

	for(int i=1;i<=abs(x);i++)
	{
		erg = erg + f*y;
	}

	return erg;
}

int main()
{
	printf("%i",f(-2,-3));
}

int h(int x)
{
// [ x=6 ]
	return x-1;
// [ return=5 ]
}

int h2(int x,int y)
{
// [ x=6 ]
	return x-2*y;
// [ return=5 ]
}
/*
5 = x-2*y
*/
