#import <stdio.h>

int abs(int x)
{
	if(x>0) return x;
	else return 0-x;
}

// Analyse mode
int g_ana_abs(int x,int y)
{
	// [y=Any,x=2],[y=Any,x=1]
	int e = y;

	// [e=Any,x=2],[e=Any,x=1]
	if(x>abs(y))
	{
		// [e = Any]
		e = e * 2;
	}

	// [e = Any]
	return e-1;
	// [erg = Any]
}

// Analyse mode
int g_ana(int x,int y)
{
	// [y=Any,x=2],[y=Any,x=1]
	int e = y;

	// [e=Any,x=2],[e=Any,x=1]
	if(x>1)
	{
		// [e = Any]
		e = e * 2;
	}

	// [e = Any]
	return e-1;
	// [erg = Any]
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
