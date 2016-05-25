
int f(int x,int m)
{
	// FREE VARIABLES: x:Any, m:Any
	int y =
		(m*
		x)+
		1;
	// m = (y-1)/x; oder
	// x = (y-1)/m
	return y;
}

int m(int x)
{
	int res = 0;
	int y = x + 1;

	if(f(y,2)>0)
	{
		int z = 10;
		res = f(z,y)+1;
	}
	else
	{
		res = 11;
	}

	return res;
}
