
int f(int x)
{
	int y = 2*x+1;
	return y;
}

int m(int x)
{
	int res = 0;
	int y = x + 1;

	if(f(y)>0)
	{
		res = 10;
	}
	else
	{
		res = 11;
	}

	return res;
}
