/* Sample ECMAscript code for `test-language'.  */

function fib (n)
{
    if (n <= 1)
	return n;
    else
	return fib (n - 1) + fib (n - 2);
}

if (fib (7) != 13)
    error ("Something's wrong!");
