// Part 1 about the 3n+1 conceture
//=================================


//(1) Complete the collatz function below. It should
//    recursively calculate the number of steps needed 
//    until the collatz series reaches the number 1.
//    If needed you can use an auxilary function that
//    performs the recursion. The function should expect
//    arguments in the range of 1 to 1 Million.

def collatz(n: Long): Int = collatzRec(n, 0)

def collatzRec(n: Long, count: Int): Int = {
	if (n == 1) count + 1
	else if((n % 2) == 0) collatzRec(n/2, count + 1)
	else collatzRec(3*n + 1, count + 1)
}

//(2)  Complete the collatz bound function below. It should
//     calculuate how many steps are needed for each number 
//     from 1 upto a bound and returns the maximum number of
//     steps and the corresponding number that needs that many 
//     steps. You should expect bounds in the range of 1
//     upto 1 million. The first component of the pair is
//     the maximum number of steps and the second is the 
//     corresponding number.

def collatz_max(bnd: Int): (Int, Int) = {
	var maxSteps = 0
	var value = 0;
	for(i <- 1 to bnd) 
		if (collatz(i) > maxSteps) {
			maxSteps = collatz(i)
			value = i 
		}
	(maxSteps, value)
}